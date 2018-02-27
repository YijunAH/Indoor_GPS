setwd('~/Documents/DataScience/R/CaseStudyR/Chapter1PredictingLocationIndoor/data/')

library(ggplot2)
library(dplyr)
library(tidyr)

txt=readLines('offline.final.trace.txt')

process_each_line<-function(x){
  yi<-strsplit(x,'[;=,]')[[1]]
  if (length(yi) == 10)
    return (NULL)
  right<-matrix(yi[-c(1:10)],,4,byrow = TRUE)
  left<-matrix(yi[c(2,4,6,7,8,10)],nrow=nrow(right),6,byrow=TRUE)
  cbind(left,right)
}

tmp<-lapply(txt, process_each_line)

offline<-as.data.frame(do.call('rbind',tmp),stringsAsFactors=FALSE)

colnames(offline)<-c('rawtime','scanMac','X','Y','Z','orientation','mac','signal','channel','type')

numberVals<-c('rawtime','X','Y','Z','orientation','signal','type')

offline[numberVals]<-lapply(offline[numberVals],as.character)
offline[numberVals]<-lapply(offline[numberVals],as.numeric)

offline<-offline[offline$type=='3',]
offline = offline[,"type" != names(offline) ]

numberVals<-c('rawtime','X','Y','Z','orientation','signal')

offline$Z<-NULL
offline$scanMac<-NULL

roundOrientation=function(angles){
  refs=seq(0, by=45, length=9)
  x=sapply(angles, function(a) which.min(abs(a-refs)))
  c(refs[1:8],0)[x]
}

offline$angle=roundOrientation(offline$orientation)

subMacs<-names(sort(table(offline$mac),decreasing=TRUE))[1:7]
offline=offline[offline$mac %in% subMacs, ]

macChannel<-with(offline,table(mac,channel))
apply(macChannel,1,function (x) sum(x>0))

offline$channel<-NULL

offline$XY = paste(offline$X, offline$Y, sep = "-")

offSignalSummary<-offline %>% group_by(XY, angle, mac) %>%
  summarise(median=median(signal),
            average=mean(signal),
            sd=sd(signal),
            IQR=IQR(signal),
            ncounts=n()) %>% ungroup()

offSignalSummary$XY2<-offSignalSummary$XY

offSignalSummary<- offSignalSummary%>%
  separate(XY2, c("X", "Y"), "-")

offSignalSummary$X<-as.numeric(offSignalSummary$X)
offSignalSummary$Y<-as.numeric(offSignalSummary$Y)

AP = matrix( c( 7.5, 6.3, 2.5, -.8, 1, 14, 33.5, 9.3,  33.5, 2.8),
             ncol = 2, byrow = TRUE, dimnames = list(subMacs[ -c(2,4) ], c("X", "Y") ))

APdf<-as.data.frame(AP)

APdf<-setNames(cbind(rownames(APdf), APdf, row.names = NULL), c("mac", "X", "Y"))

offSignalSummary<-subset(offSignalSummary, mac != subMacs[2])
offSignalSummary<-subset(offSignalSummary, mac != subMacs[4])

LocationDiff<-offSignalSummary[, c('X','Y')] - AP[offSignalSummary$mac,c('X','Y')]

offSignalSummary$dist<-sqrt(LocationDiff[ , 1]^2 + LocationDiff[ , 2]^2)

readData = 
  function(filename = 'Data/offline.final.trace.txt', 
           subMacs = c("00:0f:a3:39:e1:c0", "00:0f:a3:39:dd:cd", "00:14:bf:b1:97:8a",
                       "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:90", "00:14:bf:b1:97:8d",
                       "00:14:bf:b1:97:81"))
  {
    txt = readLines(filename)
    lines = txt[ substr(txt, 1, 1) != "#" ]
    tmp = lapply(lines, process_each_line)
    offline = as.data.frame(do.call("rbind", tmp), 
                            stringsAsFactors= FALSE) 
    
    names(offline) = c("time", "scanMac", "X", "Y", "Z", "orientation", 
                       "mac", "signal", "channel", "type")
    
    offline = offline[ offline$type == "3", ]
    
    dropVars = c("scanMac", "Z", "channel", "type")
    offline = offline[ , !( names(offline) %in% dropVars ) ]
    
    offline = offline[ offline$mac %in% subMacs, ]
    
    numVars = c("time", "X", "Y", "orientation", "signal")
    offline[ numVars ] = lapply(offline[ numVars ], as.numeric)
    
    offline$angle = roundOrientation(offline$orientation)
    return(offline)
  }

macs = unique(offSignalSummary$mac)
online = readData("online.final.trace.txt", subMacs = macs)
online$XY = paste(online$X, online$Y, sep = "-")

onSignalSummary<-
  online %>% group_by(XY, mac) %>%
  summarise(mean = mean(signal) ,
            angle = mean(angle),
            orientation = min(orientation)) %>%
  ungroup()

onSignalSummary<-spread(onSignalSummary, mac, mean)

onSignalSummary$XY2<-onSignalSummary$XY

onSignalSummary<-onSignalSummary%>%
  separate(XY2, c("X", "Y"), "-")

onSignalSummary$X<-as.numeric(onSignalSummary$X)
onSignalSummary$Y<-as.numeric(onSignalSummary$Y)

reshapeDF = function(data, varSignal = "average")
{
  subsetDF<-data %>% group_by(XY, mac) %>%
    summarise(X=mean(X), Y=mean(Y), average=mean(average)) %>%
    ungroup()
  subsetWide<-spread(subsetDF, mac, average)
  return(subsetWide)
}

findNearestNeighbor = function(newSignal, trainData) {
  difference = apply(trainData[ , 4:8], 1, 
                     function(x) x - newSignal)
  dists = apply(difference, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainData[closest, 1:3 ])
}

selectTrain = function(angleNewObs, signals = NULL, m = 1){

  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles = sort(angles) 
  
  offSubset =  offSignalSummary[ offSignalSummary$angle %in% angles, ]
  reshapeDF(offSubset, varSignal = "average")
}

predXY = function(newSignals, newAngles, trainData, numAngles = 1, k){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles$orientation[i], trainData, m = numAngles)
    closeXY[[i]] = 
      findNearestNeighbor(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  
  estXY = lapply(closeXY, function(x) sapply(x[ , 2:3], 
                                             function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}

calcError = function(estXY, actualXY) 
  sum( rowSums( (estXY - actualXY)^2) )

estXYk5 = predXY(newSignals = onSignalSummary[, 4:8], 
                 newAngles = onSignalSummary[, 3], 
                 offSignalSummary,
                 numAngles = 3, k = 5)
dfestXYk5<-as.data.frame(estXYk5)

estXYk14 = predXY(newSignals = onSignalSummary[, 4:8], 
                  newAngles = onSignalSummary[, 3], 
                  offSignalSummary,
                  14, n = 5)
dfestXYk14<-as.data.frame(estXYk14)

actualXY = onSignalSummary[ , c("X", "Y")]


OptimizeK = function(k){
  result=NULL
  for (i in 1:k) {
    estXY = predXY(newSignals = onSignalSummary[, 4:8], 
                   newAngles = onSignalSummary[, 3], 
                   offSignalSummary,
                   numAngles = 3, k = i)
    Error=sapply(list(estXY), calcError, actualXY)
    result=rbind(result,data.frame(i, Error))
  }
  return(result)
}

findoptK50<-OptimizeK(50)

ggplot(aes(x=i, y=Error), data=findoptK50)+ geom_point()+geom_line()+ ggtitle('How many nearby neighbour to include in the prediction?')+labs(x='Numbers of Neighbour', y='Sum of Square Errors')

OptimizeAngles = function(numAngles){
  result=NULL
  for (i in 1:numAngles) {
    estXY = predXY(newSignals = onSignalSummary[, 4:8], 
                   newAngles = onSignalSummary[, 3], 
                   offSignalSummary,
                   numAngles = i, 14)
    Error=sapply(list(estXY), calcError, actualXY)
    result=rbind(result,data.frame(i, Error))
  }
  return(result)
}

findoptAngles<-OptimizeAngles(8)

ggplot(aes(x=i, y=Error), data=findoptAngles)+ geom_point()+geom_line()+ggtitle('How many angles to include in the prediction?')+labs(x='Numbers of Angle', y='Sum of Square Errors')



dfestXYk14$ID <- seq.int(nrow(dfestXYk14))
str(dfestXYk5)

actualposition<-data.frame(onSignalSummary$X, onSignalSummary$Y)
colnames(actualposition)<-c('X','Y')
actualposition$ID <- seq.int(nrow(actualposition))

positioncompare<-rbind(actualposition, dfestXYk14)

ggplot()+geom_point(aes(x=X, y=Y, group=ID), data=positioncompare, color='red')+geom_line(aes(x=X, y=Y, group=ID), data=positioncompare, color='black', linetype = "dashed")+geom_point(aes(x=X, y=Y), data=positioncompare[1:60,], color='black')+geom_point(aes(x=X, y=Y), shape=21, data=offSignalSummary,color='grey')+ggtitle('Floor Map with Predicted(Red) and Actual(Black) Locations')
