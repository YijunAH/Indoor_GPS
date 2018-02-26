setwd('~/Documents/DataScience/R/CaseStudyR/Chapter1PredictingLocationIndoor/data/')
txt=readLines('offline.final.trace.txt')

### 1. Data cleaning

# length(txt)
# 166locations*8angles*110recordings+5312 comments lines

# txt[1]
# txt[2]
# txt[3]
# first three are some background information

# txt[4]
# txt[4:10]
# look into a few examples

# strsplit(txt[4],';')
# strsplit(txt[4],';')[[1]]
# strsplit(txt[4],'[;=,]')[[1]]
# the first ten elements gave the basic information of the hand hold device (t, id, position, degree) the rest of the elements provided information of the MAC address, channel, type, corresponding signal strength

process_each_line<-function(x){
  yi<-strsplit(x,'[;=,]')[[1]]
  if (length(yi) == 10)
    return (NULL)
  right<-matrix(yi[-c(1:10)],,4,byrow = TRUE)
  left<-matrix(yi[c(2,4,6,7,8,10)],nrow=nrow(right),6,byrow=TRUE)
  cbind(left,right)
}

# process each line in the offline file

# tmp<-lapply(txt[4:10], process_each_line)

# sapply(tmp,nrow)
# options(error = recover, warn = 1)

tmp<-lapply(txt, process_each_line)
# use the process_each_line function on the entire txt file
# this process can take a few seconds to complete

offline<-as.data.frame(do.call('rbind',tmp),stringsAsFactors=FALSE)
# create a dataframe using the tmp list we created before
# It contains over 1000000 rows and 10 columns


### 2. Formating the dataframe for further analysis

colnames(offline)<-c('rawtime','scanMac','X','Y','Z','orientation','mac','signal','channel','type')

# View(offline)

numberVals<-c('rawtime','X','Y','Z','orientation','signal','type')

# table(offline$mac)
# unique(offline$mac)
# 21 mac in total!!! But the background information of this dataset just mentioned 6 mac address. Why is that? And also some mac have a lot of readings while some only have 1 or only a few hundreds of readings. Clearly there are some mac address that we do not want to use.

offline[numberVals]<-lapply(offline[numberVals],as.character)
offline[numberVals]<-lapply(offline[numberVals],as.numeric)

# unique(offline$type)
# two 'type' present in the dataset: 1 and 3
# We want to eliminate the rows which type==1 because we onlye want to use the signal measured to the fixed access points

offline<-offline[offline$type=='3',]
offline = offline[,"type" != names(offline) ]
# View(offline)
# removed over 100,000 rows from offline dataset and now there is 9 columns present

# table(offline$mac)
# 12 mac address now

# dim(offline)

numberVals<-c('rawtime','X','Y','Z','orientation','signal')
#update numberVals here: because 'type' was removed from the dataframe

# summary(offline[,numberVals])
# rawtime seems odd here (we might be able to delete rawtime column) and Z(height) is 0 in all the rows (measured at the same floor, no height change)
# we can remove column which stores all Z information from our analysis
# value of signal is negative

offline$Z<-NULL

# str(offline)
# down to 8 columns now

# summary(sapply(offline[,c('scanMac','mac','channel')],as.factor))
# only one scanMac: we can eliminate this from our analysis too

offline$scanMac<-NULL
# down to 7 columns now

### 3. Orientation variable

# unique(offline$orientation)
# sort(unique(offline$orientation))
# The orientations are not exact! over 200 values, not just 9 values as we might expected (0, 45, 90, 135, 180, 225, 270, 315, 360)
# but all the values are around (0, 45, 90, 135, 180, 225, 270, 315, 360)
# It may be useful in our analysis to work with values corresponding to the 8 equi-spaced angles.

library(ggplot2)
# ggplot(offline, aes(orientation)) + stat_ecdf(geom = "point")
# ggplot(offline, aes(orientation)) + stat_ecdf(geom = "step")

# ggplot(offline, aes(orientation)) + stat_ecdf(geom = "step")+labs(title="Empirical Cumulative Density Function", y = "F(height)", x="Orientation")+theme_classic()

# ggsave('orientation_ecdf.pdf')
# save plot as pdf

roundOrientation=function(angles){
  refs=seq(0, by=45, length=9)
  x=sapply(angles, function(a) which.min(abs(a-refs)))
  c(refs[1:8],0)[x]
}
# roundOrientation function: 
#   input: column in a dataframe
#   output: a new column to a dataframe
# angles refers to a list of angles. That is why sapply is used here
# roundOrientation convert angles to the nearest angle standard (0, 45, 90, 135, 180, 225, 270, 315, 0)

offline$angle=roundOrientation(offline$orientation)
# unique(offline$angle)
# create a new column called 'angle'. We keep the orientation in case we need to use the orientation parameter later.

# ggplot(offline, aes(factor(x=angle), y=orientation)) + geom_boxplot()
# ggsave('angleVsorientation.pdf')
# the weired looking 'outlier' in the top left corner is caused by the fact that we map some of the angles near 360 to 0.
# remember to factor the angle variable because now it is a numeric variable


### 4. Mac variable

# table(offline$mac)
# table(offline$channel)
# 12 mac and 8 channels. But only 6 access points according to the background information!!! Need look into both mac and channel
# both mac and channel have some items with usual low counts (one mac usually should have 110*8*166= 146080 counts)
# MAC address look up and search: http://coffer.com/mac_find/

# sort(table(offline$mac),decreasing=TRUE)
# the last three mac should be eliminated from the analysis due to low counts

subMacs<-names(sort(table(offline$mac),decreasing=TRUE))[1:7]
offline=offline[offline$mac %in% subMacs, ]

macChannel<-with(offline,table(mac,channel))
apply(macChannel,1,function (x) sum(x>0))
# one to one correspondence between mac and channel. We can eliminate channel from offline

offline$channel<-NULL
# dim(offline)
# 7 columnes now

### 5. Position(X and Y) variable

library(dplyr)

# offlineXY<-offline %>% group_by(X, Y) %>%
#  summarise(counts=n()) %>%
#  ungroup()

# str(offlineXY)

# range(offlineXY$counts)
# 5309-5774: have similar counts throughout the dataset

# ggplot(aes(x=X, y=Y), data=offlineXY)+xlab('X (m)')+ylab('Y (m)')+ggtitle('Signal Counts Detected at Each Location')+geom_text(aes(label=offlineXY$counts),angle=45)+geom_point(color='Red')

# ggsave('CountsofSignalsatEachLoc.pdf',width = 12, height = 8)
# ggsave('CountsofSignalsatEachLoc.png',width = 10, height = 6)


### 6. Signal strength variable
# looked at two subsets: one @ X==2&Y==12, one @ X==24&Y==4

# offlineX2Y12<-subset(offline, X==2&Y==12)
# ggplot(aes(x=factor(angle), y=signal), data=offlineX2Y12)+facet_grid(~mac)+geom_boxplot()+ggtitle('Signal Strength by angle&mac @ X=2&Y=12')
# ggsave('SingalStrengthX2Y12.pdf')

# boxplot at one postion: X==2&Y==12, 7 plots in total
# remember to treat angle as factor, otherwise the system will treat it as numeric value

# offlineX24Y4<-subset(offline, X==24&Y==4)

# ggplot(aes(x=factor(angle), y=signal), data=offlineX24Y4)+facet_grid(~mac)+geom_boxplot()+ggtitle('Signal Strength by angle&mac @ X=24&Y=4')
# ggsave('SingalStrengthX24Y4.pdf')

# library(gridExtra)

# p1<-ggplot(aes(x=factor(angle), y=signal), data=offlineX2Y12)+facet_grid(~mac)+geom_boxplot()+ggtitle('Signal Strength by angle&mac @ X=2&Y=12')

# p2<-ggplot(aes(x=factor(angle), y=signal), data=offlineX24Y4)+facet_grid(~mac)+geom_boxplot()+ggtitle('Signal Strength by angle&mac @ X=24&Y=4')

# SignalStrengthatDiffLoc<-grid.arrange(p1, p2, ncol=1)

# ggsave('SignalStrengthatDiffLoc.pdf', SignalStrengthatDiffLoc, width = 20, height = 12)

# looking at two different locations we know that different mac address have complete different effect towards signal strength:
# @ X==2&Y==12: 8a, 90, c0 have strong signal while 8d, c6 and 81 have weak signal
# @ X==24&Y==4: 8d, 81, c0, c6 have strong signal while 8d, 90 has weak signal
# In both cases, dd:cd have weak signals.
# Looking at Y scale, in general, X=2&Y=12 has stronger signal comparing with X=24&Y=4
# signal strength on one location depends on different mac address: maybe the closer to the mac, the stronger the signal

# Using GridExtra require slight different syntax for ggsave! Be careful, it might just save the last plots

# summary(offline$signal)
# range(offline$signal)
# range from -98 to -25. Signal are in negative values

# ggplot(aes(x=signal), data=offlineX24Y4)+geom_density()+facet_grid(factor(angle)~mac)+ggtitle('Signal Distribution by angle&mac @ X=24&Y=4')+ylim(c(0,0.5))

# ggsave('SignalDistbyAngleMac.pdf', width= 8, height= 12)

# densityplot at one position: X==24&Y==4
# not all of them are normally distributed, some have bimodal distribution
# Does the angle matters? Yes. Especially when you compare 0 and 180, not much difference if you compare neighboring angles
# mac plays important role: cd and 90 have weak signals while c0, 81, 8d have strong signal

offline$XY = paste(offline$X, offline$Y, sep = "-")
# add one new column to offline dataframe

offSignalSummary<-offline %>% group_by(XY, angle, mac) %>%
  summarise(median=median(signal),
            average=mean(signal),
            sd=sd(signal),
            IQR=IQR(signal),
            ncounts=n()) %>% ungroup()

offSignalSummary$XY2<-offSignalSummary$XY

library(tidyr)

offSignalSummary<- offSignalSummary%>%
  separate(XY2, c("X", "Y"), "-")

offSignalSummary$X<-as.numeric(offSignalSummary$X)
offSignalSummary$Y<-as.numeric(offSignalSummary$Y)

# str(offSignalSummary)
# summary(offSignalSummary)
# Be aware: XY is charater, but X and Y are num now
# We organized/formated the offline dataset into a summary type of dataset for further investigation.

# breaks = seq(-90, -25, by = 5)
# Because average range from -89 to -28

# ggplot(aes(x=cut(average, breaks=breaks), y= sd),data=offSignalSummary)+geom_boxplot() +ggtitle('SD of Signal Strength by Mean Signal Strength')+xlab('Mean Signal Strength')+ylab('SD Signal Strength')
# ggsave('SdVsMeanSignal.pdf', width= 8, height= 6)

# weaker signal has relative smaller sd

# ggplot(aes(x=cut(median, breaks=breaks), y= sd),data=offSignalSummary)+geom_boxplot() +ggtitle('SD of Signal Strength by Median Signal Strength')+xlab('Median Signal Strength')+ylab('SD Signal Strength')
# ggsave('SdVsMediamSignal.pdf', width= 8, height= 6)

# p3<-ggplot(aes(x=cut(average, breaks=breaks), y= sd),data=offSignalSummary)+geom_boxplot() +ggtitle('SD of Signal Strength by Mean Signal Strength')+xlab('Mean Signal Strength')+ylab('SD Signal Strength')

# p4<-ggplot(aes(x=cut(median, breaks=breaks), y= sd),data=offSignalSummary)+geom_boxplot() +ggtitle('SD of Signal Strength by Median Signal Strength')+xlab('Median Signal Strength')+ylab('SD Signal Strength')
# SDVsAveMed<-grid.arrange(p3, p4, ncol=1)

# ggsave('SDVsAveMed.pdf', SDVsAveMed, width= 8, height= 12)

# cor.test(offSignalSummary$sd,offSignalSummary$median)
# cor.test(offSignalSummary$sd,offSignalSummary$average)
# cor.test(offSignalSummary$angle,offSignalSummary$average)


### 7. Relationship between signal and distance (Heatmap ect.)

# oneLocAngle1=subset(offSignalSummary, mac==subMacs[1] & angle ==0)

# oneLocAngle1<-oneLocAngle1%>%
#   separate(XY, c("X", "Y"), "-")

# summary(oneLocAngle1)
# Beware: X&Y are charater now

# oneLocAngle1$X<-as.numeric(oneLocAngle1$X)
# oneLocAngle1$Y<-as.numeric(oneLocAngle1$Y)
# summary(oneLocAngle1)

# ggplot(aes(x=oneLocAngle1$X, y=oneLocAngle1$Y), data= oneLocAngle1)+geom_point(color='Red')+geom_tile(aes(fill=average))

# ggplot(aes(x=oneLocAngle1$X, y=oneLocAngle1$Y), data= oneLocAngle1)+geom_point(color='Red')+geom_contour(aes(z=average))

# predictSurface

# library(spam)
# library(maps)
# library(fields)
# fields package is great for creating nice heatmap

# smoothSurface1=Tps(oneLocAngle1[, c('X', 'Y')],oneLocAngle1$average)
# vizSmooth1 = predictSurface(smoothSurface1)
# plot.surface(vizSmooth1, type = "C")
# points(oneLocAngle1$X, oneLocAngle1$Y, pch=19, cex = 0.5)

# dev.copy(png,'Heatmap1.png')
# dev.off()
# save plot as png

# oneLocAngle2=subset(offSignalSummary, mac==subMacs[1] & angle ==135)

# oneLocAngle2<-oneLocAngle2%>%
#  separate(XY, c("X", "Y"), "-")

# oneLocAngle2$X<-as.numeric(oneLocAngle2$X)
# oneLocAngle2$Y<-as.numeric(oneLocAngle2$Y)

# smoothSurface2=Tps(oneLocAngle2[, c("X","Y")],oneLocAngle2$average)
# vizSmooth2 = predictSurface(smoothSurface2)
# plot.surface(vizSmooth2, type = "C")
# points(oneLocAngle2$X, oneLocAngle2$Y, pch=19, cex = 0.5)

# dev.copy(png,'Heatmap2.png')
# dev.off()

### 8. mac address in X&Y

AP = matrix( c( 7.5, 6.3, 2.5, -.8, 12.8, -2.8, 1, 14, 33.5, 9.3,  33.5, 2.8),
             ncol = 2, byrow = TRUE, dimnames = list(subMacs[ -2 ], c("X", "Y") ))
# class(AP)
# dim(AP)

APdf<-as.data.frame(AP)
# class(APdf)

APdf<-setNames(cbind(rownames(APdf), APdf, row.names = NULL), c("mac", "X", "Y"))
# dim(APdf)
# 6 routers' location

# ggplot()+geom_point(aes(x=X, y=Y),data=APdf, color='black')+geom_point(color='Red', aes(x=X, y=Y), data=offlineXYcount)+ggtitle('Calibration Point (red) and Mac (black)')+geom_text(aes(x=X, y=Y, label=APdf$mac),data=APdf, size=3, nudge_y=-0.5)

# ggsave('CalibPointandMac.png', width=11, height=6)

# looking back to our questions in previous section (section 6)
# signal strength on one location depends on different mac address(two locations looked @ X==2&Y==12 and @ X==24&Y==4): maybe the closer to the mac, the stronger the signal
# Yes! We were right! Closer to mac, the stronger the signal collected.

offSignalSummary<-subset(offSignalSummary, mac != subMacs[2])

# unique(offSignalSummary$mac)
# 6 in total now. Excluded '00:0f:a3:39:dd:cd' from further analysis

LocationDiff<-offSignalSummary[, c('X','Y')] - AP[offSignalSummary$mac,c('X','Y')]

offSignalSummary$dist<-sqrt(LocationDiff[ , 1]^2 + LocationDiff[ , 2]^2)
# calculated the distance to access points

# cor.test(offSignalSummary$average, offSignalSummary$dist)
# cor.test(offSignalSummary$median, offSignalSummary$dist)

# ggplot(aes(x=dist, y=average), data=offSignalSummary)+geom_point(color='blue', alpha=0.5)+facet_grid(factor(angle)~mac)+ggtitle('Signal Strength vs. Distance')+xlab('Distance to Access Points')+ylab('Signal Strength Mean')

# ggsave('SignalStrengthvsDist.pdf', width=12, height=10)
# ggsave('SignalStrengthvsDist.png', width=10, height=10)

# signal strength decrease as the distance to access points increase

# ggplot(aes(x=dist, y=median), data=offSignalSummary)+geom_point(color='blue', alpha=0.5)+facet_grid(factor(angle)~mac)+ggtitle('Signal Strength vs. Distance')+xlab('Distance to Access Points')+ylab('Signal Strength Median')

# ggplot(aes(x=dist, y=average), data=subset(offSignalSummary, mac=subMacs[1]))+geom_line(aes(color=factor(angle)), alpha=0.2)

# ggplot(aes(x=dist, y=average), data=subset(offSignalSummary, mac=subMacs[3]))+geom_point(aes(color=factor(angle)), alpha=0.2)+geom_smooth()

# signal strength decrease as the distance to access points increase


### 9. Preparing the Test Data: online dataset

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
    
    # keep only signals from access points
    offline = offline[ offline$type == "3", ]
    
    # drop scanMac, posZ, channel, and type - no info in them
    dropVars = c("scanMac", "Z", "channel", "type")
    offline = offline[ , !( names(offline) %in% dropVars ) ]
    
    # drop more unwanted access points
    offline = offline[ offline$mac %in% subMacs, ]
    
    # convert numeric values
    numVars = c("time", "X", "Y", "orientation", "signal")
    offline[ numVars ] = lapply(offline[ numVars ], as.numeric)
    
    # round orientations to nearest 45
    offline$angle = roundOrientation(offline$orientation)
    
    return(offline)
  }
# comparing with the instructor's code, I did not convert time in this readData function

macs = unique(offSignalSummary$mac)
online = readData("online.final.trace.txt", subMacs = macs)
online$XY = paste(online$X, online$Y, sep = "-")
# View(online)

# unique(online$XY)
# length(unique(online$XY))
# 60 locations to be studied

# table(online$XY, online$angle)
# dim(table(online$XY, online$angle))
# 60 XY and 8 angle
# for each XY location, they only picked one angle to do measurement. It is not like the calibration measurement with the offline dataset

# dim(table(online$XY, online$orientation))
# 60 XY and 61 orientation

onSignalSummary<-
  online %>% group_by(XY, mac) %>%
  summarise(mean = mean(signal) ,
            angle = mean(angle),
            orientation = min(orientation)) %>%
  ungroup()

# str(onSignalSummary)

onSignalSummary<-spread(onSignalSummary, mac, mean)
# convert it to a wide format

onSignalSummary$XY2<-onSignalSummary$XY
# duplicate XY column

onSignalSummary<-onSignalSummary%>%
  separate(XY2, c("X", "Y"), "-")

onSignalSummary$X<-as.numeric(onSignalSummary$X)
onSignalSummary$Y<-as.numeric(onSignalSummary$Y)

# View(onSignalSummary)
# str(onSignalSummary)
# dim(onSignalSummary)
# agree with previous findings: 60 observations, 11 columns

# identical(subMacs,names(onSignalSummary)[4:9])
# False: different order
# subMacs
# names(onSignalSummary)[4:9]

### 10. Choice of Orientation (how to subset offSignalSummary?)
# from previous section, we know that orientation(angle) can impact the strength of the signal

# m = 3
# angleNewObs = 230
# m: numbers of angles we wanted to incorporate into the estimation
# angleNewObs: as an example

# refs=seq(0, by=45, length=8)

# nearestAngle = roundOrientation(angleNewObs)

# nearestAngle

# if (m %% 2 == 1) {
#   angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
# } else {
#  m = m + 1
#  angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
#  if (sign(angleNewObs - nearestAngle) > -1) 
#    angles = angles[ -1 ]
#  else 
#    angles = angles[ -m ]
# }
# angles = angles + nearestAngle
# angles[angles < 0] = angles[ angles < 0 ] + 360
# angles[angles > 360] = angles[ angles > 360 ] - 360

# angles
# angles: 180, 225, 270

# offSubset =  offSignalSummary[ offSignalSummary$angle %in% angles, ]
# View(offSubset)
# unique(offSubset$angle)
# making sure we are subseting the dataframe in a correct way! Just three angles as we expected

reshapeDF = function(data, varSignal = "average")
{
  subsetDF<-data %>% group_by(XY, mac) %>%
    summarise(X=mean(X), Y=mean(Y), average=mean(average)) %>%
    ungroup()
  subsetWide<-spread(subsetDF, mac, average)
  return(subsetWide)
}
# reshapeDF:
#   input: a dataframe and varSignal
#   output: a wide format of that dataframe
# if want to keep angle: subsetDF<-group_by(data, X, Y, mac, angle)
# this will keep angle column in the train130

# trainSS = reshapeDF(offSubset, varSignal = "average")

selectTrain = function(angleNewObs, signals = NULL, m = 1){
  # m is the number of angles to keep
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

# train130 = selectTrain(130, offSignalSummary, m = 3)

# View(train130)
# trainXXX data subset should only have eight columns in total: X, Y and 6 mac. We already average the signal on angles.
# selectTrain averages the signal strengths for the different angles to produce one set of signal strengths for each of the 166 locations in the training data

# head(train130)

# length(train130[[1]])


### 11. Finding the Nearest Neighbors

findNearestNeighbor = function(newSignal, trainData) {
  difference = apply(trainData[ , 4:9], 1, 
                     function(x) x - newSignal)
  dists = apply(difference, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainData[closest, 1:3 ])
}
# findNearestNeighbor:
#   input: onSignalSummary as newSignal and as offSignalSummary as trainData
#   output: return closest X, Y position information

predXY = function(newSignals, newAngles, trainData, numAngles, k){
  
  closeXY = list(length = nrow(newSignals))
  # create an empty list to save results  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles$orientation[i], trainData, numAngles)
    closeXY[[i]] = 
      findNearestNeighbor(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  
  estXY = lapply(closeXY, function(x) sapply(x[ , 2:3], 
                                             function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}
# numAngles: how many angles do we want to include from the offSignalSummary
# k: how many nearest neighbours do we want to average on

calcError = function(estXY, actualXY) 
  sum( rowSums( (estXY - actualXY)^2) )
# calculate the difference between the prediction model and actual XY location

estXYk1 = predXY(newSignals = onSignalSummary[, 4:9], 
                 newAngles = onSignalSummary[, 3], 
                 offSignalSummary,
                 numAngles = 3, 1)

estXYk3 = predXY(newSignals = onSignalSummary[, 4:9], 
                 newAngles = onSignalSummary[, 3], 
                 offSignalSummary,
                 numAngles = 3, 3)

estXYk5 = predXY(newSignals = onSignalSummary[, 4:9], 
                 newAngles = onSignalSummary[, 3], 
                 offSignalSummary,
                 numAngles = 3, 5)

actualXY = onSignalSummary[ , c("X", "Y")]

sapply(list(estXYk1, estXYk3), calcError, actualXY)
#k=3 gave smaller calcError
sapply(list(estXYk5, estXYk3), calcError, actualXY)
#k=5 258 smaller calcError comparing with k=3
#how much k do we need in order to get to the smallest calcError?

OptimizeK = function(k){
  result=NULL
  for (i in 1:k) {
    estXY = predXY(newSignals = onSignalSummary[, 4:9], 
                   newAngles = onSignalSummary[, 3], 
                   offSignalSummary,
                   numAngles = 3, i)
    Error=sapply(list(estXY), calcError, actualXY)
    result=rbind(result,data.frame(i, Error))
  }
  return(result)
}

findoptK20<-OptimizeK(20)

# ggplot(aes(x=i, y=Error), data=findoptK20)+ geom_point()+geom_line()+ ggtitle('How many nearby neighbour to include in the prediction?')+labs(x='Numbers of Neighbour', y='Sum of Square Errors')

# ggsave('NeighbourPrediction.png')

# a follow-up question would be how many angles should we include in our training model in order to minimize the error?
# We set k=5 (numbers of nearby neighour points) to explore the optimized number of angles 

OptimizeAngles = function(numAngles){
  result=NULL
  for (i in 1:numAngles) {
    estXY = predXY(newSignals = onSignalSummary[, 4:9], 
                   newAngles = onSignalSummary[, 3], 
                   offSignalSummary,
                   numAngles = i, 5)
    Error=sapply(list(estXY), calcError, actualXY)
    result=rbind(result,data.frame(i, Error))
  }
  return(result)
}

findoptAngles<-OptimizeAngles(8)
# we have eight angles/orientations in total in our dataset

# ggplot(aes(x=i, y=Error), data=findoptAngles)+ geom_point()+geom_line()+ggtitle('How many angles to include in the prediction?')+labs(x='Numbers of Angle', y='Sum of Square Errors')
# ggsave('AnglePrediction.png')


### 12. Visualizing Final Estimation

dfestXYk5<-as.data.frame(estXYk5)
dfestXYk5$ID <- seq.int(nrow(dfestXYk5))
str(dfestXYk5)

actualposition<-data.frame(onSignalSummary$X, onSignalSummary$Y)
colnames(actualposition)<-c('X','Y')
actualposition$ID <- seq.int(nrow(actualposition))

# ggplot(aes(x=X, y=Y), data=onSignalSummary)+geom_point(color='red')+geom_point(aes(x=X, y= Y), data=dfestXYk5, color='blue')

positioncompare<-rbind(actualposition, dfestXYk5)

# ggplot()+geom_point(aes(x=X, y=Y, group=ID), data=positioncompare, color='red')+geom_line(aes(x=X, y=Y, group=ID), data=positioncompare, color='black', linetype = "dashed")+geom_point(aes(x=X, y=Y), data=positioncompare[1:60,], color='black')+geom_point(aes(x=X, y=Y), shape=21, data=offSignalSummary,color='grey')+ggtitle('Floor Map with Predicted and Actual Locations')

# ggsave('FloorMapPredictedActualLocs.png')