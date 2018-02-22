Predicting location via indoor positioning systems

This example is taken from: Chapter 1 Predicting Location via Indoor Positioning Systems from the book: Case Studies in Data Science with R by Deborah Nolan, University of California, Berkeley and Duncan Temple Lang, University of California, Davis.
Detailed information and dataset can be downloaded from: http://rdatasciencecases.org/Data.html

![Heatmap1](Heatmap1.png?raw=true "FirstHeatmap")
Fig.1 Signal strength heat map generated angle==0 and with mac=="00:0f:a3:39:e1:c0"

Brief Background Information:
data size: nearly one million measurements of signal/measurements
two documents/datasets: offline(as the training set) and online (as the test set)

![SignalCounts](CountsofSignalsatEachLoc.png?raw=true "SignalCounts")
Fig. 2 Counts of signals detected at each calibration points (red, labels shows the number of counts at each location)

offline: signal strengths measured on a grid of 166 points spaced 1 meter apart in the hallways of one floor shown in the figure
![FloorMapandMac](CalibPointandMac.png?raw=true "FloorMapandMac")
Fig. 3 Floor map with calibration points (red, 166 points in total) and router position (blacks, 6 routers in total)



# Signal strengths were recorded at 8 orientations in 45 degree increments. Further, the documentation for the data indicates that 110 signal strength measurements were recorded to each of the 6 access points for every location-orientation combination.
![SignalStrengthvsDist](SignalStrengthvsDist.png?raw=true "SignalStrengthvsDist")

# online: location prediction using the trained model from the offline data set
# online: 60 locations and orientations are chosen at random and 110 signals are measured from them to each access point.


In this study, part 1 and 2 focused on data cleaning and formating. Part 3 to part 7 looked at different variables, orientation, mac address, X and Y position, distance to router and their effect towards signal strength.
