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

In this study, part 1 and 2 focused on data cleaning and formating. We started with offline dataset and formated it into a clean and organized dataframe. Part 3 to part 8 looked at different variables, orientation, mac address, X and Y position, distance to router and their effect towards signal strength.

![SignalStrengthvsDist](SignalStrengthvsDist.png?raw=true "SignalStrengthvsDist")

Fig. 4 Signal strength plotted against distance to mac. The signal strength decreases with the distance to mac increase

Part 9 to part 10, we moved on to the online dataset (test dataset). We modified it into a dataframe format we preferred. Our goal was to built a prediction model using the signal strength in the online dataset to predict corresponding X and Y position. Online dataset contains information measured at 60 radom locations.

Part 11, through k-Nearest-Neighbour method, we built prediction model based on offline dataset and tested the model with the online dataset. We calculated the error in the model and further explored the number of nearby calibration points and number of nearby angles we need to include in the training model in order to get to minimize our estimation error.

![NeighbourPrediction](NeighbourPrediction.png?raw=true "NeighbourPrediction")


Fig. X Error against the number of nearby calibration points used in the training model.


![AnglePrediction](AnglePrediction.png?raw=true "AnglePrediction")


Fig. X Error against the number of nearby angles used in the training model.


Part 12, we visulized final results by plotting the prediction X and Y location against actual X and Y location in the online dataset via ggplot2.


![FloorMapPredictedActualLocs](FloorMapPredictedActualLocs.png?raw=true "FloorMapPredictedActualLocs")


Fig. X Floor map with actual position (solid black), estimated position (solid red) and calibration points (hollow grey circle)


