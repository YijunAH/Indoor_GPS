Predicting location via indoor positioning systems

Please wait for figures to show up

This example is taken from: Chapter 1 Predicting Location via Indoor Positioning Systems from the book: Case Studies in Data Science with R by Deborah Nolan, University of California, Berkeley and Duncan Temple Lang, University of California, Davis.
Detailed information and data files can be downloaded from: http://rdatasciencecases.org/Data.html

![Heatmap1](doc/Heatmap1.png?raw=true "FirstHeatmap")
Fig.1 Signal strength heat map generated @angle==0 with mac=="00:0f:a3:39:e1:c0"

Brief Background Information about this dataset:
Data size: nearly one million measurements of signal/rows
Data structure: two documents/datasets: offline(as the training set) and online (as the test set)

![FloorMapandMac](CalibPointandMac.png?raw=true "FloorMapandMac")
Fig. 2 Floor map with calibration points (red point, 166 points in total, spaced 1 meter apart in the hallways of one floor) and mac position (black point, 6 routers in total).

I have two RMD files in this repository. The one named "XXXX" is the one with all my comments and graphs. The one named "XXX" is a 'cleaned' version of the previous one. It only has all the required lines to run the final position prediction estimation, without any comments or graphs.

In '', I divided this study into 12 parts/sections. Part 1 and 2 focused on data cleaning and formating. We started with playing with the offline dataset and formated it into a clean and organized dataframe. From part 3 to part 8, we looked at different variables, orientation, mac address, X and Y position, distance to mac and their effect towards signal strength.

![SignalStrengthvsDist](SignalStrengthvsDist.png?raw=true "SignalStrengthvsDist")
Fig. 3 Signal strength plotted against distance to mac. The signal strength decreases with the distance to mac increase

Part 9 to part 10, we moved on to the online dataset (test dataset). We modified it into a dataframe format we preferred. Our goal was to built a prediction model using the signal strength in the online dataset to predict corresponding X and Y position. Online dataset contains information measured at 60 radom locations.

Part 11, through k-Nearest-Neighbour method, we built prediction model based on offline dataset and tested the model with the online dataset. We calculated the error in the model and further explored the number of nearby calibration points and number of nearby angles we need to include in the training model in order to minimize our estimation error.

![NeighbourPrediction](NeighbourPrediction.png?raw=true "NeighbourPrediction")
Fig. X Error against the number of nearby calibration points used in the training model.

![AnglePrediction](AnglePrediction.png?raw=true "AnglePrediction")
Fig. X Error against the number of nearby angles used in the training model.

In part 12, we visualized final result by plotting the prediction X and Y location against actual X and Y location in the online dataset via ggplot2.

![FloorMapPredictedActualLocs](FloorMapPredictedActualLocs.png?raw=true "FloorMapPredictedActualLocs")
Fig. X Floor map with actual position (solid black), estimated position (solid red) and calibration points (hollow grey circle)
