# Indoor_GPS
# Predicting location via indoor positioning systems
# Data science in R
![Alt text](relative/path/to/img.jpg?raw=true "Title")



# Brief background information:
# Example is taken from: Chapter 1 Predicting Location via Indoor Positioning Systems from the book: Case Studies in Data Science with R

# data size: nearly one million measurements of signal
# two documents: offline and online

# detailed information can be downloaded from: http://rdatasciencecases.org/Data.html

# offline: training data set
# offline: measured at 6 WiFi access points (routers)
# offline: signal strengths measured on a grid of 166 points spaced 1 meter apart in the hallways of one floor
# Signal strengths were recorded at 8 orientations in 45 degree increments. Further, the documentation for the data indicates that 110 signal strength measurements were recorded to each of the 6 access points for every location-orientation combination.

# online: location prediction using the trained model from the offline data set
# online: 60 locations and orientations are chosen at random and 110 signals are measured from them to each access point.
