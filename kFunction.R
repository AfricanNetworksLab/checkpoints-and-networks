# Calculate bivariate Ripley's K-function
# Reading in spatial data with sf
# and transforming it to 
# David Russell, January 2026

###### Read in libraries ######
install.packages(c("sf","spatstat","maptools","tidyverse"))
library(sf)
library(spatstat)
library(maptools)
library(tidyverse)

###### Read in data ######
# Make sure shapefiles contain projected data

# Read in points
pointsShapefile <- st_read(file.choose())

# Read in study window shapefile and convert to owin
windowShapefile <- st_read(file.choose())

window.lon <- st_coordinates(windowShapefile)[,1]
window.lat <- st_coordinates(windowShapefile)[,2]

window.owin <-  owin(poly = list(x=rev(window.lon),y=rev(window.lat)))


###### Create spatstat ppp object ######
pointsShapefile <- pointsShapefile %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
points.ppp <- ppp(x = pointsShapefile$lon,
                  y = pointsShapefile$lat,
                  window = window.owin,
                  marks = as.factor(pointsShapefile$type)) 
# Make sure marks column name is correct


###### Perform K-function analysis ######

# Run homogeneous K-cross (bivariate K-function)
# Make sure names match
homogeneousK <- envelope(points.ppp, Kcross, i = "iName", j = "jName", correction = "iso")

# Run inhomogeneous K-cross
# Make sure names match
inhomogeneousK <- envelope(points.ppp, Kcross.inhom, i = "iName", j = "jName", correction = "iso")

