library(raster)
library(rgdal)
library(maptools)


download.file("https://firms.modaps.eosdis.nasa.gov/active_fire/viirs/text/VNP14IMGTDL_NRT_South_America_24h.csv" 
,"mydata", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))

my<-read.csv("mydata",sep=",",dec=".",header=T)

library(sp)
coordinates(my) <- ~longitude+latitude
projection(my) <- CRS("+proj=longlat +datum=WGS84")

setwd("c:/Users/Ignac/Downloads/SNASPE_versión 25092017.1/SNASPE_versión 25092017.1/Huso_19/")

myShape <- readOGR("SNASPE_Continente_wgs84.shp")
projection(myShape)<- CRS("+proj=longlat +datum=WGS84")

plot(myShape)
points(my,col="red",type="p")
