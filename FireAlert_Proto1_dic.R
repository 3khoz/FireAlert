library(raster)
library(rgdal)
library(maptools)
library(sp)
library(geosphere)
library(rgeos)
library(OpenStreetMap)


download.file("https://firms.modaps.eosdis.nasa.gov/active_fire/viirs/text/VNP14IMGTDL_NRT_South_America_24h.csv" 
              ,"modis", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
download.file("https://firms.modaps.eosdis.nasa.gov/active_fire/c6/text/MODIS_C6_South_America_24h.csv" 
              ,"viirs", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))

modis<-read.csv("modis",sep=",",dec=".",header=T)
viirs<-read.csv("viirs",sep=",",dec=".",header=T)
coordinates(modis) <- ~longitude+latitude
projection(modis) <- CRS("+proj=longlat +datum=WGS84")
coordinates(viirs) <- ~longitude+latitude
projection(viirs) <- CRS("+proj=longlat +datum=WGS84")

setwd("z:/GEODATABASE/VECTOR/utilidades/")
myShape <- readOGR("Chile_continental.shp")
projection(myShape)<- CRS("+proj=longlat +datum=WGS84")

st_mod <- modis[myShape, ]
st_vii <- viirs[myShape, ]

setwd("z:/GEODATABASE/VECTOR/utilidades/")
myASP <- readOGR("SNASPE.shp")
projection(myASP)<- CRS("+proj=longlat +datum=WGS84")

##st<-spTransform(st, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
##myASP<-spTransform(myASP, CRS("+proj=utm +south +zone=19 +datum=WGS84"))


dist.mod <- geosphere::dist2Line(p = st_mod, line = myASP)
dist.modis <- cbind(st_mod, dist.mod)
dist.vii <- geosphere::dist2Line(p = st_vii, line = myASP)
dist.viis <- cbind(st_vii, dist.vii)


sdist.mod<-subset(dist.modis,dist.modis$distance<10000)


#for (i in dim(sdist.mod)[1]){
i=1
ASPsel<-myASP[myASP$OBJECTID==sdist.mod$ID[i],]
ASPext<-ASPsel@bbox
zoom<-0.12

map <- openmap(c(ASPext[2,1]-zoom,ASPext[1,1]-zoom), c(ASPext[2,2]+zoom, 
                                                       ASPext[1,2]+zoom),type="osm")

map <- openproj(map,projection="+proj=longlat +datum=WGS84")

plot(map)
title(ASPsel$UNIDAD,cex.main=1,col.main="black")
#ASPsel <- spTransform(ASPsel, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
plot(ASPsel,add=T,border="darkgreen",lwd=2)

pp<-sdist.mod[sdist.mod$ID==sdist.mod$ID[i],]
#pp <- spTransform(pp, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
points(pp,pch=21,col="white",lwd=2,cex=2,bg="red")

legend("bottomleft",pch=c(NA,21),lty=c(1,NA),lwd=c(2,NA),col=c("green","red"), 
       legend=c("Límite ASP","Hot spot"))

### ENVIAR CORRE0

