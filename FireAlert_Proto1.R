library(raster)
library(rgdal)
library(maptools)
library(sp)
library(geosphere)
library(rgeos)
library(OpenStreetMap)
library(rjson)


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


temp0 <- tempfile()
download.file("https://www.dropbox.com/s/pi6kubj9603wi9d/chile_wgs84.geojson?dl=0",temp0)
myShape<-fromJSON(paste(readLines(temp0), collapse=""))
unlink(temp)



              
setwd("Z:/GEODATABASE/VECTOR/Chile_utm19/chile_utm19/chile_continental_utm19/")
myShape <- readOGR("chile_wgs84.shp")
projection(myShape)<- CRS("+proj=longlat +datum=WGS84")

st_mod <- modis[myShape, ]
st_vii <- viirs[myShape, ]

setwd("Z:/GEODATABASE/VECTOR/SNASPE_simplify/")
myASP <- readOGR("SNASPE_wgs84.shp")
projection(myASP)<- CRS("+proj=longlat +datum=WGS84")

st_mod_utm<-spTransform(st_mod, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
myASP_utm<-spTransform(myASP, CRS("+proj=utm +south +zone=19 +datum=WGS84"))

dist.mod<-as.data.frame(gDistance(myASP_utm, st_mod_utm,  byid=TRUE)) # filas son SNASPE y columnas hotspot
nASP<-myASP_utm@data[3]
colnames(dist.mod)<-nASP[,1]
dist.modis <- cbind(st_mod_utm@data, dist.mod)
dist.modis <- cbind(st_mod_utm@coords, dist.modis)
dist.modis <- cbind(ID=row.names(dist.modis), dist.modis)


# i=115
# z<-subset(dist.modis,dist.modis[,i]<26000)
# colnames(z)[which(z[15:115]<26000, arr.ind = T)] 
# 
# Map(`[`, list(names(z[,15:115])), split(col(z[,15:115])[z[,15:115] <15000], row(z[,15:115])[z[,15:115] <15000]))


#aqui hay un probelma!!!!!!!!!!!!!!!!!!!!!!
sdist.mod<-NULL
for (i in 15:115){
  z<-subset(dist.modis,dist.modis[,i]<14000)
  n<-Map(`[`, list(names(z[,15:115])), split(col(z[,15:115])[z[,15:115] <15000], row(z[,15:115])[z[,15:115] <15000]))
  z2<-cbind(n[1],z)
  sdist.mod<-rbind(sdist.mod,z2)
}

#coordinates(dist.modis) <- ~longitude+latitude
#writeOGR(dist.modis,"Z:/GEODATABASE/VECTOR", 'hotspot', driver="ESRI Shapefile")

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





