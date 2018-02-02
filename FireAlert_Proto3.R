## ALERTA DE INCENDIOS EN ASP
## UNIDAD DE MONITOREO y PREDICCIÓN
## FECHA: Enero 2018

## Cargar librerias

library(raster)
library(rgdal)
library(maptools)
library(sp)
library(geosphere)
library(rgeos)
library(rJava)
library(OpenStreetMap)
library(rjson)
library(leaflet)

## Descargar HotSpot 

download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/viirs/csv/VNP14IMGTDL_NRT_South_America_24h.csv" 
              ,"modis", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))

download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/c6/csv/MODIS_C6_South_America_24h.csv" 
              ,"viirs", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))

modis<-read.csv("modis",sep=",",dec=".",header=T)
viirs<-read.csv("viirs",sep=",",dec=".",header=T)
coordinates(modis) <- ~longitude+latitude
projection(modis) <- CRS("+proj=longlat +datum=WGS84")
coordinates(viirs) <- ~longitude+latitude
projection(viirs) <- CRS("+proj=longlat +datum=WGS84")

## Filtrar por limites nacionales

setwd("z:/GEODATABASE/VECTOR/utilidades/")
myShape <- readOGR("Chile_continental.shp")
projection(myShape)<- CRS("+proj=longlat +datum=WGS84")

st_mod <- modis[myShape, ]
st_mod_utm<-spTransform(st_mod, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
st_vii <- viirs[myShape, ]
st_vii_utm<-spTransform(st_vii, CRS("+proj=utm +south +zone=19 +datum=WGS84"))

## Cargar datos SIDCO

setwd("z:/GEODATABASE/VECTOR/")
mySIDCOc <- readOGR("combate.shp")
projection(mySIDCOc)<- CRS("+proj=longlat +datum=WGS84")
mySIDCOo <- readOGR("observacion.shp")
projection(mySIDCOo)<- CRS("+proj=longlat +datum=WGS84")
mySIDCO<-rbind(mySIDCOc,mySIDCOo)
mySIDCO_utm<-spTransform(mySIDCO, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
rm(mySIDCOc,mySIDCOo)

## Cargar coberturas de prioridad

setwd("z:/GEODATABASE/VECTOR/utilidades/")
myASP <- readOGR("SNASPE.shp")
projection(myASP)<- CRS("+proj=longlat +datum=WGS84")
myASP_utm<-spTransform(myASP, CRS("+proj=utm +south +zone=19 +datum=WGS84"))

myBNP <- readOGR("BNP.shp")
projection(myBNP)<- CRS("+proj=longlat +datum=WGS84")
myBNP_utm<-spTransform(myBNP, CRS("+proj=utm +south +zone=19 +datum=WGS84"))


## Calcular distancia a...

## MODIS
dist.mod<-as.data.frame(gDistance(myASP_utm, st_mod_utm,  byid=TRUE)) # filas son SNASPE y columnas hotspot
nASP<-myASP_utm@data[3]
colnames(dist.mod)<-nASP[,1]
dist.modis <- cbind(st_mod_utm@data, dist.mod)
dist.modis <- cbind(st_mod@coords, dist.modis)
dist.modis <- cbind(ID=row.names(dist.modis), dist.modis)
name_asp<-as.data.frame(myASP)
name_asp<-unique(name_asp$UNIDAD)
rm(dist.mod)

sMODIS<-NULL
for (i in 14:115){
  d1<-subset(dist.modis,dist.modis[,i]<5000)
  sMODIS<-rbind(sMODIS,d1)
}

## Viirs
dist.vii<-as.data.frame(gDistance(myASP_utm, st_vii_utm,  byid=TRUE)) # filas son SNASPE y columnas hotspot
colnames(dist.vii)<-nASP[,1]
dist.viis <- cbind(st_vii_utm@data, dist.vii)
dist.viis <- cbind(st_vii@coords, dist.viis)
dist.viis <- cbind(ID=row.names(dist.viis), dist.viis)
rm(dist.vii) 

sVIIRS<-NULL
for (i in 14:115){
  d1<-subset(dist.viis,dist.viis[,i]<5000)
  sVIIRS<-rbind(sVIIRS,d1)
}

## SIDCO
dist.SIDCO<-as.data.frame(gDistance(myASP_utm, mySIDCO_utm,  byid=TRUE)) # filas son SNASPE y columnas hotspot
colnames(dist.SIDCO)<-nASP[,1]
dist.SIDCOs <- cbind(mySIDCO_utm@data, dist.SIDCO)
dist.SIDCOs <- cbind(mySIDCO@coords[,1:2], dist.SIDCOs)
dist.SIDCOs<- cbind(ID=row.names(dist.SIDCOs), dist.SIDCOs)
rm(d1,dist.SIDCO,modis,myShape,myASP)

sSIDCOs<-NULL
for (i in 14:114){
  d1<-subset(dist.SIDCOs,dist.SIDCOs[,i]<5000)
  sSIDCOs<-rbind(sSIDCOs,d1)
}

names(sSIDCOs)<-c("ID","longitude","latitude")


leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircles(data = sSIDCOs[,2:3],radius = 100 ,fill = T, stroke = TRUE, color = "#FF0000", 
             popup = paste0("Fecha: ", as.character(dist.modis$acq_date)), group = "SIDCO") %>%
  addCircles(data = sMODIS[,2:3],radius = 100 ,fill = T, stroke = TRUE, color = "#F70B81", 
             popup = paste0("Fecha: ", as.character(dist.modis$acq_date)), group = "HotSpot MODIS") %>% 
  addCircles(data = sVIIRS[,2:3],radius = 100, fill = T, stroke = TRUE, color = "#FF8000", 
             popup = paste0("Fecha: ", as.character(dist.modis$acq_date)), group = "HotSpot Viirs") %>%
  addPolygons(data = myASP, fill = TRUE, stroke = TRUE, color = "#36FF33", 
              popup = paste0("Unidad: ", as.character(myASP@data[,3])), group = "ASP") %>% 
  # add a legend
  addLegend("bottomright", colors = c("#FF0000","#F70B81","#FF8000", "#36FF33"), labels = c("SIDCO","HotSpot MODIS","HotSpot Viirs", "ASP")) %>%   
  # add layers control
  addLayersControl(
    overlayGroups = c("SIDCO","HotSpot MODIS","HotSpot Viirs", "ASP"),
    options = layersControlOptions(collapsed = FALSE)
  )