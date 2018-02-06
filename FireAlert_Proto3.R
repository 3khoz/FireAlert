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
library(XML)

## Funcion kml


# ## Descargar HotSpot 
# 
# download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/viirs/csv/VNP14IMGTDL_NRT_South_America_24h.csv" 
#               ,"modis", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
# 
# download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/c6/csv/MODIS_C6_South_America_24h.csv" 
#               ,"viirs", method="auto", quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
# 
# modis<-read.csv("modis",sep=",",dec=".",header=T)
# viirs<-read.csv("viirs",sep=",",dec=".",header=T)
# coordinates(modis) <- ~longitude+latitude
# projection(modis) <- CRS("+proj=longlat +datum=WGS84")
# coordinates(viirs) <- ~longitude+latitude
# projection(viirs) <- CRS("+proj=longlat +datum=WGS84")
# 
# ## Filtrar por limites nacionales
# setwd("C:/Users/Gasp/Documents/GitHub/FireAlert/")
# myShape <- readOGR("data/Chile_continental.shp")
# projection(myShape)<- CRS("+proj=longlat +datum=WGS84")
# 
# st_mod <- modis[myShape, ]
# st_mod_utm<-spTransform(st_mod, CRS("+proj=utm +south +zone=19 +datum=WGS84"))
# st_vii <- viirs[myShape, ]
# st_vii_utm<-spTransform(st_vii, CRS("+proj=utm +south +zone=19 +datum=WGS84"))

## Cargar datos SIDCO

mySIDCO <- readOGR("data/sidco_06022018.shp")
projection(mySIDCO)<- CRS("+proj=longlat +datum=WGS84")
mySIDCO_utm<-spTransform(mySIDCO, CRS("+proj=utm +south +zone=19 +datum=WGS84"))

## Cargar coberturas de prioridad

myASP <- readOGR("data/Proteccion.shp")
projection(myASP)<- CRS("+proj=longlat +datum=WGS84")
myASP_utm<-spTransform(myASP, CRS("+proj=utm +south +zone=19 +datum=WGS84"))

## Calcular distancia a...

# ## MODIS
# dist.mod<-as.data.frame(gDistance(myASP_utm, st_mod_utm,  byid=TRUE)) # filas son SNASPE y columnas hotspot
nASP<-myASP_utm@data[3]
# colnames(dist.mod)<-nASP[,1]
# dist.modis <- cbind(st_mod_utm@data, dist.mod)
# dist.modis <- cbind(st_mod@coords, dist.modis)
# dist.modis <- cbind(ID=row.names(dist.modis), dist.modis)
# name_asp<-as.data.frame(myASP)
# name_asp<-unique(name_asp$UNIDAD)
# 
# 
# sMODIS<-NULL
# for (i in 14:115){
#   d1<-subset(dist.modis,dist.modis[,i]<5000)
#   sMODIS<-rbind(sMODIS,d1)
# }
# 
# ## Viirs
# dist.vii<-as.data.frame(gDistance(myASP_utm, st_vii_utm,  byid=TRUE)) # filas son SNASPE y columnas hotspot
# colnames(dist.vii)<-nASP[,1]
# dist.viis <- cbind(st_vii_utm@data, dist.vii)
# dist.viis <- cbind(st_vii@coords, dist.viis)
# dist.viis <- cbind(ID=row.names(dist.viis), dist.viis)
# 
# sVIIRS<-NULL
# for (i in 14:115){
#   d1<-subset(dist.viis,dist.viis[,i]<5000)
#   sVIIRS<-rbind(sVIIRS,d1)
# }

## join de ubicacion

myRCP <- readOGR("data/union_rpc.shp")
projection(myRCP)<- CRS("+proj=longlat +datum=WGS84")

rcp<-over(mySIDCO, myRCP)

## SIDCO
dist.SIDCO<-as.data.frame(gDistance(myASP_utm, mySIDCO_utm, byid=TRUE)) # filas son SNASPE y columnas hotspot
colnames(dist.SIDCO)<-nASP[,1]
dist.SIDCOs <- cbind(mySIDCO_utm@data, dist.SIDCO)
dist.SIDCOs <- cbind(rcp[2:4],dist.SIDCOs)
cord<-mySIDCO@coords[,1:2]
colnames(cord)<-c("longitude","latitude")
dist.SIDCOs <- cbind(cord, dist.SIDCOs)
dist.SIDCOs<- cbind(ID=row.names(dist.SIDCOs), dist.SIDCOs)

#write.table(dist.SIDCOs,"dist.csv",sep=";",dec=",",row.names = F)

sSIDCOs<-NULL
m1<-dim(dist.SIDCOs)[2]

for (i in 9:m1){
  d1<-subset(dist.SIDCOs,dist.SIDCOs[,i]<5000)
  sSIDCOs<-rbind(sSIDCOs,d1)
}

datafin<-as.data.frame(sSIDCOs)
frecuen<-as.data.frame(summary(datafin$Name))
fre<-cbind(Name=row.names(frecuen),Frecuencia=frecuen$`summary(datafin$Name)`)
unicos<-unique(merge(datafin,fre,by="Name"))

resumen<-NULL

for (i in 1:dim(unicos)[1]){
  
  x<-unicos[i,]
  f<-as.numeric(as.character(x$Frecuencia))
  s<-sort(x[9:509])
  nASP2<-myASP@data[2:3]
  
  for (h in 1:f) {
    n1<-names(s[h])[which.min(apply(s[h],MARGIN=2,min))]
    n2<-subset(nASP2,nASP2$NAME==n1)
    n2<-as.character(n2$TIPO)
    n3<-(s[h])[which.min(apply(s[h],MARGIN=2,min))]
    n3<-as.numeric(n3[1])/1000
    nf<-cbind(x[1:8],Sitio=n1,Tipo=n2,DistFoco=n3)
    resumen<-rbind(resumen,nf)
}}

name1<-gsub('.*/(.*)','\\1',resumen$FolderPath)

compi<-cbind(resumen[1],resumen[5:7],Estado=name1,resumen[9:11])


sidcors<-datafin[,2:3]

library(sp)
coordinates(sSIDCOs)<-~longitude+latitude
projection(sSIDCOs)<- CRS("+proj=longlat +datum=WGS84")

b1<-buffer(sSIDCOs,5000,dissolve=F)
projection(b1)<- CRS("+proj=longlat +datum=WGS84")

# plot

  i=1
  leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%
    addCircles(data = sSIDCOs[i,],radius = 5000 ,fill = T, fillOpacity = .05,stroke = TRUE, color = "#FF0000", 
               popup = paste0("Name: ", as.character(sSIDCOs$Name[i])), group = "SIDCO") %>%
    setView(lng = mean(sidcors$longitude[i]), lat = mean(sidcors$latitude[i]), zoom = 11) %>%
    addCircles(data = sSIDCOs[i,],radius = 5,fill = T,stroke = TRUE, color = "#FF0000") %>%
    addPolygons(data = myASP, fill = TRUE, stroke = TRUE, color = "#36FF33", 
                popup = paste0("Unidad: ", as.character(myASP@data[,3])), group = "ASP") %>% 
    addLegend("bottomright", colors = c("#FF0000", "#36FF33"), labels = c("SIDCO 5 km", "ASP")) %>%   
    addLayersControl(overlayGroups = c("SIDCO","ASP"),options = layersControlOptions(collapsed = FALSE))

  
# 
# leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%
#   addCircles(data = sSIDCOs[,2:3],radius = 5000 ,fill = T, stroke = TRUE, color = "#FF0000", 
#              popup = paste0("Name: ", as.character(sSIDCOs$Name)), group = "SIDCO") %>%
#   addCircles(data = sMODIS[,2:3],radius = 5000 ,fill = T, stroke = TRUE, color = "#F70B81", 
#              popup = paste0("Fecha: ", as.character(dist.modis$acq_date)), group = "HotSpot MODIS") %>% 
#   addCircles(data = sVIIRS[,2:3],radius = 5000, fill = T, stroke = TRUE, color = "#FF8000", 
#              popup = paste0("Fecha: ", as.character(dist.modis$acq_date)), group = "HotSpot Viirs") %>%
#   addPolygons(data = myASP, fill = TRUE, stroke = TRUE, color = "#36FF33", 
#               popup = paste0("Unidad: ", as.character(myASP@data[,3])), group = "ASP") %>% 
#   # add a legend
#   addLegend("bottomright", colors = c("#FF0000","#F70B81","#FF8000", "#36FF33"), labels = c("SIDCO","HotSpot MODIS","HotSpot Viirs", "ASP")) %>%   
#   # add layers control
#   addLayersControl(
#     overlayGroups = c("SIDCO","HotSpot MODIS","HotSpot Viirs", "ASP"),
#     options = layersControlOptions(collapsed = FALSE)
#   )

  iconv.data.frame<-function (df, ...)
  {
    df.names <- iconv(names(df), ...)
    df.rownames <- iconv(rownames(df), ...)
    names(df) <- df.names
    rownames(df) <- df.rownames
    df.list <- lapply(df, function(x) {
      if (class(x) == "factor") {
        x <- factor(iconv(as.character(x), ...))
      }
      else if (class(x) == "character") {
        x <- iconv(x, ...)
      }
      else {
        x
      }
    })
    df.new <- do.call("data.frame", df.list)
    return(df.new)
  }
  
  iconv.data.frame(compi,from="UTF8",to="latin1")
  