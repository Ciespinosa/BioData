#extraer por Ecoregion

library(sf)
library(raster)
library(tmap)

source("3._script/datos2.R", encoding = "UTF-8")

ecoreg <- st_read("2._data/ecoregion/ecoSouth.shp")


##Transformamos en datos espaciales

dta_spoint <- dta1[complete.cases(dta1[,c("X", "Y")]),]
dta1 <- dta1[complete.cases(dta1[,c("X", "Y")]),]
dta_spoint$country <- droplevels(as.factor(dta_spoint$Pais), exclude = 0)
coordinates(dta_spoint) <-  ~Y+X


ecoreg1 <- as_Spatial(ecoreg)  


##Unificamos la proyeccion espacial
crs(dta_spoint) <- crs(ecoreg1)


##Extraemos los datos de la capa de fondo
dtaext <- over(dta_spoint, ecoreg1)

##Unificamos los datos
dta1 <- cbind(dta1, filtroE = dtaext$ECO_NAME, filtroP=dtaext$name)

secos <- c("South American Pacific mangroves",
                     "Tumbes-Piura dry forests",
                     "Ecuadorian dry forests",
                     "Eastern Cordillera real montane forests",
                     "Sechura desert",
                     "Guayaquil flooded grasslands" ,
                     "Marañón dry forests"
)

dta1$Bioma <- dta1$filtroE

dta1$Bioma[dta1$filtroE %in% secos] <- "Bosques secos"
dta1$Bioma[!dta1$filtroE %in% secos] <- "Bosques lluviosos"

#dta1 <- dta1[complete.cases(dta1[,c("filtroP", "filtroE")]),]


dta1$Clase <- factor(dta1$Clase, levels = c( "AVES","AMPHIBIA", "MAMMALIA", "PECES", "REPTILIA"))

write.csv2(dta1, "2._data/dta1.csv")
