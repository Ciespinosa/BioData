################################################################################
# Procesamiento y preparación
# Descripción:
#             En este script realizamos el procesamiento de los datos para que 
#             pueda ser analizado dentro del dashboard. Se debería ejecutar este 
#             script cada que se realice una acualización de los datos de entrada.
# 
# Versión 2
# Fecha: 31/03/2023
# Autor: Carlos Iván Espinosa
# 
################################################################################

library(readxl)
library(dplyr)
library(sf)
library(raster)
library(tmap)


## Leemos los datos

Ubica <- read_excel("2._data/dta.31.3.23.xlsx", sheet = "Ubicacion")
clase <- read_excel("2._data/dta.31.3.23.xlsx", sheet = "Taxonomia")
tipo <- read_excel("2._data/dta.31.3.23.xlsx", sheet = "Bibliografia")


#Preparamos los datos ----

clase <- unique(clase[,c("Codigo", "Clase")])

Ubica <- unique(Ubica[, c("Codigo", "X","Y", "Pais")])


#Datos de bibliografía

tipo <- left_join(tipo %>% 
                    filter(Tipo== "Artículo"),
                  clase, by = "Codigo")


tipo$Tematica <- factor(tipo$Tematica)

##Unimos algunas temáticas

levels(tipo$Tematica) <- list(Distribución = "Biogeografía y distribución",
                              Conservación = c("Conservación", "Crónicas"),
                              Diversidad = "Diversidad",
                              Ecología = c("Ecología", "General", "Especies introducidas y parásitos", "Paleontología" ),
                              H.Natural = "Historia natural",
                              Manejo = c("Manejo y crianza en cautiverio", "Etnozoología y cacería"),
                              Educación = "Educación y difusión",
                              Sistemática = "Sistemática")

tipo$Clase <- factor(tipo$Clase)


#Unimos tipo y ubicación

tipo <- merge(tipo, unique(Ubica[, c("Codigo", "Pais")]) ,by = "Codigo", all.x = T)


##Datos de ubicación por clase

dta1 <- merge(Ubica, clase[c("Codigo", "Clase")], by = "Codigo", all.x = T)

## Obtenemos los datos de la ecoregión ----

#extraer por Ecoregion

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


dta1$Clase <- factor(dta1$Clase, levels = c( "AVES","AMPHIBIA", "MAMMALIA", "PECES", "REPTILIA", "PLANTAE"))


dta <- left_join(tipo, 
                 dta1%>% 
                   dplyr::select(Codigo, Bioma) %>% 
                   group_by(Codigo) %>% 
                   unique(), by = "Codigo")


dta <-  dta %>% 
  mutate(Bioma = ifelse(is.na(Bioma), "N.A.", Bioma))


rm(list = setdiff(ls(), c("dta", "dta1")))
save.image("data.RData")

