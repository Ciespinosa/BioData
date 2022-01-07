#extraer por Ecoregion

# library(raster)
# library(rnaturalearth)
# world <- ne_countries(continent = "South America")
# class(world)
# 
# world <- aggregate(world)
# 
# 
# ecoreg <- st_read("2._data/ecoregion/Terrestrial_Ecoregions_World.shp")
# ecoreg1 <- as_Spatial(ecoreg)  
# 
# crs(world) <- crs(ecoreg1) 
# 
# world <- gBuffer(world, byid=TRUE, width=0)
# ecoreg1 <- gBuffer(ecoreg1, byid=TRUE, width=0)
# 
# ecoSouth <- raster:::intersect(world,ecoreg1)
# tm_shape(ecoSouth) + tm_fill(col="ECO_NAME") +tm_borders(col = "white")
# 
# 
# ecoSouth <- st_as_sf(ecoSouth)
# st_write(ecoSouth, "2._data/ecoregion/ecoSouth.shp", driver="ESRI Shapefile")  


library(sf)
library(raster)
library(tmap)

source(file = "3._script/datos.R", encoding = "UTF-8")

ecoreg <- st_read("2._data/ecoregion/ecoSouth.shp")


##Transformamos en datos espaciales
dta <- dta[complete.cases(dta[,c("X", "Y")]),]
dta_spoint <- dta


#dta_spoint$country <- droplevels(as.factor(dta_spoint$Pais), exclude = 0)
coordinates(dta_spoint) <-  ~Y+X


ecoreg1 <- as_Spatial(ecoreg)  


##Unificamos la proyeccion espacial
crs(dta_spoint) <- crs(ecoreg1)


##Extraemos los datos de la capa de fondo
dtaext <- over(dta_spoint, ecoreg1)

##Unificamos los datos
dta <- cbind(dta, filtroE=dtaext$ECO_NAME, filtroP=dtaext$name)

secos <- c("South American Pacific mangroves",
           "Tumbes-Piura dry forests",
           "Ecuadorian dry forests",
           "Eastern Cordillera real montane forests",
           "Sechura desert",
           "Guayaquil flooded grasslands" ,
           "Marañón dry forests"
)

dta$Bioma <- dta$filtroE

dta$Bioma[dta$filtroE %in% secos] <- "Bosques secos"
dta$Bioma[!dta$filtroE %in% secos] <- "Bosques lluviosos"

#dta <- dta[complete.cases(dta[,c("filtroP", "filtroE")]),]
  
#write.csv2()

#readr::write_csv(dta, "2._data/dta.csv")
