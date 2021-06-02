#extraer por pais

library(sf)
library(rnaturalearth)
world <- ne_countries(continent = "South America")

##Transformamos en datos espaciales

dta_spoint <- dta

dta_spoint$country <- droplevels(as.factor(dta_spoint$Pais), exclude = 0)
coordinates(dta_spoint) <-  ~Y+X

##Unificamos la proyeccion espacial
crs(dta_spoint) <- crs(world)
class(dta_spoint)

##Extraemos los datos de la capa de fondo
dtaext <- over(dta_spoint, world)

##Unificamos los datos

dta <- cbind(dta, filtro=dtaext$name)

