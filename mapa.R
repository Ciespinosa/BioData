library(plotly)
library(readxl)

dta <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Ubicacion")
clase <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Taxonomia")

dta <- aggregate(dta, list(codigo = dta$Codigo,
                                 Pais = dta$País,
                                 Provincia = dta$Provincia,
                                 Canton = dta$Cantón,
                                 Localidad = dta$Localidad), max)

dta <- merge(dta, clase, by = "Codigo", all.x = T)

#Densidad ----

fig <- dta 
fig <- fig %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~X,
    lon = ~Y,
    coloraxis = 'coloraxis',
    radius = 2) 
fig <- fig %>%
  layout(
    mapbox = list(
      style = "stamen-terrain",
      center = list(lon=-75),
      zoom = 3), 
      coloraxis = list(colorscale = "Rainbow"))

fig

#Puntos ----

fig <- dta 

fig <- fig %>%
  plot_ly(
    lat = ~X,
    lon = ~Y,
    type = 'scattermapbox',
    hovertext = 'text',
    mode = 'markers',
    color = dta$Clase,
    paste("Clase: ", dta$Clase,
          "<br>País: ", dta$País,
          "<br>Localidad: ", dta$Localidad)
    ) 

fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =3.8,
      center = list(lon = -80, lat = -2),
    coloraxis = list(colorscale = "Rainbow")) )

fig


#Querys



library(rnaturalearth)
world <- ne_countries(returnclass = "sf", continent = "South America")
class(world)
#> [1] "sf"    "data.frame"

library(sf)
data(wrld_simpl)

##Transformamos en datos espaciales

dta_spoint <- dta
dta_spoint <- dta_spoint[complete.cases(dta_spoint),]

dta_spoint$country <- droplevels(as.factor(dta_spoint$Pais), exclude = 0)
coordinates(dta_spoint) <-  ~Y+X

##Unificamos la proyeccion espacial
crs(dta_spoint) <- crs(wrld_simpl)
class(dta_spoint)

##Extraemos los datos de la capa de fondo
ovr <- over(dta_spoint, wrld_simpl)

##Unificamos los datos
dta1 <- dta[complete.cases(dta_spoint),]

xx <- cbind(dta1, filtro=ovr$NAME)



dtaF <- xx[which(xx$filtro=="Ecuador"),]


fig <- dtaF 

fig <- fig %>%
  plot_ly(
    lat = ~X,
    lon = ~Y,
    type = 'scattermapbox'
  ) 

fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =3.8,
      center = list(lon = -80, lat = -2),
      coloraxis = list(colorscale = "Rainbow")) )

fig








dta_spoint <- dta_spoint[dta_spoint$country==Ecuador,]
pais <- table(ovr$NAME)
pais <- pais[pais>0]
