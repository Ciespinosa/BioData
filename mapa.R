library(plotly)
library(readxl)

dta <- read_excel("2._data/Vertebrados Región Tumbesina.xlsx", sheet = "Ubicación")
clase <- read_excel("2._data/Vertebrados Región Tumbesina.xlsx", sheet = "Taxonomía")

dta <- aggregate(dta, list(codigo = dta$Código,
                                 orden = dta$Orden,
                                 Pais = dta$País,
                                 Provincia = dta$Provincia,
                                 Canton = dta$Cantón,
                                 Localidad = dta$Localidad), max)

dta <- merge(dta, clase, by = "Código", all.x = T)

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
    hovertext = dta["orden"],
    mode = 'markers',
    color = dta$Clase
    ) 

fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =3.8,
      center = list(lon = -80, lat = -2),
    coloraxis = list(colorscale = "Rainbow")) )

fig


plot_mapbox(dta,
  lat = ~X,
  lon = ~Y
) 
