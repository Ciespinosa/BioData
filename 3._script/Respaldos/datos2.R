library(readxl)
library(dplyr)

Ubica <- read_excel("2._data/dta.31.3.23.xlsx", sheet = "Ubicacion")
clase <- read_excel("2._data/dta.31.3.23.xlsx", sheet = "Taxonomia")
tipo <- read_excel("2._data/dta.31.3.23.xlsx", sheet = "Bibliografia")


#Preparamos los datos

clase <- unique(clase[,c("Codigo", "Clase")])

Ubica <- unique(Ubica[, c("Codigo", "X","Y", "Pais")])


#Datos de bibliografía

tipo <- left_join(tipo %>% 
                    filter(Tipo== "Artículo"),
                  clase, by = "Codigo")


tipo$Tematica <- factor(tipo$Tematica)

levels(tipo$Tematica) <- list(Distribución = "Biogeografía y distribución",
                              Conservación = c("Conservación", "Crónicas"),
                              Diversidad = "Diversidad",
                              Ecología = c("Ecología", "General", "Especies introducidas y parásitos", "Paleontología" ),
                              H.Natural = "Historia natural",
                              Manejo = c("Manejo y crianza en cautiverio", "Etnozoología y cacería"),
                              Educación = "Educación y difusión",
                              Sistemática = "Sistemática")
tipo$Clase <- factor(tipo$Clase)

tipo <- merge(tipo, unique(Ubica[, c("Codigo", "Pais")]) ,by = "Codigo", all.x = T)


##Datos de ubicación

dta1 <- merge(Ubica, clase[c("Codigo", "Clase")], by = "Codigo", all.x = T)


