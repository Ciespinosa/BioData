library(readxl)

dta <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Ubicacion")
clase <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Taxonomia")
tipo <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Bibliografia")

clase <- unique(clase[,c("Codigo", "Clase")])
dta <- merge(dta, clase[c("Codigo", "Clase")], by = "Codigo", all.x = T)
dta <- merge(dta, tipo[c("Codigo","Tematica")], by = "Codigo", all.x = T)

dta <- aggregate(dta, list(codigo = dta$Codigo,
                           Pais = dta$Pais,
                           Provincia = dta$Provincia,
                           Canton = dta$Canton,
                           Localidad = dta$Localidad,
                           Clase = dta$Clase,
                           tematica = dta$Tematica), max)

#dta <- dta[complete.cases(dta[,1:15]),]


