library(readxl)
dtA <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Ubicacion")
clase <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Taxonomia")
tipo <- read_excel("2._data/VertebradosRT.xlsx", sheet = "Bibliografia")

clase <- unique(clase[,c("Codigo", "Clase")])

dta1 <- aggregate(dtA[, c( "X","Y", "Pais")], by=list(Codigo = dtA$Codigo), max)

clase <- unique(clase[,c("Codigo", "Clase")])
dta1 <- merge(dta1, clase[c("Codigo", "Clase")], by = "Codigo", all.x = T)
dta1 <- merge(dta1, tipo, by = "Codigo", all.x = T)

#dta1 <- dta1[complete.cases(dta1[,c("X","Y")]),]

dta1$Tematica <- factor(dta1$Tematica)

levels(dta1$Tematica) <- list(Distribución = "Biogeografía y distribución",
                           Conservación = c("Conservación", "Crónicas"),
                           Diversidad = "Diversidad",
                           Ecología = c("Ecología", "General", "Especies introducidas y parásitos", "Paleontología" ),
                           H.Natural = "Historia natural",
                           Manejo = c("Manejo y crianza en cautiverio", "Etnozoología y cacería"),
                           Educación = "Educación y difusión",
                           Sistemática = "Sistemática")
dta1$Clase <- factor(dta1$Clase)
