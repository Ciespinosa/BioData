#source(file = "3._script/datos.R", encoding = "UTF-8")
source(file = "3._script/appCSS.R", encoding = "UTF-8")
#source("3._script/DataChart.R", encoding = "UTF-8")
#source("3._script/E_ecoregion.R", encoding = "UTF-8")
dta <- read.csv2("2._data/dta.csv")[,-1]
dta1 <- read.csv2("2._data/dta1.csv")[,-1]
dta1$Clase <- factor(dta1$Clase)
dta$Clase <- factor(dta$Clase)