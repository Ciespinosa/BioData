#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

library(shiny)


source("3._script/datos.R")

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="https://ciespinosa.github.io/BioData/">BioData</a>'), id="nav",
               windowTitle = "BioData",
    # Application title
    tabPanel("DataMapas",
             div(class="outer",
                 tags$head(includeCSS("support/styles.css")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "map", label = strong("Tipo de mapa"),
                        choices = c("Ocurrencia", "Densidad"),
                        selected = "Ocurrencia"),
            
            selectInput(inputId = "type", label = strong("Mostrar por Clase"),
                        choices = c("TOTAL", unique(dta$Clase)),
                        selected = "TOTAL"),
            
            selectInput(inputId = "pais", label = strong("Mostrar por País"),
                        choices = c("TOTAL", "Ecuador", "Peru"),
                        selected = "TOTAL"),
            
            selectInput(inputId = "eco", label = strong("Mostrar por Ecoregión"),
                        choices = c("TOTAL", "Bosques secos", "Bosques lluviosos"),
                        selected = "TOTAL")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            h1(paste0("Concentracion de Estudios")),
            h2("En la Región Tumbesina"),
            plotlyOutput("distPlot")
        )
    )
)),
tabPanel("DataGráficos",
         div(class="outer",
             tags$head(includeCSS("support/styles.css")),
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "type", label = strong("Mostrar por Clase"),
                                 choices = c("TOTAL", unique(dta$Clase)),
                                 selected = "TOTAL"),
                     
                     selectInput(inputId = "pais", label = strong("Mostrar por País"),
                                 choices = c("TOTAL", "Ecuador", "Peru"),
                                 selected = "TOTAL"),
                     
                     selectInput(inputId = "eco", label = strong("Mostrar por Ecoregión"),
                                 choices = c("TOTAL", "Bosques secos", "Bosques lluviosos"),
                                 selected = "TOTAL")
                 ),
                 
                 
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     h1(paste0("Tendencias de los estudios")),
                     h2("En la Región Tumbesina"),
                     plotlyOutput("distPlot2")
                     
                     
                 )
             )
         )
        )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        #Puntos ----
        library(plotly)
        library(dplyr)
        
        source("3._script/E_ecoregion.R")
        
        if(input$type!="TOTAL"){
        dta <- dta %>% subset(Clase==input$type)}
        
        if(input$pais!="TOTAL"){
            
            dta <- dta[which(dta$filtroP==input$pais),]
            
        }
        
        if(input$eco=="Bosques secos"){

        dta <- dta[dta$Bioma =="Bosques secos",]
        }
        if(input$eco=="Bosques lluviosos"){

            dta <- dta[dta$Bioma =="Bosques lluviosos",]
        }
        
    if(input$map == "Densidad"){
        
            fig <- dta 
            fig <- fig %>%
                plot_ly(
                    type = 'densitymapbox',
                    lat = ~X,
                    lon = ~Y,
                    coloraxis = 'coloraxis',
                    radius = 2)
            
              }
        
        if(input$map == "Ocurrencia"){
            
            fig <- dta 
            
            fig <- fig %>%
                plot_ly(
                    lat = ~X,
                    lon = ~Y,
                    type = 'scattermapbox',
                    text = paste("Código: ", dta$codigo, 
                                  "<br>Clase: ", dta$Clase,
                                  "<br>Temática: ", dta$tematica,
                                  "<br>Bioma: ", dta$filtroE),
                    hoverinfo = 'text',
                    mode = 'markers',
                    color = dta$Clase
                        ) 
        }
        
            fig <- fig %>%
                layout(
                    mapbox = list(
                        style = 'open-street-map',
                        zoom =4,
                        center = list(lon = -80, lat = -2)),
                        coloraxis = list(colorscale = "Rainbow"))
            
            fig
            
    })
    
    output$distPlot2 <- renderPlotly({
        # generate bins based on input$bins from ui.R
        #Puntos ----
        library(plotly)
        library(dplyr)
        
        source("3._script/DataChart.R")
        
        source("3._script/E_clase.R")
        
        par(mfcol=c(1,5), mar=c(1,1,1,1))
        plot(x, axes=FALSE, type="n", xlab="", ylab="")
        rect(1,1,6,6,col =  "#337dff" )
        text(1,2,"Aves", cex=2, pos=4)
        text(1,4, dfTot[1], cex=3, pos=4)
        rasterImage(bird, 3.2,2,5,4)
        
        plot(x, axes=FALSE, type="n", xlab="", ylab="")
        rect(1,1,6,6,col = "#afee59")
        text(1,2,"Anfibios", cex=2, pos=4)
        text(1,4, dfTot[2], cex=3, pos=4)
        rasterImage(anfibio, 3.5,2.5,5,4.5)
        
        plot(x, axes=FALSE, type="n", xlab="", ylab="")
        rect(1,1,6,6,col = "#e5d8bd")
        text(1,2,"Mamíferos", cex=2, pos=4)
        text(1,4, dfTot[3], cex=3, pos=4)
        rasterImage(mamifero, 3,2.5,5,4)
        
        plot(x, axes=FALSE, type="n", xlab="", ylab="")
        rect(1,1,6,6,col = "#3f84e3")
        text(1,2,"Peces", cex=2, pos=4)
        text(1,4, dfTot[4], cex=3, pos=4)
        rasterImage(pez, 3,2.5,5,4)
        
        plot(x, axes=FALSE, type="n", xlab="", ylab="")
        rect(1,1,6,6,col = "grey")
        text(1,2,"Reptiles", cex=2, pos=4)
        text(1,4, dfTot[5], cex=3, pos=4)
        rasterImage(reptil, 3,2.3,5,4.9)
        
        
        fig <- plot_ly(df, x = ~year, y = ~AVES, type = 'bar', name = colnames(df[2]))
        fig <- fig %>% add_trace(y = ~AMPHIBIA, name = colnames(df[3]))
        fig <- fig %>% add_trace(y = ~MAMMALIA, name = colnames(df[4]))
        fig <- fig %>% add_trace(y = ~PECES, name = colnames(df[5]))
        fig <- fig %>% add_trace(y = ~REPTILIA, name = colnames(df[6]))
        
        fig <- fig %>% layout(yaxis = list(title = 'Número'), 
                              xaxis = list(title = "Año de Publicación"), barmode = 'group')
        
        fig
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
