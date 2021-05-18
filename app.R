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

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">BioData</a>'), id="nav",
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
            
            selectInput(inputId = "type", label = strong("Clase"),
                        choices = c("TOTAL",unique(dta$Clase)),
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
tabPanel("DataMapas2",
         div(class="outer",
             tags$head(includeCSS("support/styles.css")),
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "map", label = strong("Tipo de mapa"),
                                 choices = c("Ocurrencia", "Densidad"),
                                 selected = "Ocurrencia"),
                     
                     selectInput(inputId = "type", label = strong("Clase"),
                                 choices = c("TOTAL",unique(dta$Clase)),
                                 selected = "TOTAL")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   
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
        library(readxl)
        
        dta <- read_excel("2._data/Vertebrados Región Tumbesina.xlsx", sheet = "Ubicación")
        clase <- read_excel("2._data/Vertebrados Región Tumbesina.xlsx", sheet = "Taxonomía")
        dta <- merge(dta, clase, by = "Código", all.x = T)
        
        dta <- aggregate(dta, list(codigo = dta$Código,
                                   orden = dta$Orden,
                                   Pais = dta$País,
                                   Provincia = dta$Provincia,
                                   Canton = dta$Cantón,
                                   Localidad = dta$Localidad,
                                   Clase = dta$Clase), max)
        
        
        
        library(dplyr)
        
        if(input$type!="TOTAL"){
        dta <- dta %>% subset(Clase==input$type)}
        
        
        
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
                    hovertext = dta["orden"],
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
}

# Run the application 
shinyApp(ui = ui, server = server)
