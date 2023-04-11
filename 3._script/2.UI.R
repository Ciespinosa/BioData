############################################################################
# Preparación de la salida de la plataforma
# Descripción:
#               Este Script contiene la información de estructura de la plataforma.
#               Se establece la cabecera y las diferentes partes que estructuran la plataforma
# Versión 2
# Autor: Carlos Iván Espinosa
# Fecha: 04/04/2023
# 
################################################################################



library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(googlesheets4)
library(plotly)
library(reshape2)
library(leaflet)




# Cargamos los datos----

source(file = "3._script/1.Datos.R", encoding = "UTF-8")

#Generamos la cabecera

header <- dashboardHeader(
  title = "BioData")

# Defino el menu

sidebar <- dashboardSidebar(
  sidebarMenu (
    menuItem("Inicio",startExpanded = TRUE, icon = icon("home",verify_fa = FALSE),
             href = "https://bosquesecocienciaygestion.cedia.edu.ec/"),
    menuItem("BioData",startExpanded = TRUE, icon = icon("list-alt",verify_fa = FALSE),
             tabName = "BioData"),
    menuItem("DataMapas",startExpanded = TRUE, icon = icon("map-marked-alt",verify_fa = FALSE), 
             tabName = "DataMapas"), 
    menuItem("DataGráficos",startExpanded = TRUE, icon = icon("file-image",verify_fa = FALSE),
             tabName = "DataGráficos"),
    menuItem("DataConceptos",startExpanded = TRUE, icon = icon("archive",verify_fa = FALSE),
             tabName = "DataConceptos"),
    menuItem("DataDatos",startExpanded = TRUE, icon = icon("table",verify_fa = FALSE),
             tabName = "DataDatos"),
    menuItem("DataSum",startExpanded = TRUE, icon = icon("plus-square",verify_fa = FALSE),
             tabName = "DataSum")
  )
)


#Desarrollo la estructura del cuerpo de la plataforma

body <- dashboardBody(
  
  tabItems(
    #BioData----
    tabItem(tabName = "BioData",
            box(title= "Prólogo", solidHeader = TRUE,status="primary",
                width = 12, height = 950, 
                HTML("<b>","BioData","</b>"),HTML("es una plataforma diseñada para 
                  poner a disposición de los tomadores de decisión información científica 
                  rigurosa y actualizada sobre los bosques tropicales estacionalmente secos
                  (BTES) y sus zonas de influencia. Nuestro objetivo es que BioData se convierta
                  en una herramienta indispensable para los profesionales que necesitan 
                  tomar decisiones informadas en relación al manejo y conservación de los BTES.
             
<br>
<br>

A través de BioData, los usuarios tendrán acceso a una amplia selección de artículos
científicos, ordenados y sistematizados de manera intuitiva y fácil de usar. Además, 
BioData también proporcionará información sobre los vacíos de investigación existentes en 
los BTES, tanto en términos de temas como de ubicaciones geográficas.

<br>
<br>
Nuestra visión es que BioData se convierta en un catalizador para la investigación 
en los BTES y en una plataforma colaborativa para la comunidad científica. Esperamos 
que BioData contribuya a mejorar la toma de decisiones en el manejo de los BTES, 
ayudando a preservar estos ecosistemas cruciales para la biodiversidad y el bienestar humano."
                )
            ),
            
            box(title= "Estructura", solidHeader = TRUE,status="primary",
                width = 12,  
                "BioData está estructurado en 5 secciones que permiten explorar
    la información científica producida en la región de los bosques tropicales 
    estacionalmente secos (BTES). Si eres un investigador que está trabajando en 
    los BTES, BioData te brinda la oportunidad de contribuir a la plataforma 
    subiendo la información y los resultados de tus investigaciones. 
    Esto permitirá que otros investigadores, tomadores de decisiones y público 
    en general tengan acceso a tus hallazgos y contribuyan a ampliar el conocimiento
    científico sobre los BTES."
            ),
            
            box( id = "foo", title = NULL, headerBorder = FALSE,  width = 12, height = 5,
                 column(width = 2,  height = 5,
                        imageOutput("image1", height = "50px")),
                 
                 column(width = 10, height = 5,
                        HTML("<b>","DataMapas;","</b>"),"nos permite analizar la distribución 
              espacial de estudios científicos y realiza búsquedas específicas sobre
              grupos biológicos y formaciones vegetales en los BTES.")
                 
                 
            ),
            box( id = "foo", title = NULL, headerBorder = FALSE,  width = 12, height = 50,
                 column(width = 2,  
                        imageOutput("image2", height = "50px")),
                 
                 column(width = 10, 
                        HTML("<b>","DataGráficos;","</b>"), "en esta sección podrás analizar las
            tendencias temporales de la producción científica. Además, podrás evaluar 
            que temáticas son abordadas dentro de cada grupo.")
                 
                 
            ),
            
            box( id = "foo", title = NULL, headerBorder = FALSE,  width = 12, height = 50,
                 column(width = 2,  
                        imageOutput("image3", height = "50px")),
                 
                 column(width = 10, 
                        HTML("<b>","DataConceptos;","</b>"), "en esta sección hemos organizado la información para evaluar las 
    palabras más frecuentes usadas en los artículos y los autores con mayor producción
    científica. Además podrás analizar las redes de colaboración entre los diferentes autores, 
   permitiendo identificar los grupos de investigación activos.",
                 )
                 
                 
            ),
            box( id = "foo", title = NULL, headerBorder = FALSE,  width = 12, height = 50,
                 column(width = 2,  
                        imageOutput("image4", height = "50px")),
                 
                 column(width = 10, 
                        HTML("<b>","DataDatos;","</b>"), "en esta sección podrás obtener información 
            de los artículos que se han 
    producido o podrás verificar si tus artículos se encuentran subidos a la base de datos.",
                 )
                 
                 
            ),
            box( id = "foo", title = NULL, headerBorder = FALSE,  width = 12, height = 50,
                 column(width = 2,  
                        imageOutput("image5", height = "50px")),
                 
                 column(width = 10, 
                        HTML("<b>","DataSum;","</b>"), "en esta sección podrás aportar a la base de datos, si consideras que hay 
    artículos que no se han subido a la base de datos puedes ayudarnos a subir información
    que será verificada y complementada.",
                 )
                 
                 
            ),
            tags$head(tags$style('#foo .box-header{ display: none}; .DISABLED { min-height: 100vh !important}'))
    ),
    
    ##Mapas----
    tabItem(tabName = "DataMapas",
            box(width = 3, height = "500px",  title = "Selecciona los filtros:", solidHeader = TRUE,status="primary",
                
                selectInput(inputId = "map", label = strong("Tipo de mapa"),
                            choices = c("Ocurrencia", "Densidad"),
                            selected = "Densidad"),
                
                selectInput(inputId = "type", label = strong("Mostrar por Clase"),
                            choices = c("TOTAL", as.character(unique(dta$Clase))),
                            selected = "TOTAL"),
                
                selectInput(inputId = "pais", label = strong("Mostrar por País"),
                            choices = c("TOTAL", "Ecuador", "Peru"),
                            selected = "TOTAL"),
                
                selectInput(inputId = "eco", label = strong("Mostrar por Ecoregión"),
                            choices = c("TOTAL", "Bosques secos", "Bosques lluviosos"),
                            selected = "TOTAL"),
                
                
                br(),
                br(),
                br(),
                
                h4(strong("Nota:")),
                
                h6("Puedes usar los filtros para hacer énfasis en aquella información que es de interés."),
                
                h6("Explora los puntos y obtén información de cada uno de ellos, muchas veces el mismo estudio tiene muchos puntos de evaluación ")
            ),
            
            box(title= "Concentracion de Estudios", solidHeader = TRUE,status="primary",
                width = 9, height = 950, 
                leafletOutput("distPlot", height = "430px"),
                footer = h6("La densidad es calculada como la acumulacion de puntos de muestreo dentro de los estudios en un determinado sitio. 
                            Puedes usar las herramientas del gráfico para hacer zoom en una Área específica")
            )
    ),
    
    ##Graficos ----
    
    tabItem(tabName = "DataGráficos",
            valueBoxOutput("progressBox",width = 2),
            valueBoxOutput("progressBox1",width = 2),
            valueBoxOutput("progressBox2",width = 2),
            valueBoxOutput("progressBox3",width = 2),
            valueBoxOutput("progressBox4",width = 2),
            valueBoxOutput("progressBox5",width = 2),
            
            column(width = 4,
                   selectInput(inputId = "pais1", label = strong("Mostrar por País"),
                               choices = c("TOTAL", "Ecuador", "Perú"),
                               selected = "TOTAL")
            ),
            column(width = 4,
                   selectInput(inputId = "eco1", label = strong("Mostrar por Ecoregión"),
                               choices = c("TOTAL", "Bosques secos", "Bosques lluviosos"),
                               selected = "TOTAL")
            ),
            
            box(width = 6, height = 240,
                plotlyOutput("distPlot2", height = "310px")),
            box(width = 6, height = 340,
                plotlyOutput("distPlot3", height = "310px"))
            
    ),
    
    ##Conceptos----
    
    tabItem(tabName = "DataConceptos",
            
            box(title = "Selecciona los filtros:", solidHeader = TRUE,status="primary", width = 12,
                column(width = 3,
                       selectInput(inputId = "pais2", label = strong("Mostrar por País"),
                                   choices = c("TOTAL", "Ecuador", "Perú"),
                                   selected = "TOTAL")
                ),
                column(width = 3,
                       selectInput(inputId = "eco2", label = strong("Mostrar por Ecoregión"),
                                   choices = c("TOTAL", "Bosques secos", "Bosques lluviosos"),
                                   selected = "TOTAL")
                ),
                column(width = 3,
                       selectInput(inputId = "type2", label = strong("Mostrar por Clase"),
                                   choices = c("TOTAL", as.character(unique(dta$Clase))),
                                   selected = "TOTAL")
                ),
                
                column(width = 1,
                       radioButtons(inputId = "red", label = "Redes", choices = c("On", "Off"),
                                    selected = "Off" )),
                column(width = 2,       
                       h5(strong("Nota:")),
                       h6("Selecciona 'on' para ver las redes de autores")
                )
            ),
            
            box(solidHeader = TRUE,status="primary", width = 3,
                
                sliderInput("freq",
                            "Frecuencia:",
                            min = 1,  max = 40, value = 3),
                sliderInput("max",
                            "Número:",
                            min = 1,  max = 150,  value = 70),
                selectInput("aut", "Autores", 
                            choices = seq(5,25, by= 5), selected = 5),
                
                h6(strong("Nota:")),
                
                h6("Usa Frecuencia para limitar las palabras con esa frecuencia mínima.
                  Usa Número para limitar el número de palabras en el gráfico. Usa Autores 
                  para cambiar el filtro de autores en la red", style = "font-size:9px;")
            ),
            
            box(width = 4, height = 400, title = "Resumen",
                plotOutput("textPlot2", height = "330px")),
            
            box(title = "Autores",
                width = 5, 
                plotOutput("textPlot3", height = "330px"))
            
    ),
    
    ##Datos ----
    
    tabItem(tabName = "DataDatos",
            
            h4('Revisar la información'),
            DT::dataTableOutput('x1')
    ),
    ##Sumando datos ----      
    
    
    tabItem(tabName = "DataSum",
            shinyjs::useShinyjs(),
            shinyjs::inlineCSS(appCSS),
            div( id= "form",
                 box(title= "Llenar los campos", width = 6,
                     
                     textInput("cita", labelMandatory("Incluya la cita completa"),
                               value = ""),
                     textInput("autor", ("Listado de Autores"),
                               value = ""),
                     textInput("anio", ("Ingrese el año de publicación"),
                               value = " "),
                     textInput("titulo", ("Ingrese el título del artículo"),
                               value = ""),
                     textInput("DOI", labelMandatory("Ingrese la DOI del artículo"),
                               value = " "),
                     actionButton("go", "Enviar", class = "btn-primary")
                 ),
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h4("Muchas gracias, su aporte ha sido enviado con", strong("Éxito!")),
                     h5("Puedes hacer un nuevo aporte haciendo clic en Nuevo envío"),
                     actionButton("ngo", "Nuevo envío", class = "btn-success", width = 220)
                   )
                 )
            ),
            
            DT::dataTableOutput('x2')
            
    )
    
    
  )
)
