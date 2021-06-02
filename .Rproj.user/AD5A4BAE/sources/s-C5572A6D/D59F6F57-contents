library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(reshape2)


library(googledrive)
library(googlesheets4)
library(googleAuthR)
library(googlesheets)



# deauth

source(file = "3._script/1.Datos.R", encoding = "UTF-8")


header <- dashboardHeader(
            title = "BioData")


sidebar <- dashboardSidebar(
                    sidebarMenu (
                      menuItem("Home",startExpanded = TRUE, icon = icon("home"),
                               href = "https://ciespinosa.github.io/BioData/"),
                      menuItem("DataMapas",startExpanded = TRUE, icon = icon("map-marked-alt"), 
                               tabName = "DataMapas"), 
                      menuItem("DataGráficos",startExpanded = TRUE, icon = icon("file-image"),
                               tabName = "DataGráficos"),
                      menuItem("DataConceptos",startExpanded = TRUE, icon = icon("archive"),
                               tabName = "DataConceptos"),
                      menuItem("DataDatos",startExpanded = TRUE, icon = icon("table"),
                               tabName = "DataDatos"),
                      menuItem("DataSum",startExpanded = TRUE, icon = icon("plus-square"),
                               tabName = "DataSum")
                      )
                    )


body <- dashboardBody(
  
    tabItems(
    #Mapas----
        tabItem(tabName = "DataMapas",
            box(width = 3, height = "500px",  title = "Selecciona los filtros:", solidHeader = TRUE,status="primary",
                
              selectInput(inputId = "map", label = strong("Tipo de mapa"),
                          choices = c("Ocurrencia", "Densidad"),
                          selected = "Ocurrencia"),
              
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
                               
                               h6("Explora los puntos y obtén información de cada uno de ellos")
                               ),
            
            box(title= "Concentracion de Estudios", solidHeader = TRUE,status="primary",
                width = 9, height = 950, 
                 plotlyOutput("distPlot", height = "430px"),
                footer = h6("La densidad es calculada como la acumulacion de estudios en un determinado sitio. 
                            Puedes usar las herramientas del gráfico para hacer zoom en una área específica")
            )
            ),
    
    ##Graficos ----
    
    tabItem(tabName = "DataGráficos",
            valueBoxOutput("progressBox",width = 2),
            valueBoxOutput("progressBox1",width = 2),
            valueBoxOutput("approvalBox",width = 2),
            valueBoxOutput("approvalBox2",width = 2),
            valueBoxOutput("approvalBox3",width = 2),
            valueBoxOutput("approvalBox4",width = 2),
            
           
              column(width = 4,
                    selectInput(inputId = "pais1", label = strong("Mostrar por País"),
                            choices = c("TOTAL", "Ecuador", "Peru"),
                            selected = "TOTAL")
                    ),
              column(width = 4,
                    selectInput(inputId = "eco1", label = strong("Mostrar por Ecoregión"),
                            choices = c("TOTAL", "Bosques secos", "Bosques lluviosos"),
                            selected = "TOTAL")
                    )
            
            ,
            
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
                               choices = c("TOTAL", "Ecuador", "Peru"),
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
            
            column(width = 3, 
                   h4(strong("Nota:")),
                   
                   h6("Usa los filtros para obtener las palabras más usadas en los artículos y
                      los autores con más trabajos")
                   )
              ),
           
           box(title = "Controles", solidHeader = TRUE,status="primary", width = 2,
               
               sliderInput("freq",
                           "Frecuencia:",
                           min = 1,  max = 40, value = 3),
               sliderInput("max",
                           "Número:",
                           min = 1,  max = 150,  value = 70),
               
               h4(strong("Nota:")),
               
               h6("Usa Frecuencia para limitar las palabras con esa frecuencia mínima.
                  Usa Número para limitar el número de palabras en el gráfico")
                  ),

            box(width = 5, height = 400, title = "Resumen",
                plotOutput("textPlot2", height = "330px")),
            
           box(title = "Autores",
                width = 5, 
                plotOutput("textPlot3", height = "330px"))

         ),
    
    #Datos ----
    
    tabItem(tabName = "DataDatos", 
                  
            h4('Revisar la información'),
                            DT::dataTableOutput('x1')
                      ),
             
    #Sumando datos ----      
  
    
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
                  h4("Muchas gracias, su aporte ha sido enviado con éxito!"),
                  h5("Puedes hacer un nuevo aporte haciendo clic en Nuevo envío"),
                  actionButton("ngo", "Nuevo envío", class = "btn-success", width = 220)
                )
              )
              ),
                      
                      DT::dataTableOutput('x2'),
                      
                      )
                     )
                   )

##UI ---

ui <- dashboardPage(header, sidebar, body,)

server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    #Puntos ----
    library(plotly)
    library(dplyr)
    

    
    if(input$type!="TOTAL"){
      dta <- dta %>% subset(Clase==input$type)}
    
    if(input$pais!="TOTAL"){
      
      dta <- dta[which(dta$Pais==input$pais),]
      
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
          radius = 2,
          height = 450)
      
    }
    
    if(input$map == "Ocurrencia"){
      
      fig <- dta 
      
      fig <- fig %>%
        plot_ly(
          lat = ~X,
          lon = ~Y,
          type = 'scattermapbox',
          text = paste("Código: ", dta$Codigo, 
                       "<br>Clase: ", dta$Clase,
                       "<br>Temática: ", dta$Tematica,
                       "<br>Bioma: ", dta$filtroE),
          hoverinfo = 'text',
          mode = 'markers',
          color = dta$Clase,
          height = 450
        ) 
    }
    
    fig <- fig %>%
      layout(
        mapbox = list(
          style = 'open-street-map',
          zoom =4.5,
          center = list(lon = -77, lat = -2.8)),
        coloraxis = list(colorscale = "Rainbow"))
    
    fig
    
  })

  
  ##Second chart----
  
  output$progressBox1 <- renderValueBox({
    
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 &Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
      valueBox(
      dfTot[1], h4("Aves"), icon = icon("crow"),
      color = "blue"
    )
  })
  
  output$approvalBox2 <- renderValueBox({
    
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 &Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    valueBox(
      dfTot[2], h4("Anfibios"), icon = icon("frog"),
      color = "olive"
    )
    
  })
  
  output$approvalBox <- renderValueBox({
    
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 &Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    valueBox(
      dfTot[3], h4("Mamíferos"), icon = icon("otter"),
      color = "light-blue"
    )
  })
  
  output$progressBox <- renderValueBox({
    
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 &Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    valueBox(
      dfTot[4], h4("Peces"), icon = icon("fish"),
      color = "aqua"
    )
  })
  
  output$approvalBox3 <- renderValueBox({
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 & Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    valueBox(
      dfTot[5], h4("Reptiles"), icon = icon("suse"),
      color = "yellow"
    )
    
  })
  
  output$approvalBox4 <- renderValueBox({
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 &Pais==input$pais1)
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
      dfTot <- colSums(dfT)
    }
    
    valueBox(
      sum(dfTot), h4("Total"), icon = icon("stopwatch-20"),
      color = "red"
    )
    
  })
  output$distPlot2 <- renderPlotly({

    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Pais==input$pais1)
      df <- as.data.frame.matrix(table(dta1$year,dta1$Clase))
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1)
      df <- as.data.frame.matrix(table(dta1$year,dta1$Clase))
    }
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1 <- dta1 %>% subset(Bioma==input$eco1 & Pais==input$pais1)
      df <- as.data.frame.matrix(table(dta1$year,dta1$Clase))
      }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      df <- as.data.frame.matrix(table(dta1$year,dta1$Clase))
      }
    
      fig1 <- plot_ly(df, x = ~rownames(df), y = ~AVES, type = 'bar', 
                      name = "Aves",
                      height = 310)
      fig1 <- fig1 %>% add_trace(y = ~AMPHIBIA, name = "Anfibios")
      fig1 <- fig1 %>% add_trace(y = ~MAMMALIA, name = "Mamíferos")
      fig1 <- fig1 %>% add_trace(y = ~PECES, name = "Peces")
      fig1 <- fig1 %>% add_trace(y = ~REPTILIA, name = "Reptiles")
      
      fig1 <- fig1 %>% layout(yaxis = list(title = 'Número', 
                                           tickfont = list(size = 8)), 
                              xaxis = list(title = "Año de Publicación", 
                                           tickfont = list(size = 8)),
                              legend = list(x = .05, y = .99,
                                            font = list(
                                family = "sans-serif",
                                size = 8),
                                title=list(text='<b>Grupo taxonómico </b>',
                                           font = list(size = 9))),
                              margin = list(l = 30, r = .20, b = 30, t = .10))
      fig1

  })
      output$distPlot3 <- renderPlotly({
        
        if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
          dta1 <- dta1 %>% subset(Pais==input$pais1)
          dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
         
        }
        
        if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
          dta1 <- dta1 %>% subset(Bioma==input$eco1)
          dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
        }
        if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
          dta1 <- dta1 %>% subset(Bioma==input$eco1 &Pais==input$pais1)
          dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
        }
        
        if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
          dfT <- as.data.frame.matrix(table(dta1$Tematica,dta1$Clase))
        }

        fig2 <- plot_ly(dfT, x = ~rownames(dfT), y = ~AVES, type = 'bar', 
                      name = "Aves",
                      height = 310)
        fig2 <- fig2 %>% add_trace(y = ~AMPHIBIA, name = "Anfibios")
        fig2 <- fig2 %>% add_trace(y = ~MAMMALIA, name = "Mamiferos")
        fig2 <- fig2 %>% add_trace(y = ~PECES, name = "Peces")
        fig2 <- fig2 %>% add_trace(y = ~REPTILIA, name = "Reptiles")
        fig2 <- fig2 %>% layout(yaxis = list(title = 'Número', 
                                             tickfont = list(size = 8)), 
                                xaxis = list(title = "", 
                                             tickfont = list(size = 8)),
                                legend = list(x = .8, y = .99,
                                              font = list(
                                                family = "sans-serif",
                                                size = 8),
                                              title=list(text='<b>Grupo taxonómico </b>',
                                                         font = list(size = 9))),
                                margin = list(l = 30, r = .20, b = 50, t = .10))
        fig2
    
    })
      
  
      
  # #Third chart -----

      output$textPlot2 <- renderPlot({

        if(input$eco2=="TOTAL" & input$pais2!="TOTAL" & input$type2=="TOTAL"){
          dta1.1 <- dta1 %>% subset(Pais==input$pais2)
        }

        if(input$eco2=="TOTAL" & input$pais2=="TOTAL"& input$type2!="TOTAL"){
          dta1.1 <- dta1%>% subset(Clase==input$type2)
        }

        if(input$eco2!="TOTAL" & input$pais2=="TOTAL" & input$type2=="TOTAL"){
          dta1.1 <- dta1 %>% subset(Bioma==input$eco2)
        }

        if(input$eco2!="TOTAL" & input$pais2!="TOTAL"& input$type2=="TOTAL"){
          dta1.1 <- dta1%>% subset(Bioma==input$eco2 & Pais==input$pais2)
          }

        if(input$eco2!="TOTAL" & input$pais2=="TOTAL" & input$type2!="TOTAL"){
          dta1.1 <- dta1 %>% subset(Bioma==input$eco2 &Clase==input$type2)
        }

        if(input$eco2=="TOTAL" & input$pais2!="TOTAL" & input$type2!="TOTAL"){
          dta1.1 <- dta1 %>% subset(Clase==input$type2 & Pais==input$pais2)
        }
        
        if(input$eco2!="TOTAL" & input$pais2!="TOTAL" & input$type2!="TOTAL"){
          dta1.1 <- dta1 %>% subset(Bioma==input$eco2 &Clase==input$type2 & Pais==input$pais2)
        }

        if(input$eco2=="TOTAL" & input$pais2=="TOTAL" & input$type2=="TOTAL"){
          dta1.1 <- dta1
        }


        library(tm)
        library(stringr)
        library(wordcloud)
        library(memoise)
        library(shiny)


        textR <- str_c(na.omit(dta1.1$Resumen), collapse = " ")
        textT <- str_c(na.omit(dta1.1$Titulo), collapse = " ")

        textRT <- str_c(textR, textT, collapse = " ")

        ptd <- PlainTextDocument(textRT,id = basename(tempfile()),
                                 language = "en")


        myText <- Corpus(VectorSource(ptd))
        myText = tm_map(myText, tolower)
        myText <- tm_map(myText, removePunctuation)
        myText = tm_map(myText, removeNumbers)
        myText = tm_map(myText, removeWords, stopwords("spanish"))

        myText = tm_map(myText, removeWords, stopwords("english"))
        myText = tm_map(myText, removeWords, c("two","first","perú",
                                               "ecuador", "peru", "aplica", "species"))

        myText <- tm_map(myText, PlainTextDocument)


        myDTM = TermDocumentMatrix(myText)


        m = as.matrix(myDTM)

        v = sort(rowSums(m), decreasing = TRUE)

        #install.packages('wordcloud')
        library(wordcloud)

        # Finalmente creamos la nube:

        par(mar=c(0,0,0,0))
        wordcloud_rep <- repeatable(wordcloud)
        wordcloud_rep(names(v), v, scale=c(2.3,0.1),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))


      })

      output$textPlot3 <- renderPlot({
        if(input$eco2=="TOTAL" & input$pais2!="TOTAL" & input$type2=="TOTAL"){
          dta1.1 <- dta1 %>% subset(Pais==input$pais2)
        }

        if(input$eco2=="TOTAL" & input$pais2=="TOTAL"& input$type2!="TOTAL"){
          dta1.1 <- dta1%>% subset(Clase==input$type2)
        }

        if(input$eco2!="TOTAL" & input$pais2=="TOTAL" & input$type2=="TOTAL"){
          dta1.1 <- dta1 %>% subset(Bioma==input$eco2)
        }

        if(input$eco2!="TOTAL" & input$pais2!="TOTAL"& input$type2=="TOTAL"){
          dta1.1 <- dta1%>% subset(Bioma==input$eco2 & Pais==input$pais2)
        }

        if(input$eco2!="TOTAL" & input$pais2=="TOTAL" & input$type2!="TOTAL"){
          dta1.1 <- dta1 %>% subset(Bioma==input$eco2 &Clase==input$type2)
        }

        if(input$eco2=="TOTAL" & input$pais2!="TOTAL" & input$type2!="TOTAL"){
          dta1.1 <- dta1 %>% subset(Clase==input$type2 & Pais==input$pais2)
        }
        
        if(input$eco2!="TOTAL" & input$pais2!="TOTAL" & input$type2!="TOTAL"){
          dta1.1 <- dta1 %>% subset(Bioma==input$eco2 &Clase==input$type2 & Pais==input$pais2)
        }
        
        if(input$eco2=="TOTAL" & input$pais2=="TOTAL" & input$type2=="TOTAL"){
          dta1.1 <- dta1
        }


        library(tm)
        library(stringr)
        library(wordcloud)
        library(memoise)
        library(shiny)

        textA <- str_c(na.omit(dta1.1$Autor), collapse = " ")
        Aptd <- PlainTextDocument(textA,id = basename(tempfile()),
                                  language = "en")


        myTextA <- Corpus(VectorSource(textA))
        myTextA = tm_map(myTextA, tolower)
        #myTextA <- tm_map(myTextA, removePunctuation)
        myTextA <- tm_map(myTextA, PlainTextDocument)


        myDTMA = TermDocumentMatrix(myTextA)


        mA = as.matrix(myDTMA)

        vA = sort(rowSums(mA), decreasing = TRUE)

        set.seed(25)

        # Finalmente creamos la nube:

        par(mar=c(0,0,0,0))
        wordcloud_rep <- repeatable(wordcloud)
        wordcloud_rep(names(vA), vA, scale=c(2.3,0.1),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))

      })

      ##Fourth chart----
      
      output$x1 <- DT::renderDataTable(
                              dta1[,c( "Codigo", "Autor", "year", "Titulo" )],
                              escape=FALSE, rownames = FALSE,
                              options = list(
                                pageLength = 5, autoWidth = TRUE,
                                columnDefs = list(list( targets = 1, 
                                                        width = '200px')),
                                                        scrollX = TRUE
                                              ),
                              class = "display"
            )
      
      
        ##Fifty chart ----
      
      observe({
        mandatoryFilled <-
          vapply(fieldsMandatory,
                 function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
                 },
                 logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        shinyjs::toggleState(id = "go", condition = mandatoryFilled)
      }) 
      
      fieldsAll <- c("Cita", "Autor(es)", "year", "Titulo", "DOI")
      
      formData <- reactive({
        data_to_add <- data.frame(Cita = input$cita, 'Autor(es)'= input$autor,
                                  year = input$anio, "Titulo" = input$titulo, 
                                  DOI = input$DOI)
        data_to_add
      })
      
      saveData <- function(data) {
        options(
          gargle_oauth_cache = ".secrets",
          gargle_oauth_email = "civan.espinosain@gmail.com")# run sheets auth
        
        sheets_auth()
        
        ss <- gs4_get("https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o/edit?usp=sharing") # do the authentication once, manually.
        
        data_to_write <- read_sheet('https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o')
        
        sheet_append(ss,
                     data)
        
      }
      
      observeEvent(input$go, {
        data_to_add <- formData()
        saveData(data_to_add)
        shinyjs::show("thankyou_msg")
      })
      
      observeEvent(input$go, {
        output$x2 <- DT::renderDataTable((formData()))
      })
      
      observeEvent(input$ngo, {
        shinyjs::reset("form")
        shinyjs::hide("thankyou_msg")
      })
  
}

shiny::shinyApp(ui, server)
