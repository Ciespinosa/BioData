
############################################################################
# Recursividad
# Descripción:
#               Este Script contiene la salida de la plataforma y define la 
#               recursividad. El script esta separado por carpetas que sería 
#               cada una de las secciones de la plataforma.
# Versión 2
# Autor: Carlos Iván Espinosa
# Fecha: 04/04/2023
# 
################################################################################


server <- function(input, output) {
  

  # Carpeta 1: BioData ----
  
  ## Obtenemos las imagenes para cada sección
  
    output$image1 <- renderImage({
                          ImgTxt <- "1._images/imagen1.png"
                          width<- "100%"
                          height<- "100%"
                          list(src = ImgTxt,
                               contentType = "image/png",
                               width = "auto",
                               height = height
                          )
                        }, deleteFile = FALSE)
  
  output$image2 <- renderImage({
                      ImgTxt2 <- "1._images/imagen2.png"
                      width<- "100%"
                      height<- "100%"
                      list(src = ImgTxt2,
                           contentType = "image/png",
                           width = "auto",
                           height = height
                      )
                    }, deleteFile = FALSE)
  
  output$image3 <- renderImage({
                      ImgTxt3 <- "1._images/imagen3.png"
                      width<- "100%"
                      height<- "100%"
                      list(src = ImgTxt3,
                           contentType = "image/png",
                           width = "auto",
                           height = height
                      )
                    }, deleteFile = FALSE)
  
  output$image4 <- renderImage({
                      ImgTxt4 <- "1._images/imagen4.jpg"
                      width<- "100%"
                      height<- "100%"
                      list(src = ImgTxt4,
                           contentType = "image/jpg",
                           width = "auto",
                           height = height
                      )
                    }, deleteFile = FALSE)
  
  
  output$image5 <- renderImage({
                      ImgTxt5 <- "1._images/imagen5.png"
                      width<- "100%"
                      height<- "100%"
                      list(src = ImgTxt5,
                           contentType = "image/png",
                           width = "auto",
                           height = height
                      )
                    }, deleteFile = FALSE)
  
  output$distPlot <- renderLeaflet({

    #Carpeta 2: Mapas ----
    
    library(leaflet)
    library(dplyr)
    
    # Definimos la interactividad

    
    ##Filtramos los datos según ----
    
    # grupo biológico    
    
    if(input$type!="TOTAL"){
      dta1 <- dta1 %>% subset(Clase==input$type)}
    
    # País
    
    if(input$pais!="TOTAL"){
      
      dta1 <- dta1[which(dta1$Pais==input$pais),]
      
    }
    
    #ecosistema
    
    if(input$eco=="Bosques secos"){
      
      dta1 <- dta1[dta1$Bioma =="Bosques secos",]
    }
    
    
    if(input$eco=="Bosques lluviosos"){
      
      dta1 <- dta1[dta1$Bioma =="Bosques lluviosos",]
    }
    
    ## Mapa Densidad ----
    
    if(input$map == "Densidad"){
      
      dta11 <- sp::SpatialPointsDataFrame(coords = dta1[,c("Y", "X")], data = dta1[,-(2:3)])
      
      #establecemos la salida en los tags de los iconos
      
        content <- paste(sep = "<br/>",
                       paste0("<b>", dta11$Clase, "</b>"),
                       dta11$Codigo, 
                       dta11$Bioma)
      
      # Generamos una función para poner colores según la clase
      
        
      getColor <- function(x, c) {
                        cla <- levels(c)
                        x$col <- dta1$Clase
                        
                        levels(x$col) <- c("blue", "green", "gray",
                                           "lightblue","orange","darkgreen")
                        return(x$col)
                      }
      
      
      #Definimos los iconos a mostrar
      
      icons <- awesomeIcons(
                    icon = 'ios-close',
                    iconColor = 'black',
                    library = 'ion',
                    markerColor = getColor(dta1, dta1$Clase)
                  )
      
      
      ##Generamos el mapa
      fig <-  leaflet(dta11) %>%
        addTiles() %>% addAwesomeMarkers(icon = icons,
                                         popup = content,
                                         clusterOptions = markerClusterOptions()
        )%>% 
        setView(lng=mean(dta1$Y), lat=mean(dta1$X) , zoom = 5.5)
      
    }
    
    
    ## Mapa de ocurrencia ----
    
    if(input$map == "Ocurrencia"){
      
      dta11 <- sp::SpatialPointsDataFrame(coords = dta1[,c("Y", "X")], data = dta1[,-(2:3)])
      
      content <- paste(sep = "<br/>",
                       paste0("<b>", dta11$Clase, "</b>"),
                       dta11$Codigo, 
                       dta11$Bioma)
      
      pal <- colorFactor(c("#55a9d9", "#afcf99", "#b09460",
                           "#7fcdff","#d6c8ac","#096f11"), domain = levels(dta1$Clase), alpha = 0.5)
      
      
      fig <- leaflet(dta11) %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircleMarkers(radius = 3,
                         color = ~pal(Clase),
                         stroke = FALSE, fillOpacity = 0.8,
                         popup = content
        ) %>% 
        setView(lng=mean(dta1$Y), lat=mean(dta1$X) , zoom = 5.5)
      
      
    }
    
    fig 
  })
  
  
  
  ##Carpeta 3: Graficos ----
  
  source("3._script/4. funcionFiltrar.R", encoding = "UTF-8")
  
  dta.filtrados <- reactive({
    filtrar(pais = input$pais1, ecos = input$eco1)
  })
  
  output$progressBox <- renderValueBox({
    
    valueBox(
      dta.filtrados()["AVES"], h4("Aves"), icon = icon("crow"),
      color = "blue"
    )
  })
  
  output$progressBox1 <- renderValueBox({
    
    valueBox(
      dta.filtrados()["AMPHIBIA"], h4("Anfibios"), icon = icon("frog"),
      color = "olive"
    )
    
  })
  
  output$progressBox2 <- renderValueBox({
        valueBox(
          dta.filtrados()["MAMMALIA"], h4("Mamíferos"), icon = icon("otter"),
              color = "light-blue"
    )
  })
  
  output$progressBox3 <- renderValueBox({
    
    valueBox(
      dta.filtrados()["PECES"], h4("Peces"), icon = icon("fish"),
      color = "aqua"
    )
  })
  
  output$progressBox4 <- renderValueBox({
   
    valueBox(
      dta.filtrados()["REPTILIA"], h4("Reptiles"), icon = icon("suse"),
      color = "yellow"
    )
    
  })
  
  output$progressBox5 <- renderValueBox({
   
    valueBox(
      dta.filtrados()["PLANTAE"], h4("Plantas"), icon = icon("seedling"),
      color = "green"
    )
    
  })
  
  
  output$distPlot2 <- renderPlotly({
    dta$Clase <- as.factor(dta$Clase)
    if(input$eco1=="TOTAL" & input$pais1!="TOTAL"){
      dfT <- dta %>% subset(Pais==input$pais1) %>% 
        dcast(Clase~year)
      
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dfT <- dta %>% 
        subset(Bioma==input$eco1) %>% 
        dcast(Clase~year) 
    }
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dfT <- dta %>% 
        subset(Bioma==input$eco1 &Pais==input$pais1) %>% 
        dcast(Clase~year) 
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT <- dta %>% 
        dcast(Clase~year)
    }
    
    dfTt <- as.data.frame(t(dfT[,-1]))
    colnames(dfTt) <- dfT[,1]
    
    fig1 <- plot_ly(dfTt, x = ~rownames(dfTt), y = ~AVES, type = 'bar',
                    name = "Aves",
                    height = 310)
    ifelse(colSums(dfTt)["AMPHIBIA"]>0,
           fig1 <- fig1 %>% add_trace(y = ~AMPHIBIA, name = "Anfibios"),
           fig1 <- fig1)
    
    ifelse(colSums(dfTt)["MAMMALIA"]>0,
           fig1 <- fig1 %>% add_trace(y = ~MAMMALIA, name = "Mamíferos"),
           fig1 <- fig1)
    
    ifelse(colSums(dfTt)["PECES"]>0,
           fig1 <- fig1 %>% add_trace(y = ~PECES, name = "Peces"),
           fig1 <- fig1)
    
    ifelse(colSums(dfTt)["REPTILIA"]>0,
           fig1 <- fig1 %>% add_trace(y = ~REPTILIA, name = "Reptiles"),
           fig1 <- fig1)
    
    ifelse(colSums(dfTt)["PLANTAE"]>0,
           fig1 <- fig1 %>% add_trace(y = ~PLANTAE, name = "Plantas"),
           fig1 <- fig1)
    
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
      dta1.1 <- dta %>% subset(Pais==input$pais1)
      dfT1 <- dcast(dta1.1, Tematica~Clase)[-9,]
      
    }
    
    if(input$eco1!="TOTAL" & input$pais1=="TOTAL"){
      dta1.1 <- dta %>% subset(Bioma==input$eco1)
      dfT1 <- dcast(dta1.1, Tematica~Clase)[-9,]
    }
    if(input$eco1!="TOTAL" & input$pais1!="TOTAL"){
      dta1.1 <- dta %>% subset(Bioma==input$eco1 &Pais==input$pais1)
      dfT1 <- dcast(dta1.1, Tematica~Clase)[-9,]
    }
    
    if(input$eco1=="TOTAL" & input$pais1=="TOTAL"){
      dfT1 <- dcast(dta, Tematica~Clase)[-9,]
    }
    
    fig2 <- plot_ly(dfT1, x = ~Tematica , y = ~AVES, type = 'bar',
                    name = "Aves",
                    height = 310)
    ifelse(colSums(dfT1[,-1])["AMPHIBIA"]>0,
           fig2 <- fig2 %>% add_trace(y = ~AMPHIBIA, name = "Anfibios"),
           fig2 <- fig2)
    ifelse(colSums(dfT1[,-1])["MAMMALIA"]>0,
           fig2 <- fig2 %>% add_trace(y = ~MAMMALIA, name = "Mamíferos"),
           fig2 <- fig2)
    ifelse(colSums(dfT1[,-1])["PECES"]>0,
           fig2 <- fig2 %>% add_trace(y = ~PECES, name = "Peces"),
           fig2 <- fig2)
    ifelse(colSums(dfT1[,-1])["REPTILIA"]>0,
           fig2 <- fig2 %>% add_trace(y = ~REPTILIA, name = "Reptiles"),
           fig2 <- fig2)
    ifelse(colSums(dfT1[,-1])["PLANTAE"]>0,
           fig2 <- fig2 %>% add_trace(y = ~PLANTAE, name = "Plantas"),
           fig2 <- fig2)
    
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
  
  
  
  # Carpeta 4: Conceptos -----
  # 
  
  ##Filtramos los datos
  
  
  dta.fil2 <- reactive({
    filt2(pais = input$pais1, ecos = input$eco1, type = input$type2)
  })
  
  
  output$textPlot2 <- renderPlot({
    
   
    
    library(tm)
    library(stringr)
    library(wordcloud)
    library(memoise)
    library(shiny)
    
    
    textR <- str_c(na.omit(dta.fil2()$Resumen), collapse = " ")
    textT <- str_c(na.omit(dta.fil2()$Titulo), collapse = " ")
    
    textRT <- str_c(textR, textT, collapse = " ")
    
    ptd <- PlainTextDocument(textRT,id = basename(tempfile()))
    
    
    myText <- Corpus(VectorSource(ptd))
    myText = tm_map(myText, tolower)
    myText <- tm_map(myText, removePunctuation)
    myText = tm_map(myText, removeNumbers)
    myText = tm_map(myText, removeWords, stopwords("spanish"))
    myText <- tm_map(myText, stemDocument)
    myText = tm_map(myText, removeWords, stopwords("english"))
    myText = tm_map(myText, removeWords, c("two","first","perú",
                                           "ecuador", "peru", "aplica", "species", "speci"))
    
    myText <- tm_map(myText, PlainTextDocument)
    
    
    myDTM = TermDocumentMatrix(myText)
    
    
    m = as.matrix(myDTM)
    
    v = sort(rowSums(m), decreasing = TRUE)
    
    
    source(file = "3._script/Respaldos/aggregate.R", encoding = "UTF-8")
    
    v1 <- aggregate.plurals(v)
    
    #install.packages('wordcloud')
    library(wordcloud)
    
    # Finalmente creamos la nube:
    
    par(mar=c(0,0,0,0))
    wordcloud_rep <- repeatable(wordcloud)
    wordcloud_rep(names(v1), v1, scale=c(2.3,0.1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
    
    
  })
  
  output$textPlot3 <- renderPlot({

    
    if(input$red == "Off"){
      library(tm)
      library(stringr)
      library(wordcloud)
      library(memoise)
      library(shiny)
      library(tidyr)
      library(stringi)
      library(dplyr)
      
      autor <- dta.fil2() %>%
        distinct(Codigo, Autor) %>% 
        separate("Autor", into = paste0("autor", 1:20), ";") 
      
      
      for(i in 2:ncol(autor)){
        autor[,i] <- stri_trim(autor[,i])
        autor[,i] <- gsub(" ", "_", autor[,i])
        autor[,i] <- stri_replace_all_charclass(autor[,i],"[.]", "")
        autor[,i] <- stri_replace_all_charclass(autor[,i],"[-]", "_")
      }            
      
      autor <- autor %>% 
        unite("autores", autor1:autor20,sep = " ", na.rm = TRUE, remove = FALSE)
      
      textA <- str_c(autor$autores, collapse = " ")
      myTextA <- Corpus(VectorSource(textA))
      myDTMA <- DocumentTermMatrix(myTextA, control=list(tolower=FALSE))
      myDTMA <- tm::TermDocumentMatrix(myTextA, control=list(tolower=FALSE))
      
      
      mA = as.matrix(myDTMA)
      
      vA = sort(rowSums(mA), decreasing = TRUE)
      
      set.seed(25)
      
      # Finalmente creamos la nube:
      
      # par(mar=c(0,0,0,0))
      # wordcloud_rep <- repeatable(wordcloud)
      # wordcloud_rep(names(vA), vA, scale=c(2.3,0.1),
      #               min.freq = 4, max.words=70,
      #               colors=brewer.pal(8, "Dark2"))
      
      par(mar=c(0,0,0,0))
      wordcloud_rep <- repeatable(wordcloud)
      wordcloud_rep(names(vA), vA, scale=c(2.3,0.1),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"))
      
      
    }      
    
    if(input$red=="On"){
      library(reshape2)
      library(igraph)
      library(tnet)
      library(tm)
      library(stringr)
      library(wordcloud)
      library(memoise)
      library(shiny)
      library(tidyr)
      library(stringi)
      library(dplyr)
      library(ggraph)
      
      
      autor <- dta.fil2() %>%
        distinct(Codigo, Autor) %>% 
        separate("Autor", into = paste0("autor", 1:20), ";") 
      
      
      for(i in 2:ncol(autor)){
        autor[,i] <- stri_trim(autor[,i])
        autor[,i] <- gsub(" ", "_", autor[,i])
        autor[,i] <- stri_replace_all_charclass(autor[,i],"[.]", "")
        autor[,i] <- stri_replace_all_charclass(autor[,i],"[-]", "_")
      } 
      
      autor1 <- autor %>% 
        filter(!is.na(autor2))
      
      dtaaut <- melt(autor1, id.vars = "Codigo", 
                     variable.name = "Autors", value.name = "from") 
      
      
      autF <- crossprod(table(dtaaut[,c("Codigo", "from")]))
      
      autF <- autF[-1,-1]
      
      va <- as.data.frame(autF) %>%
        dplyr::mutate(Persona = rownames(.),
                      Occurrences = rowSums(.)) %>%
        dplyr::select(Persona, Occurrences) %>% 
        dplyr::filter(Occurrences>as.numeric(input$aut))
      
      
      ed <- as.data.frame(autF) %>% 
        dplyr::mutate(from = rownames(.)) %>%
        tidyr::gather(to, Frequency, 1:ncol(autF)) %>%
        dplyr::mutate(Frequency = ifelse(Frequency == 0, NA, Frequency))
      
      ed <- ed %>% 
        filter(!is.na(ed$Frequency),
               to%in%va$Persona,
               from%in%va$Persona)
      
      ig <- igraph::graph_from_data_frame(d=ed, vertices=va, directed = FALSE)
      
      
      
      tg <- tidygraph::as_tbl_graph(ig) %>% 
        tidygraph::activate(nodes) %>% 
        dplyr::mutate(label=name)
      
      # set seed
      v.size <- V(tg)$Occurrences
      
      # set.seed(12345)
      
      E(tg)$weight <- E(tg)$Frequency
      # inspect weights
      # head(E(tg)$weight, 10)
      
      # set seed
      set.seed(123)
      # edge size shows frequency of co-occurrence
      tg %>%
        ggraph(layout = "fr") +
        geom_edge_arc(colour= "blue",
                      lineend = "round",
                      strength = 1,
                      aes(edge_width = weight,
                          alpha = weight)) +
        geom_node_point(size=sqrt(v.size),
                        colour=rgb(0,0,0,0.5)) +
        geom_node_text(aes(label = name), 
                       repel = TRUE, 
                       point.padding = unit(0.8, "lines"), 
                       size=log(v.size), 
                       colour="gray10") +
        scale_edge_width(range = c(0, 3.5)) +
        scale_edge_alpha(range = c(0, .6)) +
        theme_graph(background = "white") +
        theme(legend.position = "top") +
        guides(edge_width = FALSE,
               edge_alpha = FALSE)
    }
    
  })
  
  ##Carpeta 5: Datos----
  
  output$x1 <- DT::renderDataTable(
    dta[,c( "Codigo", "Autor", "year", "Titulo", "Clase", "Pais")],
    escape=FALSE, rownames = FALSE,
    options = list(
      pageLength = 5, autoWidth = TRUE,
      columnDefs = list(list( targets = 1,
                              width = '200px')),
      scrollX = TRUE
    ),
    class = "display"
  )
  
  ##Carpeta 6: Sumar datos ----
  
  
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
  
  fieldsAll <- c("Cita", "Autor", "year", "Titulo", "DOI")
  
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
    
    googlesheets4::gs4_auth()
    
    ss <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o/edit?usp=sharing") # do the authentication once, manually.
    
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