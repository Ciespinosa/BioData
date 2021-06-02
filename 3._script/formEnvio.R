fieldsMandatory <- c("cita", "DOI")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

shinyApp(
  ui = fluidPage(titlePanel= h4('Aportar información'),
          shinyjs::useShinyjs(),
          shinyjs::inlineCSS(appCSS),
          div( id= "form",
              box(width = 6,
        
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
        )),
    
       DT::dataTableOutput('x2'),
       
       shinyjs::hidden(
         div(
           id = "thankyou_msg",
           h4("Muchas gracias, su aporte ha sido enviado con éxito!"),
           actionButton("ngo", "Nuevo envío", class = "btn-success", width = 220)
         )
       ) 
       
       
    ),

server = function(input, output, session) {
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
  
  fieldsAll <- c("Cita", "Autor(es)", "year", "Título", "DOI")
  
  formData <- reactive({
      data_to_add <- data.frame(Cita = input$cita, 'Autor(es)'= input$autor,
                                          year = input$anio, "Título" = input$titulo, 
                                          DOI = input$DOI)
      data_to_add
        })
  
  saveData <- function(data) {
    library(googledrive)
    library(googlesheets4)
    library(googleAuthR)
    library(googlesheets)

    sheets_auth(
      email = "civan.espinosain@gmail.com",
      path = NULL,
      scopes = "https://www.googleapis.com/auth/spreadsheets",
      cache = gargle::gargle_oauth_cache(),
      use_oob = gargle::gargle_oob_default(),
      token = "rest.json"
    )

    ss <- gs4_get("https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o/edit?usp=sharing") # do the authentication once, manually.

    data_to_write <- read_sheet('https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o')

    sheet_append(ss,
                 data)
  }
  
  observeEvent(input$go, {
    data_to_add <- formData()
    saveData(data_to_add)
  })
  observeEvent(input$go, {
    output$x2 <- DT::renderDataTable((formData()))
  })
  
  observeEvent(input$go, {
    saveData(formData())
    shinyjs::show("thankyou_msg")
  })
  observeEvent(input$ngo, {
    shinyjs::reset("form")
    shinyjs::hide("thankyou_msg")
  })
}
)

