library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title  = "BioData"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(

      # Dynamic valueBoxes
      valueBoxOutput("progressBox",width = 2),
      valueBoxOutput("progressBox1",width = 2),
      valueBoxOutput("approvalBox",width = 2),
      valueBoxOutput("approvalBox2",width = 2),
      valueBoxOutput("approvalBox3",width = 2),
      valueBoxOutput("approvalBox4",width = 2)
    )
  )
)

server <- function(input, output) {
  
  output$progressBox1 <- renderValueBox({
    valueBox(
      dfTot[1], h4("Aves"), icon = icon("crow"),
      color = "blue"
    )
  })
  
  output$approvalBox2 <- renderValueBox({
    valueBox(
      dfTot[2], h4("Anfibios"), icon = icon("frog"),
      color = "olive"
    )
    
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      dfTot[3], h4("Mamíferos"), icon = icon("otter"),
      color = "light-blue"
    )
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      dfTot[4], h4("Peces"), icon = icon("fish"),
      color = "aqua"
    )
  })
    
  output$approvalBox3 <- renderValueBox({
    valueBox(
      dfTot[5], h4("Reptiles"), icon = icon("suse"),
      color = "yellow"
    )
    
  })
  
  output$approvalBox4 <- renderValueBox({
    valueBox(
      sum(dfTot), h4("Total"), icon = icon("stopwatch-20"),
      color = "red"
    )
    
  })
}



shinyApp(ui, server)

