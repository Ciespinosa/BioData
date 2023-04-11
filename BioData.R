library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(googlesheets4)
library(plotly)
library(reshape2)
library(leaflet)



# deauth


##UI ---

source("3._script/2.UI.R", encoding = "UTF-8")
source("3._script/3.Server.R", encoding = "UTF-8")

ui <- dashboardPage(header, sidebar, body,)



shiny::shinyApp(ui, server)
