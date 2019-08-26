library(tidyverse)
library(leaflet)
library(shiny)
library(DT)
library(shinydashboard)
library(ECharts2Shiny)

ui <- dashboardPage(skin = "green", 
                    title = "Електронна карта дерев Ботанічного саду НУБІП",
  header <- dashboardHeader(title = "Електронна карта дерев Ботанічного саду НУБІП",
                            titleWidth = 500),
  sidebar <- dashboardSidebar(
    tags$div(
      tags$blockquote("Вітаємо на сайті інтерактивної карти Ботсаду НУБІП :)"),
      tags$h4("Інформація про дерево доступна по кліку на його значок, а саме:"),
      tags$p("- опис виду (у нижньому лівому куті);"),
      tags$p("- екосистемні послуги та їхня вартість знизу від карти;"),
      tags$p("- деякі цікаві показники колекції дерев Ботсаду справа від карти."),
      style = "padding: 10px;"
      ),
    tags$hr(),
    tags$div(
      textOutput("text_name"),
      textOutput("text"), 
      style = "padding: 10px;")),
  
  body <- dashboardBody(loadEChartsLibrary(),
    fluidRow(
    column(width = 9,
           box(width = NULL,
               leafletOutput("map", height = 500)
           ),
           box(title = "Характеристика екосистемних послуг дерева або куща",
               width=NULL,
               tableOutput("table")
           )
           ),
    column(
      width = 3,
           tags$div(
             tags$h4("Колекція екземплярів дерев і кущів Ботсаду, шт.:"),
             style = "padding: 30px;"
           ),
           tags$div(id="test", style="width:100%;height:200px;"),
           deliverChart(div_id = "test"),
           tags$div(
             tags$h4("Щорічна вартість екосистемних послуг, грн.:"),
             style = "padding: 30px;"
           ),
           tags$div(id="pie", style="width:100%;height:200px;"),
           deliverChart(div_id = "pie")
    )
  )
)
)
