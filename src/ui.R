#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinydashboard)
require(r2d3)

require(extrafont)
windowsFonts(`quadrata`=windowsFont("Friz Quadrata Std"))
# Define UI for application that draws a histogram
fluidPage( 
  title = "LolDB",
  mainPanel(
    fluidRow(
      column(
        6,
        tabsetPanel(id="HEROES",
          tabPanel("TOUT",value="TOUT", dataTableOutput("TOUT")), 
          tabPanel("ASSASSINS",value="ASSASSINS", dataTableOutput("ASSASSINS")), 
          tabPanel("COMBATTANTS",value="COMBATTANTS", dataTableOutput("COMBATTANTS")), 
          tabPanel("MAGES",value="MAGES", dataTableOutput("MAGES")), 
          tabPanel("TIREURS",value="TIREURS", dataTableOutput("TIREURS")), 
          tabPanel("SUPPORTS",value="SUPPORTS", dataTableOutput("SUPPORTS")), 
          tabPanel("TANKS",value="TANKS", dataTableOutput("TANKS")) 
          
        )
        
      ),
      column(
          3,
          imageOutput("loading", height="30%", width="10%")
      ),
      column(
        3,
        fluidRow(        
          column(4, uiOutput("name")),
          column(8, uiOutput("title")),
          #column(1, imageOutput("icon", height="10%"))
        ),
        fluidRow(
          column(1),
          column(10,
                 plotOutput("info", height="150px", width="195px"),
                 uiOutput("desc"),
          ),
          column(1)
        )
      )
    ),
    hr(),
    fluidRow(
      imageOutput("splash", height="50%")
    )
  )
)
