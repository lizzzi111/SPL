library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(
    selectInput("dependent", "Dependent Variable:", c("G1", "G2", "G3"), "G1"),
    uiOutput("independent")
),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Regression Plot",
               plotOutput("regPlot")),
      tabPanel("Regression Summary", 
               verbatimTextOutput("regSummary")))
  )
)