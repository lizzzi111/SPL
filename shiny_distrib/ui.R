# ui function for  daily student alcohol consumption

library(shiny)
shinyUI(fluidPage(
  titlePanel("Exploring Students' Grades"),
  sidebarLayout(
    sidebarPanel(
      helpText("Exploring student grades and their behavior"),
      selectInput("dep_var", 
                  label = "Choose a Grade to display G1,G2,G3 as dependent variable",
                  choices = c("G1", "G2",
                              "G3"),
                  selected = "G3"),
#      selectInput("indep_var", 
#                  label = "Choose a grouping variable",
#                  choices = c(),
#                  selected = ),
      sliderInput("bins", 
                  label = "Binwidth",
                  min = 0, max = 10, value = 5),
      
    ),
    mainPanel(plotOutput("plot"))
  )
))