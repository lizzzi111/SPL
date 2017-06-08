library(shiny)
library(ggplot2)

server <- (function(input, output) {
  output$independent <- renderUI({
    checkboxGroupInput(
      "independent", "Independent Variables:",
      c("G1", "G2", "G3")[!c("G1", "G2", "G3") %in% input$dependent],
      selected = c("G1", "G2", "G3")[!c("G1", "G2", "G3") %in% input$dependent][1]
    )
  })
  
  regression <- reactive({
    lm(paste(input$dependent," ~ ",
             paste(input$independent,
                   collapse = " + ")),
       data = data)
    
  })
  output$regPlot <- {
    renderPlot({
      p <- ggplot(regression(), 
                            aes_string(x = paste(input$independent, collapse = " + "), 
                                y = input$dependent)
    )
    p <- p + geom_point() + geom_smooth(method = "lm")
    
    print(p)
    
    }, height = 700)
  }
  
  output$regSummary <- renderPrint({
    if(!is.null(input$independent)){
      summary(regression())
    } else {
      print("Please select the model's dependent and independent variables.")
    }
  })
})