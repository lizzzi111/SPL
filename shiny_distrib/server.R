# server function for  daily student alchohol consumption

library(ggplot2)
alc <- read.csv("../data/student-por.csv",stringsAsFactors = F,header=T)
plot_function <- function(var,color,wbins,title_text)
{
  var <- var[,c(-1,-2)]
  var$Dalc <- as.factor(var$Dalc)
  g <- ggplot(var,aes(x=grade))+geom_histogram(binwidth = wbins,fill=color)+facet_grid(Dalc~.)+ggtitle(title_text)+theme(
    plot.title = element_text(hjust=0.4)
  )
  g
}
shinyServer(
  function(input, output) {
    output$plot <- renderPlot({
      data <- switch(input$var, 
                     "G1" = alc[,"G1"],
                     "G2" = alc[,"G2"],
                     "G3" = alc[,"G3"]
      )
      
      color <- switch(input$var, 
                      "G1" = "darkgreen",
                      "G2" = "black",
                      "G3" = "darkorange"
      )
      title <- switch(input$var,
                      "G1"="Daily Alcohol Consumption and Grade 1",
                      "G2"="Daily Alcohol Consumption and Grade 2",
                      "G3"="Daily Alcohol Consumption and Grade 3")

      plot_function(var = data, 
                    color = color, 
                    wbins = input$bins,
                    title_text=title)
    })
  }
)