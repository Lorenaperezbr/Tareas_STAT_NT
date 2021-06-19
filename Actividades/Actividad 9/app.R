#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <-fluidPage(
    sliderInput(inputId = "cant"
                  ,
                  label = "tamaño muestral:"
                  ,
                  min = 1,
                  max = 500,
                  value = 30),
plotOutput("hist")
)

server <- function(input, output){
    output$hist <- renderPlot({
  grafico <- data.frame(x = rnorm(input$cant)) %>%
            ggplot(aes(x )) + geom_histogram(binwidth = 0.25)
        print(grafico)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

###### Parte 2
ui <- fluidPage(
    sliderInput(inputId = "cant"
                ,
                label = "tamaño muestral:"
                , min = 1, max = 500,
                value = 30),
    numericInput(inputId = 'media'
                 , label="Media"
                 , value = 0),
    numericInput(inputId = 'desvio'
                 , label = "SD"
                 , value = 1),
    plotOutput("hist")
)
server <- function(input, output){
    output$hist <- renderPlot({
        dist <- data.frame(x = rnorm(input$cant,
                                     mean = input$media, sd =
                                         input$desvio))
    grafico<- dist %>%
            ggplot(aes(x)) + geom_histogram(binwidth = 0.25)
        print(grafico)
    })
}
shinyApp(ui, server)
######### Parte 3
ui <- fluidPage(
    sliderInput(inputId = "cant"
                ,
                label = "tamaño muestral:"
                , min = 1, max = 500,
                value = 30),
    selectInput('distri'
                ,
                'Distribución'
                , c("Gamma"
                    ,
                    "Normal")),
    plotOutput("hist")
)
server <- function(input, output){
    output$hist <- renderPlot({
        if (input$distri == "Normal") {
            dist <- data.frame(x = rnorm(input$cant))
            gg <- dist %>%
                ggplot(aes(x)) + geom_histogram(binwidth = 0.25)
            print(gg)
        }else{
            dist <- data.frame(x = rgamma(
                n = input$cant, shape = 1 ))
            gg <- dist %>%
                ggplot(aes(x) ) + geom_histogram(binwidth = 0.25)
            print(gg)
        }
    })
}
shinyApp(ui, server)