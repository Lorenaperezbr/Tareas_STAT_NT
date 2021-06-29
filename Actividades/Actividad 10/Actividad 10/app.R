#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Act 10, Lorena Pérez"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "cant", label = "Tamaño muestral:", min = 1, max = 100, value = 50),
            selectInput(inputId = 'dist', label = 'Distribucion', c("Normal", "Gamma", "Ambas")),
            numericInput(inputId = 'bins', label = "Cantidad de Bins", min=1, value=10),
            img(src = "shiny.png", height = 100, width = 100)),
        mainPanel(
            tabsetPanel(
                tabPanel("Histograma", plotOutput("hist")),
                tabPanel("Plot",
                         fluidRow(
                             column(5,plotOutput('p1')),
                             column(5,plotOutput('p2'))
                         ),
                         fluidRow(
                             column(5,plotOutput('p3')),
                             column(5,plotOutput('p4')))),
                tabPanel("Resumen"),
                tabPanel("Tabla")
            )
        )
    )
)
server <- 
    function(input, output) { 
        
        base <- reactive(
            if (input$dist=="Gamma") {rgamma(input$cant, shape=1)} else
                if (input$dist=="Normal") {rnorm(input$cant)} else {
                    gamma<-rgamma(input$cant, shape=1)
                    normal<-rnorm(input$cant)
                    gamma_normal<-data.frame(value=c(gamma, normal),tipo=c(rep("gamma", times=input$cant), rep("normal", times=input$cant)))
                })
        
        grafico <- reactive(
            if (input$dist=="Gamma" | input$dist=="Normal"){graf<-data.frame(x=base()) %>% ggplot(aes(x)) + geom_histogram(bins=input$bins)
            print(graf)} else {
                graf<-base() %>% ggplot(aes(x=value)) + geom_histogram(bins=input$bins) + facet_wrap(vars(tipo))
                print(graf)})
        
        
        output$hist <- renderPlot({grafico()})
        
        output$p1 <- renderPlot({plot(grafico())})
        
        output$p2 <- renderPlot({plot(grafico())})
        
        output$p3 <- renderPlot({plot(grafico())})
        
        output$p4 <- renderPlot({plot(grafico())})
        
    }

shinyApp(ui, server)
