library(shiny)
library(tidyverse)
library(DT)

# Tarea3

datos <- read_csv("propina.csv")

ui <- fluidPage( titlePanel("Datos sobre Propina"),
  sidebarLayout(  sidebarPanel(selectInput('varcolor', 'Variable en color',
                  c("sexo", "fuma", "dia", "momento")),
      selectInput('digitos', 'Decimales',c(0,1,2)),
      actionButton("run", "Mostrar"),
      actionButton("reset", "No mostrar")),
    
   mainPanel(tabsetPanel(tabPanel("Bivariado",h2("Diagrama de dispersión", align = "center"),
          plotOutput("scat" ),dataTableOutput("pestaña")),
      tabPanel("Univariado",h2("Gráfico de Barras", align = "center"),plotOutput("bar"))))
    ))

server <- function(input, output){
   val <- reactiveValues()
   observe({val$reactive_col <- input$varcol})
   observe({val$reactive_col <- input$digitos})
   serv <- reactiveValues(data = NULL)
 observeEvent(input$run, { 
     serv$g1 <-ggplot(data = datos, aes(x = total, y = propina, colour = .data[[input$varcolor]]))+
       geom_point() + theme(aspect.ratio = 1) + scale_x_continuous(name ="Total de la cuenta") +
       scale_y_continuous(name = "Propinas")
    
    serv$g2 <-ggplot(datos, aes(x=.data[[input$varcolor]])) + 
       geom_bar() + labs(y = "Cantidad de propinas")
    
    serv$pestaña <- reactive({as.data.frame(datos) %>%
        group_by(".data[[input$varcol]]") %>%
        summarise(mean_prop = mean(propina),sd_propina = sd(propina),mean_total = mean(total),
          sd_total = sd(total)) %>% 
        mutate(across(where(is.double), function (x) {round(x, digits = 3)}
          )) %>% datatable()})
    })
  
  observeEvent(input$reset,
  { serv$g1 <- NULL
    serv$g2 <- NULL
    serv$pestaña <- NULL})    
    
  output$scat <- renderPlot(
    {if (is.null(serv$g1)) 
      return()
    plot(serv$g1)})  
  
  
  output$bar <- renderPlot(
    {if (is.null(serv$g2)) 
      return()
    plot(serv$g2)})

  output$pestaña <- renderDT({
    if (is.null(serv$pestaña)) 
      return()
    print(serv$pestaña())})
  }

shinyApp(ui, server)
  




