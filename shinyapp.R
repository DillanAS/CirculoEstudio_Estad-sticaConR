library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Resultados"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Histogramas", tabName = "Dashboard", icon = icon("bar-chart")),
                    menuItem("Dispersion", tabName = "graph", icon = icon("area-chart")),
                    menuItem("Regresion", tabName = "regression", icon = icon("table")),
                    menuItem("Prediccion", tabName = "prediction", icon = icon("line-chart")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("file-excel-o"))

                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    

                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                titlePanel("Histograma de las variables"), 
                                selectInput("x", "Seleccione la variable",
                                            choices = c("spending", "rest1", "rest2", "rest3", "rest4", "total")),
                                
                                selectInput("zz", "Selecciona la variable del comparacion", 
                                            choices = c("holiday", "mkt_strategy")),
                                box(plotOutput("plot1", height = 300, width = 460)),
                            )
                    ),
                    

                    tabItem(tabName = "graph", 
                            fluidRow(
                                titlePanel(h3("Gráficos de dispersión")),
                                selectInput("a", "Selecciona el valor de x",
                                            choices = c("spending", "rest1", "rest2", "rest3", "rest4", "total")),
                                selectInput("y", "Seleccione el valor de y",
                                            choices = c("spending", "rest1", "rest2", "rest3", "rest4", "total")),
                                selectInput("z", "Selecciona la variable del grid", 
                                            choices = c("holiday", "mkt_strategy")),
                                box(plotOutput("output_plot", height = 300, width = 460) )
                                
                            )
                    ),
                    
                    
                    
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    tabItem(tabName = "regression",
                            fluidRow(
                                titlePanel(h3("Resultados regresion lineal")),
                                verbatimTextOutput("regression")
                            )
                    ),
                    
                    tabItem(tabName = "prediction",
                            fluidRow(
                                titlePanel(h3("Prediccion total spending")),
                                box(plotOutput("prediction", height = 300, width = 460)
                            )
                    )
                    
                )
            )
        )
    ))



server <- function(input, output) {
    library(ggplot2)
    url_github = "https://raw.githubusercontent.com/DillanAS/CirculoEstudio_EstadisticaConR/main/RestaurantVisitors.csv"
    df <- read.csv(url_github, header = TRUE)
    model1 <- lm(df$spending ~ df$mkt_strategy + df$holiday + df$total)
    spending.ts <- ts(df$spending, start = 1, freq = 12)
    arima_model <- arima(spending.ts, order = c(2, 1, 4))
    pred <- predict(arima_model, 30)$pred

    output$plot1 <- renderPlot({
        
        x <- df[,input$x]

        ggplot(df, aes(x, fill = df[,input$zz])) + 
            geom_histogram() +
            labs( xlim = c(0, max(x))) + 
            theme_light() + 
            xlab(input$x) + ylab("Frecuencia") + 
            facet_grid(input$zz)
        
        
    })
    

    output$output_plot <- renderPlot({ 
        
        ggplot(df, aes(x =  df[,input$a] , y = df[,input$y], 
                           colour = factor(df[,input$z]))) + 
            geom_point() +
            ylab(input$y) +
            xlab(input$a) + 
            theme_linedraw()
        
    })

    
    output$data_table <- renderDataTable({df}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
    output$prediction <- renderPlot({
        ts.plot(cbind(spending.ts, pred), col = c("blue", "red"), xlab = "")
        title(main = "Time Series Prediction ARIMA(2,1,4)",
              xlab = "Time",
              ylab = "Total spending")
    })
    
    output$regression <- renderPrint({
        summary(model1)
    })
}


shinyApp(ui, server)