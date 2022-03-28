#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(ggplot2)
#library(ggthemes)
library(datasets)
library(forecast)
library(zoo)

#load dataset
df <- read.csv("C:\\Users\\bmace\\OneDrive\\Formação\\PhD\\Aulas\\LabHIDA\\Superbugs\\dataset_amr.csv", sep=";", dec=".")
colnames(df)[colnames(df) == "ï..ano"] <- "ano"

ano_max <- max(df$ano)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SuperBugs - AMR Monitor"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
             selectInput(
                inputId = "bacteria",
                label = "Select bacteria:",
                choices = df$bacteria,
                selected = TRUE,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL),
            selectInput(
                inputId = "antibiotic",
                label = "Select antibiotic:",
                choices = df$antibiotic,
                selected = TRUE,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL),
            selectInput(
                inputId = "ano", 
                label = "Select forecast year:", 
                choices = 2021:ano_max)
            
        ),

               # Show a plot of the generated distribution
        mainPanel(
           plotOutput("consumoPlot"),
           plotOutput("consumoPlotMes"),
           plotOutput("resistPlot"),
           plotOutput("doentesPlot"),
           plotOutput("crkpPlot"),
           plotOutput("custosPlot"),
           plotOutput("mortesPlot")
           ),
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

     
    ## fazer um dataset ano/mes e criar uma time-series de quarter/year
    df2 <- read.csv("C:\\Users\\bmace\\OneDrive\\Formação\\PhD\\Aulas\\LabHIDA\\Superbugs\\dataset_amr_data.csv", sep=";", dec=".")
    colnames(df2)[colnames(df2) == "ï..data"] <- "data"
    
    #library(zoo)
    df2$data <- as.Date(as.yearmon(df2$data, "%Y/%m"), format="%Y/%m", frac = 1)
    
    df3 <- data.frame(df2[,-1])
    rownames(df3) <- df2[,1]

    
    
    ## test dataset 
    #df_test <- df[c(1,5)]
    
    ## Forecast consumption by input
    #head(df3)
    #ds_ts <- ts(df3, start=2017, frequency=12)
    #f <- decompose(ds_ts)
    #plot(f)
    #plot(ds_ts)
    #fit <- auto.arima(ds_ts)
    #plot(forecast(fit, h=100))
    
    
    ## Regression on resistance % by input
    
    
    
    ## New dataset with consumption, regression, additional costs and deaths by input
    
    
    
    ## Graphical outputs
   
    output$consumoPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x1    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y1 <- df$consumo_ddd_hab_day[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        
        # draw the histogram with the specified number of bins
        plot(x1, y1, col = 'darkgray', border = 'white', main="DDD/1.000 habitantes/dia", type="l")
        
    })
    
    output$consumoPlotMes <- renderPlot({
        ds_ts <- ts(df3, start=2017, frequency=12)
        f <- decompose(ds_ts)
        plot(f)
        plot(ds_ts)
        fit <- auto.arima(ds_ts)
        x0 <- (as.numeric(input$ano) - 2020) * 12
        plot(forecast(fit, h=x0), main="FOrecast DDD by month with ARIMA", ylim=c(-10000,50000))
        
    })
    
    output$resistPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x2    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y2 <- df$resistencias_crkp_perc[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        
        # draw the histogram with the specified number of bins
        plot(x2, y2, col = 'darkgray', border = 'white', main="% Resistências CRKP", type="l")
    })
    
    output$doentesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x3    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y3 <- df$n_doentes_pt[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        
        # draw the histogram with the specified number of bins
        plot(x3, y3, col = 'darkgray', border = 'white', main="N doentes carbapenemos", type="l")
    })
    
    output$crkpPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x4    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y4 <- df$n_doentes_crkp_pt[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        
        # draw the histogram with the specified number of bins
        plot(x4, y4, col = 'darkgray', border = 'white', main="N doentes carbapenemos", type="l")
    })
    
    output$custosPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x5    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y5 <- df$custos_adicionais_crkp[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        
        # draw the histogram with the specified number of bins
        barplot(y5, names.arg=x5, col = 'darkgray', border = 'white', main="Custos adicionais CRKP", xlab="ano", ylab="custos")
    })
    
    output$mortesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x6    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y6 <- df$mortes_adicionais_crkp[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        
        # draw the histogram with the specified number of bins
        barplot(y6, names.arg=x6, col = 'darkgray', border = 'white', main="Mortes adicionais CRKP", xlab="ano", ylab="mortes")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
