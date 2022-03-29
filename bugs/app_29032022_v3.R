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


#dataset automatization from SNS
df_auto <- read.csv("https://transparencia.sns.gov.pt/explore/dataset/antibioticos-carbapenemes/download/?format=csv&timezone=Europe/London&lang=pt&use_labels_for_header=true&csv_separator=%3B", sep=";", encoding = "UTF-8")
#df2 <- df_auto[c("Período","DDD.Consumidas.de.Carbapenemes","Hospital")]
#df2 <- aggregate(df2$DDD.Consumidas.de.Carbapenemes,
#                     by = list(df2$Período),
#                     FUN = sum) #fazer o aggregate dentro do output para que aconteça tb para os outros campos
#colnames(df2)[colnames(df2) == "Group.1"] <- "data"
#colnames(df2)[colnames(df2) == "x"] <- "consumo_ddd_carbapenemos"
#df2$data <- as.Date(as.yearmon(df2$data, "%Y-%m"), format="%Y-%m", frac = 1)


#load dataset manual from ECDC
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
                inputId = "hospital",
                label = "Select hospital:",
                choices = df_auto$Hospital,
                selected = TRUE,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL),
            numericInput(
                inputId = "ano", 
                label = "Select forecast year (2021 - 2030):", 
                value = 2021,
                min = 2021,
                max = 2030,
                step = 1,
                width = NULL)
#            selectInput(
#                inputId = "ano", 
#                label = "Select forecast until end of year:", 
#                choices = 2021:ano_max)
            
        ),

               # Show a plot of the generated distribution
        mainPanel(
#           plotOutput("consumoPlot"),
           plotOutput("consumoPlotMes"),
           plotOutput("consumoPlotMesHosp"),
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
    #df2 <- read.csv("C:\\Users\\bmace\\OneDrive\\Formação\\PhD\\Aulas\\LabHIDA\\Superbugs\\dataset_amr_data.csv", sep=";", dec=".")
    #colnames(df2)[colnames(df2) == "ï..data"] <- "data"
        #library(zoo)
    #df2$data <- as.Date(as.yearmon(df2$data, "%Y/%m"), format="%Y/%m", frac = 1)
    
    

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
   
#    output$consumoPlot <- renderPlot({
#        # generate bins based on input$bins from ui.R
#        x1    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
#        y1 <- df$consumo_ddd_hab_day[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
#        
#        # draw the histogram with the specified number of bins
#        plot(x1, y1, col = 'darkgray', border = 'white', main="DDD/1.000 habitantes/dia", type="l")
#        
#    })
    
    output$consumoPlotMes <- renderPlot({
        
        df2 <- df_auto[c("Período","DDD.Consumidas.de.Carbapenemes")]
        #df2 <- subset(df2, Hospital == input$hospital) #futuramente adicionar (... & Bacteria == input$bacteria & Antibiotic == input$antibiotic)
        df2 <- aggregate(df2$DDD.Consumidas.de.Carbapenemes,
                         by = list(df2$Período),
                         FUN = sum) #fazer o aggregate dentro do output em que o dataset anterior ja filtrou pelos inputs
        colnames(df2)[colnames(df2) == "Group.1"] <- "data"
        colnames(df2)[colnames(df2) == "x"] <- "consumo_ddd_carbapenemos"
        df2$data <- as.Date(as.yearmon(df2$data, "%Y-%m"), format="%Y-%m", frac = 1)
        
        df_temp <- df2[!(df2$data < "2017-01-01"),] #remove noise timeseries... automatizar!
        
        df3 <- data.frame(df_temp[,-1])
        rownames(df3) <- df_temp[,1]
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (9/12) # automatizar
        ds_ts <- ts(df3, start=start.year, frequency=12)
        f <- decompose(ds_ts)
        plot(f)
        plot(ds_ts)
        fit <- auto.arima(ds_ts)
        x0 <- (as.numeric(input$ano) - 2021) * 12 + 3 #+3 porque faltam 3 meses de dados reais no ultimo ano... automatizar! E o max.year criar para o ano máximo de dados tbm
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        plot(forecast(fit, h=x0), main="Forecast DDD all Hospitals by month with ARIMA", xlim=c(xmin,xmax))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
        
    })
    
    output$consumoPlotMesHosp <- renderPlot({
        
        df2 <- df_auto[c("Período","DDD.Consumidas.de.Carbapenemes","Hospital")]
        df2 <- subset(df2, Hospital == input$hospital) #futuramente adicionar (... & Bacteria == input$bacteria & Antibiotic == input$antibiotic)
        df2 <- aggregate(df2$DDD.Consumidas.de.Carbapenemes,
                                              by = list(df2$Período),
                                              FUN = sum) #fazer o aggregate dentro do output em que o dataset anterior ja filtrou pelos inputs
        colnames(df2)[colnames(df2) == "Group.1"] <- "data"
        colnames(df2)[colnames(df2) == "x"] <- "consumo_ddd_carbapenemos"
        df2$data <- as.Date(as.yearmon(df2$data, "%Y-%m"), format="%Y-%m", frac = 1)
        
        df_temp <- df2[!(df2$data < "2017-01-01"),] #remove noise timeseries... automatizar!
        
        df3 <- data.frame(df_temp[,-1])
        rownames(df3) <- df_temp[,1]
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (9/12) # automatizar
        ds_ts <- ts(df3, start=start.year, frequency=12)
        f <- decompose(ds_ts)
        plot(f)
        plot(ds_ts)
        fit <- auto.arima(ds_ts)
        x0 <- (as.numeric(input$ano) - 2021) * 12 + 3 #+3 porque faltam 3 meses de dados reais no ultimo ano... automatizar! E o max.year criar para o ano máximo de dados tbm
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        plot(forecast(fit, h=x0), main=paste("FOrecast DDD by Hospital and month with ARIMA","\n","(",input$hospital,")"), xlim=c(xmin,xmax))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
        
    })
    
    output$resistPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x2    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]+1
        y2 <- df$resistencias_crkp_perc[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (9/12) # automatizar
        
        # draw the histogram with the specified number of bins
        plot(x2, y2, col = 'darkgray', border = 'white', main="% Resistências CRKP all Hospitals", type="l", xlim=c(xmin,xmax))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
    })
    
    output$doentesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x3    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]+1
        y3 <- df$n_doentes_pt[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (9/12) # automatizar
        
        # draw the histogram with the specified number of bins
        plot(x3, y3, col = 'darkgray', border = 'white', main="N doentes carbapenemos all Hospitals", type="l", ylim=c(0,400000), xlim=c(xmin,xmax))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
    })
    
    output$crkpPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x4    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]+1
        y4 <- df$n_doentes_crkp_pt[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (9/12) # automatizar
        
        # draw the histogram with the specified number of bins
        plot(x4, y4, col = 'darkgray', border = 'white', main="N doentes CRKP all Hospitals", type="l", ylim=c(0,45000), xlim=c(xmin,xmax))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
    })
    
    output$custosPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x5    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic] #não adicionar 1 porque é barplot
        y5 <- df$custos_adicionais_crkp[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        # draw the histogram with the specified number of bins
        barplot(y5, names.arg=x5, col = 'darkgray', border = 'white', main="Custos adicionais CRKP all Hospitals", xlab="ano", ylab="custos", ylim=c(0,100000000))
    })
    
    output$mortesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x6    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic] #não adicionar 1 porque é barplot
        y6 <- df$mortes_adicionais_crkp[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        # draw the histogram with the specified number of bins
        barplot(y6, names.arg=x6, col = 'darkgray', border = 'white', main="Mortes adicionais CRKP all Hospitals", xlab="ano", ylab="mortes", ylim=c(0,10000))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
