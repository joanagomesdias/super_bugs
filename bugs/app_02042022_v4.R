#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("shinyWidgets")
#library(shinyWidgets)
library(ggplot2)
#library(ggthemes)
library(datasets)
library(forecast)
library(zoo)


#dataset automatization from SNS
df_auto0 <- read.csv("https://transparencia.sns.gov.pt/explore/dataset/antibioticos-carbapenemes/download/?format=csv&timezone=Europe/London&lang=pt&use_labels_for_header=true&csv_separator=%3B", sep=";", encoding = "UTF-8")
#df2 <- df_auto[c("Período","DDD.Consumidas.de.Carbapenemes","Hospital")]
#df2 <- aggregate(df2$DDD.Consumidas.de.Carbapenemes,
#                     by = list(df2$Período),
#                     FUN = sum) #fazer o aggregate dentro do output para que aconteça tb para os outros campos
#colnames(df2)[colnames(df2) == "Group.1"] <- "data"
#colnames(df2)[colnames(df2) == "x"] <- "consumo_ddd_carbapenemos"
#df2$data <- as.Date(as.yearmon(df2$data, "%Y-%m"), format="%Y-%m", frac = 1)


# filter hospitals by data completeness

hosp.list <- list("Hospital de Santa Maria Maior, E.P.E. - Barcelos",
                  "Centro Hospitalar do Médio Ave, E.P.E.",
                  "Hospital Distrital Figueira da Foz, E.P.E.",
                  "Unidade Local de Saúde do Alto Minho, E.P.E.",
                  "Centro Hospitalar Universitário Cova da Beira, E.P.E.",
                  "Centro Hospitalar do Médio Tejo, E.P.E.",
                  "Centro Hospitalar de Setúbal, E.P.E.",
                  "Centro Hospitalar Universitário de São João, E.P.E.",
                  "Centro Hospitalar Universitário do Algarve, E.P.E.",
                  "Centro Hospitalar Tondela-Viseu, E.P.E.",
                  "Hospital Prof. Doutor Fernando Fonseca, E.P.E.")
df_hosp <- data.frame(matrix(unlist(hosp.list), nrow=length(hosp.list), byrow=TRUE))
colnames(df_hosp)[colnames(df_hosp) == "matrix.unlist.hosp.list...nrow...length.hosp.list...byrow...TRUE."] <- "Hospital"
df_auto <- subset(df_auto0, Hospital%in%df_hosp$Hospital) 

n.months <- 10 #maximum month of last year
    

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
                inputId = "antibiotic",
                label = "Select antibiotic for consumption forecast:",
                choices = df$antibiotic,
                selected = TRUE,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL), 
            selectInput(
                inputId = "bacteria",
                label = "Select bacteria for resistance forecast:",
                choices = df$bacteria,
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
#            numericInput(
#                inputId = "ano", 
#                label = "Select forecast year (2021 - 2030):", 
#                value = 2021,
#                min = 2021,
#                max = 2030,
#                step = 1,
#                width = NULL),
            sliderInput(
                inputId = "ano",
                label = "Select forecast until year end:",
                min = 2021,
                max = 2030,
                value = 2021,
                step = 1,
                sep = ""),
#            selectInput(
#                inputId = "ano", 
#                label = "Select forecast until end of year:", 
#                choices = 2021:ano_max)

            # Input: Choose dataset ----
            selectInput("dataset", "Choose a dataset:",
                        choices = c("Forecast All Hospitals", "Forecast One Hospital")),
            
            # Button
            downloadButton("downloadData", "Download")

        
            
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
        #df_temp <- df2
        
        df3 <- data.frame(df_temp[,-1])
        rownames(df3) <- df_temp[,1]
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (n.months/12) # automatizar
        ds_ts <- ts(df3, start=start.year, frequency=12)
        
        #replace outliers by linear extrapolation
        tsoutliers(ds_ts, iterate = 2, lambda = NULL)
        out <- tsoutliers(ds_ts)
        ds_ts[out$index] <- out$replacements
        
        f <- decompose(ds_ts)
        plot(f)
        plot(ds_ts)
        fit <- auto.arima(ds_ts, xreg=fourier(ds_ts, K=6))
        x0 <- (as.numeric(input$ano) - 2021) * 12 + (12-n.months) #+2 porque faltam 2 meses de dados reais no ultimo ano... automatizar! E o max.year criar para o ano máximo de dados tbm
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        plot(forecast(fit, h=x0, xreg=fourier(ds_ts, K=6, h=x0)), main=paste("Forecast ",input$antibiotic, " DDD by Hospital and month with ARIMA"))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
        
        Forecast.all.hospitals <- forecast(fit, h=x0, xreg=fourier(ds_ts, K=6, h=x0))
        df.forecast.all <- as.data.frame(Forecast.all.hospitals)
   
        
    })
    
    output$consumoPlotMesHosp <- renderPlot({
        
        ## major problem: some hospitals report quartely, others montlhy!
        ## automatization of frequencies is required, or median input for missing values
        ## create dataset with columns by hospital | data frequency | data max | data min as inputs for shiny
        
        df2.b <- df_auto[c("Período","DDD.Consumidas.de.Carbapenemes","Hospital")]
#test        df2.b <- subset(df2.b, Hospital == "Centro Hospitalar do Médio Ave, E.P.E.") #futuramente adicionar (... & Bacteria == input$bacteria & Antibiotic == input$antibiotic)
        df2.b <- subset(df2.b, Hospital == input$hospital) #futuramente adicionar (... & Bacteria == input$bacteria & Antibiotic == input$antibiotic)
        df2.b <- aggregate(df2.b$DDD.Consumidas.de.Carbapenemes,
                                              by = list(df2.b$Período),
                                              FUN = sum) #fazer o aggregate dentro do output em que o dataset anterior ja filtrou pelos inputs
        colnames(df2.b)[colnames(df2.b) == "Group.1"] <- "data"
        colnames(df2.b)[colnames(df2.b) == "x"] <- "consumo_ddd_carbapenemos"
        df2.b$data <- as.Date(as.yearmon(df2.b$data, "%Y-%m"), format="%Y-%m", frac = 1)
        
        df_temp.b <- df2.b[!(df2.b$data < "2017-01-01"),] #remove noise timeseries... automatizar!
#test        df_temp <- df2
        
        df3.b <- data.frame(df_temp.b[,-1])
        rownames(df3.b) <- df_temp.b[,1]
        start.year <- substr(min(df_temp.b$data), 1, 4)
        end.date <- 2021 + (n.months/12) # automatizar
        
        
        ds_ts.b <- ts(df3.b, start=start.year, frequency=12)
        
        #replace outliers by linear extrapolation
        tsoutliers(ds_ts.b, iterate = 2, lambda = NULL)
        out.b <- tsoutliers(ds_ts.b)
        ds_ts.b[out.b$index] <- out.b$replacements
        
        f.b <- decompose(ds_ts.b)
        f.b
        plot(f.b)
        plot(ds_ts.b)
        fit.b <- auto.arima(ds_ts.b, xreg=fourier(ds_ts.b, K=6))
        
#test        x0 <- (2025 - 2021) * 12 + 2 #+2 porque faltam 2 meses de dados reais no ultimo ano... automatizar! E o max.year criar para o ano máximo de dados tbm
        x0 <- (as.numeric(input$ano) - 2021) * 12 + (12-n.months) #+2 porque faltam 2 meses de dados reais no ultimo ano... automatizar! E o max.year criar para o ano máximo de dados tbm
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
#test        xmax <- 2025 + 1
        
        Forecast.one.hospital <- forecast(fit.b, h=x0, xreg=fourier(ds_ts.b, K=6, h=x0))
        df.forecast.one <- as.data.frame(Forecast.one.hospital)
        
        plot(forecast(fit.b, h=x0, xreg=fourier(ds_ts.b, K=6, h=x0)), main=paste("Forecast ",input$antibiotic, " DDD by Hospital and month with ARIMA","\n","(",input$hospital,")"), xlim=c(xmin,xmax))
#test        plot(forecast(fit.b, h=x0, xreg=fourier(ds_ts.b, K=6, h=x0)), main=paste("FOrecast DDD by Hospital and month with ARIMA","\n","HospX"), xlim=c(xmin,xmax))
        abline(v=end.date, col="blue", lwd=0.5, lty=2)
        
    })
    
    output$resistPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x2    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]+1
        # x2    <- df$ano
        y.norm <- df$resistencias_crkp_perc[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.proj <- df$resistencias_crkp_perc_proj[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.low <- df$resistencias_crkp_perc_low_ic95[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.high <- df$resistencias_crkp_perc_high_ic95[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        # y2 <- df$resistencias_crkp_perc
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        # xmax <- 2026
        ymin <- 0
        ymax <- max(df$ymax_resist[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic])
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (n.months/12) # automatizar
        
        # draw the histogram with the specified number of bins
        #ggplot(df, aes(x=input$ano))+
            #(aes(y=y.norm), color="black")+
            #geom_line(aes(y=y.proj), color="blue")+
            #geom_line(aes(y=y.low), color="grey", linetype="dashed")+
            #geom_line(aes(y=y.high), color="grey", linetype="dashed")+
            #geom_vline(xintercept = 2020, linetype="twodash", color="blue")+
            #geom_ribbon(aes(ymin = y.low, ymax = y.high), fill = "grey", alpha = .5)+
            #theme_bw()
        
        plot(x2, y.norm, col = "black", border = 'white', main=paste("Forecast % ",input$bacteria," resistant to ",input$antibiotic," at national level"), type="l", xlim=c(xmin,xmax), ylim=c(ymin,ymax), lwd=1)+
            lines(x2, y.low)+
            lines(x2, y.high)+
            polygon(c(x2, rev(x2)), c(y.low, rev(y.high)), col = "gray85")+
            lines(x2, y.proj, col="blue", lwd=1)+
            abline(v=2021, col="blue", lwd=0.5, lty=1)
    })
    
    output$doentesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x3    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]+1
        #y3 <- df$n_doentes_pt[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.norm <- df$doentes_100000_hab_ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.proj <- df$doentes_100000_hab_ano_proj[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.low <- df$doentes_100000_hab_ano_ic95_low[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.high <- df$doentes_100000_hab_ano_ic95_high[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        ymin <- 0
        ymax <- max(df$ymax_n[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic])
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (n.months/12) # automatizar
        
        # draw the histogram with the specified number of bins
        plot(x3, y.norm, col = "black", border = 'white', main=paste("Forecast patients on ",input$antibiotic," at national level"), type="l", xlim=c(xmin,xmax), ylim=c(ymin,ymax), lwd=1)+
            lines(x3, y.low)+
            lines(x3, y.high)+
            polygon(c(x3, rev(x3)), c(y.low, rev(y.high)), col = "gray85")+
            lines(x3, y.proj, col="blue", lwd=1)+
            abline(v=2021, col="blue", lwd=0.5, lty=1)

    })
    
    output$crkpPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x4    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]+1
        #y4 <- df$n_doentes_crkp_pt[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.norm <- df$doentes_crkp_100000_hab_ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.proj <- df$doentes_crkp_100000_hab_ano_proj[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.low <- df$doentes_crkp_100000_hab_ano_ic95_low[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        y.high <- df$doentes_crkp_100000_hab_ano_ic95_high[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        ymin <- 0
        ymax <- max(df$ymax_n[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic])
        start.year <- substr(min(df_temp$data), 1, 4)
        end.date <- 2021 + (n.months/12) # automatizar
        
        # draw the histogram with the specified number of bins
        plot(x4, y.norm, col = "black", border = 'white', main=paste("Forecast patients with ",input$bacteria," resistant to ",input$antibiotic," at national level"), type="l", xlim=c(xmin,xmax), ylim=c(ymin,ymax), lwd=1)+
            lines(x4, y.low)+
            lines(x4, y.high)+
            polygon(c(x4, rev(x4)), c(y.low, rev(y.high)), col = "gray85")+
            lines(x4, y.proj, col="blue", lwd=1)+
            abline(v=2021, col="blue", lwd=0.5, lty=1)
    })
    
    output$custosPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x5    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic] #não adicionar 1 porque é barplot
        y5 <- df$custos_adicionais_crkp[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        # draw the histogram with the specified number of bins
        barplot(y5, names.arg=x5, col = 'darkgray', border = 'white', main=paste("Additional costs for ",input$bacteria," patients resistant to ",input$antibiotic," at national level"), xlab="ano", ylab="custos", ylim=c(0,100000000))
    })
    
    output$mortesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x6    <- df$ano[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic] #não adicionar 1 porque é barplot
        y6 <- df$mortes_adicionais_crkp[df$ano <= input$ano & df$bacteria == input$bacteria & df$antibiotic == input$antibiotic]
        xmin <- 2017
        xmax <- as.numeric(input$ano) + 1
        # draw the histogram with the specified number of bins
        barplot(y6, names.arg=x6, col = 'darkgray', border = 'white', main=paste("Additional deaths for ",input$bacteria," patients resistant to ",input$antibiotic," at national level"), xlab="ano", ylab="mortes", ylim=c(0,10000))
        
    })
    
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
        switch(input$dataset,
               "Forecast All Hospitals" = df.forecast.all,
               "Forecast One Hospital" = df.forecast.one)
    })
    
  
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )

    
    }

# Run the application 
shinyApp(ui = ui, server = server)
