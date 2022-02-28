
## First specify the packages of interest
packages = c("tidyverse", "ggplot2","broom","GGally", "caTools","viridis", "janitor","lubridate","forecast","fpp3","fpp2","ggthemes","hrbrthemes","correlation","see","ggraph","plotly","dplyr", "DT","graphics","shiny","shinydashboard","shinycssloaders","shinythemes","plotly")

## Now load or install&load all libraries neccessary to work
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

not_sel <- "Not Selected" # We will use this as an option for variable selection

shinyServer(function(input, output) {
  
  ## Import the dataset from the official link
    climate<-read.csv("https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=53001&Year=2020&Month=6&Day=1&timeframe=2&submit=Download+Data")
  
  ## PRE-PROCCESING
    ## remove all empty variables (columns) from dataset
  climate<-climate %>%
    remove_empty("cols") #select either row or cols or both
  
  ## add a week variable to the data
  climate <- climate %>% 
    mutate(Week = lubridate::week(Date.Time))
  
  ##  Convert numeric month numeric values to name abbreviations
  climate$Month<- month.abb[climate$Month] 
  
  ## Convert  Date.Time to a date variable for processing with time series analysis
  climate$Date.Time<- as.POSIXct(climate$Date.Time, format = "%Y-%m-%d")
  climate$Date.Time<-as.Date(climate$Date.Time,format='%Y/%m/%d')# convert time date into a date variable
  
  ## Create a weekday variable
  climate$weekday<- weekdays(climate$Date.Time)#  add new variable with weekday names from Date.Time var
  
  ## Set categorical and numerical variables
  climate[,c(6,9,11,13,15,17,20,22)] <- sapply(climate[,c(6,9,11,13,15,17,20,22)],as.numeric )
  climate[,c(3,7,10,12,14,16,18,19,21,23,1,2,4,8,24,25)] <- sapply(climate[,c(3,7,10,12,14,16,18,19,21,23,1,2,4,8,24,25)], as.factor)
  
  
  
  choices <- c(not_sel, names(climate))
  
  ## Organize the variables as numeric and factor which will be automatically displayed in the selection button on the app.
  observeEvent(climate, {
    updateSelectInput(inputId = "num_var_x", choices = names(climate[c(6,9,11,13,15,17,20,22)]),selected = names(climate[6]))
    updateSelectInput(inputId = "num_var_y", choices = names(climate[c(6,9,11,13,15,17,20,22)]),selected = names(climate[9]))
    updateSelectInput(inputId = "fact_var", choices = names(climate[c(1:4,7:8,10,12,14,16,18:19,21,23:25)]),selected = names(climate[7]))
  })
  
  
  ## Read selected values from the user for the three variables
  num_var_x <- eventReactive(input$run_button,input$num_var_x)
  num_var_y <- eventReactive(input$run_button,input$num_var_y)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  ## Creating the graphic visualizations
  draw_plot_1 <- function(data_input, num_var_x){
    fig.1 <- plot_ly(data = data_input, type = 'scatter', mode = 'lines', width = 800, height = 400)%>%
      add_trace(x=~Date.Time, y=as.formula(paste0("~`", num_var_x, "`")))%>%
      layout(showlegend = F, title='Select the range of Time Series visualization',
             xaxis = list(rangeslider = list(visible = T),
                          rangeselector=list(
                            buttons=list(
                              list(count=1, label="1m", step="month", stepmode="backward"),
                              list(count=3, label="3m", step="month", stepmode="backward"),
                              list(count=6, label="6m", step="month", stepmode="backward"),
                              list(step="all")
                            ))))
    return(fig.1)
  }
  
  draw_plot_2 <- function(data_input, num_var_y){
    fig.2 <- plot_ly(data = data_input, type = 'scatter', mode = 'lines', width = 800, height = 400)%>%
      add_trace(x=~Date.Time, y=as.formula(paste0("~`", num_var_y, "`")))%>%
      layout(showlegend = F, title='Select the range of Time Series visualization',
             xaxis = list(rangeslider = list(visible = T),
                          rangeselector=list(
                            buttons=list(
                              list(count=1, label="1m", step="month", stepmode="backward"),
                              list(count=3, label="3m", step="month", stepmode="backward"),
                              list(count=6, label="6m", step="month", stepmode="backward"),
                              list(step="all")
                            ))))
    return(fig.2)
  }
  
  
  draw_plot_3 <- function(data_input, num_var_x,fact_var){
    fig.3 <- plot_ly(data_input, y = as.formula(paste0("~`", num_var_x, "`")), color =as.formula(paste0("~`", fact_var, "`")), type = "box")
    return(fig.3)
  }
  
  
  draw_plot_4 <- function(data_input){
    fig.4<-ggcorr(data_input,label=TRUE)# for numerical variables
    return(fig.4)
    
  }
  
  draw_plot_5 <- function(data_input){
    fig.5<-correlation(data_input ,partial = TRUE) %>%
      plot()
    return(fig.5)
  }
  
  draw_plot_6 <- function(data_input, num_var_x,num_var_y,fact_var){
    fig.6 <- plot_ly(data_input, x=as.formula(paste0("~`", num_var_x, "`")), y=as.formula(paste0("~`", num_var_y, "`")), color=as.formula(paste0("~`", fact_var, "`")), colors="Set1")%>% 
      layout(title="Scatterplot of numeric variables with factor categories")
    return(fig.6)
  }
  
  
  ## Plot Output 
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(climate, num_var_x())
  })
  
  plot_2 <- eventReactive(input$run_button,{
    draw_plot_2(climate, num_var_y())
  })
  
  plot_3 <- eventReactive(input$run_button,{
    draw_plot_3(climate, num_var_x(),fact_var())
  })
  
  plot_4 <- eventReactive(input$run_button,{
    draw_plot_4(climate)
  })
  
  plot_5 <- eventReactive(input$run_button,{
    draw_plot_5(climate)
  })
  
  plot_6 <- eventReactive(input$run_button,{
    draw_plot_6(climate, num_var_x(),num_var_y(),fact_var())
  })
  
  
  output$plot_1 <- renderPlotly(plot_1())
  output$plot_2 <- renderPlotly(plot_2())
  output$plot_3 <- renderPlotly(plot_3())
  output$plot_4 <- renderPlot(plot_4())
  output$plot_5 <- renderPlot(plot_5())
  output$plot_6 <- renderPlotly(plot_6())
 
  
## Regression summary output

print_summary <- function(data_input, num_var_x,num_var_y){
  fit<-lm(data_input[,input$num_var_y]~data_input[,input$num_var_x])
 summary(fit)
 }

print_sum<- eventReactive(input$run_button,{
  print_summary(climate, num_var_x(),num_var_y())
})

output$summary <- renderPrint(print_sum())


##### test ########################

## statistics summary output ??

print_desc<-function(data_input, num_var_x,num_var_y){
 sum<-summary(data.frame(Var_X=data_input[,input$num_var_x],Var_Y=data_input[,input$num_var_y]))
  return(sum)
}

print_sum_stat<- eventReactive(input$run_button,{
  print_desc(climate, num_var_x(),num_var_y())
})

output$summary_stat <- renderTable(data.frame(print_sum_stat()))
###################################################################

## Obtain Time Series predictions from models: Seasonal Naive and SARIMA

draw_plot_7 <- function(data_input, num_var_x){
  climate_s<-ts(data_input,start=2020,frequency=365)
  pred<-snaive(ts(data_input[,input$num_var_x],start=2020,frequency=365))# fit the prediction model
  fig.7<-autoplot(forecast(pred),ylab="Numeric variable X") #plot prediction
  return(fig.7)
}

plot_7 <- eventReactive(input$run_button,{
  draw_plot_7(climate, num_var_x())
})

output$plot_7 <- renderPlot(plot_7())

draw_plot_8 <- function(data_input, num_var_x){
  climate_s<-ts(data_input,start=2020,frequency=365)
  pred<-auto.arima(ts(data_input[,input$num_var_x],start=2020,frequency=365))# fit the prediction model
  fig.8<-autoplot(forecast(pred),ylab="Numeric variable X") #plot prediction
  return(fig.8)
}

plot_8 <- eventReactive(input$run_button,{
  draw_plot_8(climate, num_var_x())
})

output$plot_8 <- renderPlot(plot_8())

})