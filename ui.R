library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(DT)
library(plotly)

not_sel <- "Not Selected"

dashboardPage(
  dashboardHeader(title = "Historical Climate"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("About", tabName = "about")
    ),
    br(),
    br(),
    br(),
    br(),
    selectInput("num_var_x", "Numerical Variable X", choices = c(not_sel)),
    selectInput("num_var_y", "Numerical Variable Y", choices = c(not_sel)),
    selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
    br(),
    conditionalPanel(
      print("Variable X should be different than variable Year to obtain a more accurate analysis."), # desired output
      condition="input.line==1"),#This is a new input
    br(),
    checkboxInput("line", "Attention! Check this box for information!"),
    #
    br(),
    actionButton("run_button", "Run Analysis", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
      # 
      # Add a tabPanel
      tabsetPanel(
          tabPanel(
            title = "Box plots",
            plotlyOutput("plot_3"),
            strong("NOTE"),
            p("The boxplot will be created automatically from inputs of numeric X variable and categorical variable selected. By default the categorical variable will be",span("Month", style = "color:red")),
            box(
             width=14,
             title="Statistics Summary for numerical variables X and Y",
             strong("NOTE"),
             h5("This is the summary output for selected variables X and Y."),
              tableOutput("summary_stat"),
              )
            ),
          tabPanel(
            title = "Time Series",
            plotlyOutput("plot_1"),
            br(),
            plotlyOutput("plot_2"),
            strong("NOTE"),
            h5("The time series plot are created from the numeric variables X and Y selected from the menu. You may change the visualization by using the scroll button at the end of each plot."),
            p("You may also use the minimized tabs at the top-left of the plot to obtain the last:",span("1m=1 Month, 3m=3 Month, 6m= 6 Month", style = "color:red") ,"visualization of the time series."),
            ),
          tabPanel(
            title = "Time Series Prediction",
            plotOutput("plot_7"),
            strong("NOTE"),
            p("This is the prediction from",span("SEASONAL NAIVE", style = "color:red"),"time series model for Numeric variable selected X.To obtain prediction for other variables just select the variable from the X variable tab"),
            br(),
            plotOutput("plot_8"),
            strong("NOTE"),
            p("This is the prediction from", span("SARIMA", style = "color:red"), "time series model for Numeric variable selected X.To obtain prediction for other variables just select the variable from the X variable tab"),
          ),
         tabPanel(
            title = "Linear Model",
            plotlyOutput("plot_6"),
            br(),br(),
            tableOutput("results"),
            box(
              withSpinner(verbatimTextOutput("summary")),
              width=14,
              title="Model Summary",
              strong("NOTE"),
              p("This is the summary output of",span("linear regression", style = "color:red"),  "model for selected variables X and Y."),
              p("If you want to see the scatterplot for different categories of factor variable just",span("Double-click", style = "color:red"),"on legend to isolate trace. "),
            )
          ),
         tabPanel(
            title = "Correlations",
            plotOutput("plot_5"),
            strong("NOTE"),
            h5("The graph shows the correlation among the variables. It helps to explore correlations through visualisation. You may use it to understand better the relations and build models."),
            br(),br(),
            plotOutput("plot_4"),
            strong("NOTE"),
            h5("The graph shows the values of correlation  among all the numeric variables of the imported dataset."),
          )
        )
      ),
      tabItem(tabName= "about", 
        h3("Climate Historical data in Ottawa"),
        h4("The data set used in this project are historical weather, climate data, and related information for numerous locations across Canada."),
        h4("Temperature, precipitation, degree days, relative humidity, wind speed and direction, monthly summaries, averages, extremes etc are observed daily."),
        h4("In this dataset the station where data are registered is: OTTAWA GATINEAU A"),
        h4("The data are automatically  imported from the official web page: The official website of the Government of Canada link:"),
        a("Link", href="https://climate.weather.gc.ca/"),
        h4("After imported from the official link the dataset is pre-processed. The empty columns are removed automatically and the variables are saved as: time; numeric; factor."),
        strong("NOTE"),
        h4(" At the end of each graph you will se a Note explaining the output and how you may use the results."),
        h4("This is a course project and results should not be taken as a research conclusions."),
        h3("30 September 2021"),
        h3(p(span("Author", style = "color:red"))),
        HTML('
        <div style="clear: left;">
        <img src="https://storage.googleapis.com/kaggle-avatars/images/5307185-gr.jpg" alt="" style="height: 274px; width: 244px; "> </div>
        <p>
        <a href="https://al.linkedin.com/in/eralda-dhamo-gjika-71879128" target="_blank"> Eralda Gjika (Dhamo)</a><br>
        Master Student, School of Mathematics and Statistics<br>
        Carleton University<br>
        <a href="https://twitter.com/EraldaGjika" target="_blank">Twitter</a><br>
        <a href="https://al.linkedin.com/in/eralda-dhamo-gjika-71879128" target="_blank">Linkedin</a> <br/>
        </p>')
      )
    )
  )
)

