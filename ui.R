
library(shiny)
library(shinythemes)
library(shiny)
library(readr)
library(tidyverse)

# shinyUI(
#     tags$head(
#       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
#     ),

UIStrokeData <- read.csv("healthcare-dataset-stroke-data.csv")

#navbarPage creates mutiple pages
navbarPage(
  #theme = shinytheme("darkly"), #Add your theme name here
  title = "Stroke Prediction Shiny App",
  
  # Defining each tab panel creates a new page
  tabPanel("Prediction",
           h1("Test your probability of getting a stroke"),
           sidebarLayout(
             sidebarPanel(
               #width = 7,
               div(class = "sidebar-panel",
                   checkboxGroupInput(
                     inputId = "predictors",
                     label = "Select Predictors:",
                     choices = c(colnames(UIStrokeData)[!(colnames(UIStrokeData) %in% c("Stroke", "id", "BMI"))], "Height & Weight"),
                     # Exclude the target variable
                     selected = colnames(UIStrokeData)[!(colnames(UIStrokeData) %in% c("Stroke", "id", "BMI"))[1]] 
                     # Select the first predictor by default
                   )
               ),
               uiOutput("dynamic_inputs")
             ),
             mainPanel(
               actionButton("runRegression", "Run Regression"),
               actionButton("saveResult", "Save Result"),
               actionButton("clearTable", "Clear Results"),
               verbatimTextOutput("regressionResults"),
               dataTableOutput("savedResultsTable")
             )
           )
  ),
  
  
  tabPanel( "Data Analysis",
            navlistPanel(
              tabPanel("3 term",
                       h1("3 term"),
                       sidebarLayout(
                         sidebarPanel(
                           # Id, title, and panel with choices using data set
                           selectInput("yvar", "Y Variable:", 
                                       c("Select Y Variable", colnames(select(UIStrokeData, Age, BMI, AvgGlucoseLevel)))),
                           selectInput("xvar", "X Variable:", 
                                       c("Select X Variable", colnames(select(UIStrokeData, Age, BMI, AvgGlucoseLevel)))),
                           selectInput("catvar", "Categorial Variable:",
                                       c("Select a Variable", colnames(select(UIStrokeData, -Age, -BMI, -id, -AvgGlucoseLevel))))
                         ),
                         mainPanel(
                           # creates scatterplot object
                           plotOutput("scatterplot")
                         )
                       )
              ),
              # tabPanel("Variable effects",
              #          plotOutput("importance_plot"),
              #          h3("Correlation Matrix"),
              #          plotOutput("correlation_plot")
              # ),
              tabPanel("Interval/Ratio", 
                       plotOutput("ageboxplot", height = "200px", width = "800px"), 
                       plotOutput("bmiboxplot", height = "200px", width = "800px"), 
                       plotOutput("glucoseboxplot", height = "200px", width = "800px"),
                       plotOutput("agehistogram", height = "200px", width = "800px"), 
                       plotOutput("bmihistogram", height = "200px", width = "800px"), 
                       plotOutput("glucosehistogram", height = "200px", width = "800px")
              ),
              tabPanel("Categorical", 
                       fluidRow(
                         column(6, plotOutput("genderfreq", height = "200px", width = "400px")),
                         column(6, plotOutput("hyperfreq", height = "200px", width = "400px"))
                       ),
                       fluidRow(
                         column(6, plotOutput("heartfreq", height = "200px", width = "400px")),
                         column(6, plotOutput("everfreq", height = "200px", width = "400px"))
                       ),
                       fluidRow(
                         column(6, plotOutput("workfreq", height = "200px", width = "400px")),
                         column(6, plotOutput("resfreq", height = "200px", width = "400px"))
                       ),
                       fluidRow(
                         column(6, plotOutput("smokefreq", height = "200px", width = "400px")),
                         column(6, plotOutput("strokefreq", height = "200px", width = "400px"))
                       ),
              ),
            )
  ),
  tabPanel("Coefficient Analysis",
           sidebarLayout(
             sidebarPanel(
               textOutput("prediction"),
               p("The red text represents coefficients that negatively affect your stroke outcome
                     and the green text represents coefficients that positively affect your stroke outcome"),
               h3("Analysis of Coefficients:"),
               htmlOutput("analysis"),
             ),
             mainPanel(
               h3("Effect Sizes of Predictors in Logistic Regression Model"),
               plotOutput("importance_plot"),
               #h3("Correlation Matrix"),
               #plotOutput("correlation_plot"),
               #h3("Model Coefficients"),
               #htmlOutput("importance_text"),
             )
           )
  )
  
  
)



