library(shiny)
library(readr)
library(tidyverse)
library(scales)
library(DT)
library(corrplot)


# Load necessary libraries
library(tidymodels)
library(dplyr)
library(ggeasy)
library(themis)

# Load the dataset
StrokeData <- read.csv("healthcare-dataset-stroke-data.csv")


# Convert necessary variables to factors and select relevant columns
StrokeData <- StrokeData %>%
  mutate(across(c(Gender, Hypertension, HeartDisease, EverMarried,
                  WorkType, ResidenceType, SmokingStatus, Stroke), as.factor)) %>%
  select(Gender, Age, Hypertension, HeartDisease, EverMarried, WorkType, ResidenceType, AvgGlucoseLevel, BMI, SmokingStatus, Stroke)


StrokeData <- StrokeData %>%
  filter(Age >= 18) %>%
  mutate(
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Healthy Weight",
      BMI < 30 ~ "Overweight",
      BMI < 35 ~ "Obese",
      TRUE ~ "Severely Obese"
    ),
    Glucose_Category = case_when(
      AvgGlucoseLevel < 70 ~ "Very Low",
      AvgGlucoseLevel < 100 ~ "Low",
      AvgGlucoseLevel < 126 ~ "Healthy",
      AvgGlucoseLevel < 200 ~ "High",
      TRUE ~ "Very High"
    )
  ) %>%
  select(Gender, Age, Hypertension, HeartDisease, EverMarried, WorkType, ResidenceType, SmokingStatus, Stroke, BMI_Category, Glucose_Category)



# Define the recipe
stroke_recipe <- recipe(Stroke ~ ., data = StrokeData) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_impute_mode(all_nominal_predictors(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.6) %>% #remember it was 0.8
  step_zv(all_predictors()) %>%  # Remove zero-variance predictors
  step_smote(Stroke)  

stroke_recipe |> 
  prep() |> 
  bake(new_data = StrokeData)


stroke_spec <- 
  logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm") 

stroke_spec

stroke_workflow <- 
  workflow() %>% 
  add_recipe(stroke_recipe) %>% 
  add_model(stroke_spec) 

stroke_workflow

model1 <- 
  fit(stroke_workflow, data = StrokeData) 

model1

stroke_model <- 
  model1 |>
  tidy(exponentiate = TRUE)

stroke_model



coefs <- model1 %>% 
  tidy() %>%
  filter(term != "(Intercept)")

# Plot the coefficients
ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Effect Sizes of Predictors in Logistic Regression Model",
       x = "Coefficient Estimate",
       y = "Predictors") +
  theme_minimal()





augment(model1, StrokeData) |> 
  select(Stroke, .pred_class, .pred_0, .pred_1) |> 
  conf_mat(Stroke, .pred_class)

#This is the end of what I copy and pasted
# Load the stroke model
#model1 <- readRDS("StrokeModel.R")
data <- read.csv("healthcare-dataset-stroke-data.csv")
data <- data %>% mutate(BMI = as.numeric(BMI)) %>%
  filter(!is.na(BMI)) %>%
  mutate(Hypertension = case_when(
    Hypertension == 1 ~ "Yes",
    Hypertension == 0 ~ "No",
    .default = NA_character_ #Handle cases where it might NA
  )) %>%
  mutate(HeartDisease = case_when(
    HeartDisease == 1 ~ "Yes",
    HeartDisease == 0 ~ "No",
    .default = NA_character_ #Handle cases where it might NA
  )) %>%
  mutate(Stroke = case_when(
    Stroke == 1 ~ "Yes",
    Stroke == 0 ~ "No",
    .default = NA_character_ #Handle cases where it might NA
  ))


shinyServer(function(input, output){
  
  
  output$scatterplot <- renderPlot({
    
    if (input$xvar != "Select X Variable" && input$yvar != "Select Y Variable") {
      
      # ggplot(data, aes(data[[input$xvar]], data[[input$yvar]])) +
      #   geom_point(aes(color = data[[input$catvar]])) + labs(x = input$xvar, y = input$yvar)
      # + scale_y_continuous(breaks = y_breaks)
      # + ylim(min(data[[input$yvar]],na.rm=T), max(data[[input$yvar]],na.rm=T))
      ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(aes(color = data[[input$catvar]])) +
        labs(x = input$xvar, y = input$yvar) +
        scale_color_discrete(name = input$catvar) +
        scale_y_continuous(breaks = seq(min(data[[input$yvar]], na.rm = TRUE), max(data[[input$yvar]], na.rm = TRUE), by = 10))
      
    } else {
      # If one or both variables are not selected, display an empty plot
      return(plot(1, type = "n", xlab = "", ylab = "", main = "Select variables to plot"))
    }
  })
  
  output$dynamic_inputs <- renderUI({
    predictor_inputs <- lapply(input$predictors, function(pred) {
      if (pred == "Gender") {
        selectInput(
          inputId = "Gender",
          label = "Gender:",
          choices = c("Male", "Female"),
          selected = "Male"
        )
      }
      else if (pred == "Age"){
        sliderInput(
          inputId = "Age",
          label = "Age",
          18, 90, 30
        )
      }
      else if (pred == "Hypertension"){
        selectInput(
          inputId = "Hypertension",
          label = "Hypertension",
          choices = c("Yes", "No"),
          selected = "Yes"
        )
      }
      else if (pred == "HeartDisease"){
        selectInput(
          inputId = "HeartDisease",
          label = "Heart Disease",
          choices = c("Yes", "No"),
          selected = "Yes"
        )}
      else if (pred == "EverMarried"){
        selectInput(
          inputId = "EverMarried",
          label = "Ever Married",
          choices = c("Yes", "No"),
          selected = "Yes"
        )}
      else if (pred == "WorkType"){
        selectInput(
          inputId = "WorkType",
          label = "Work Type",
          choices = c("Private", "Government", "Self-Employed", "Never Worked"),
          selected = "Private"
        )}
      else if (pred == "ResidenceType"){
        selectInput(
          inputId = "ResidenceType",
          label = "Residence Type",
          choices = c("Urban", "Rural"),
          selected = "Urban"
        )}
      else if (pred == "AvgGlucoseLevel"){
        numericInput(
          inputId = "AvgGlucoseLevel",
          label = "Average Glucose Level",
          min = 50, max = 300, value = 140
        )}
      else if (pred == "SmokingStatus"){
        selectInput(
          inputId = "SmokingStatus",
          label = "Smoking Status",
          choices = c("Never smoked", "Smokes", "Formerly smoked", "Unknown"),
          selected = "Never Smoked"
        )}
      else if (pred == "Height & Weight"){
        list(
          tagList(
            numericInput(
              inputId = "Height",
              label = "Height in inches",
              min = 0, max = 120, value = 66
            ),
          ),
          tagList(
            numericInput(
              inputId = "Weight",
              label = "Weight in lbs",
              min = 50, max = 300, value = 140
            )
          )
        )
      }
      else {
        numericInput(
          inputId = paste0(pred),
          label = pred,
          value = 0
        )
      }
    })
    
    #Split the list of UI elements into two equal parts
    n <- length(predictor_inputs)
    
    if (n == 1) {
      # If there is only one predictor, place it in a single column
      fluidRow(
        column(12, do.call(tagList, predictor_inputs))
      )
    } else {
      
      half <- ceiling(n / 2)
      col1 <- predictor_inputs[1:half]
      col2 <- predictor_inputs[(half + 1):n]
      
      # Arrange the elements in two columns
      fluidRow(
        column(6, do.call(tagList, col1)),
        column(6, do.call(tagList, col2))
      )
    }
  })
  ########################################################Boxplots
  output$ageboxplot <- renderPlot({
    data %>% 
      ggplot(aes(x=Age)) +
      geom_boxplot()
  })
  output$bmiboxplot <- renderPlot({
    data %>% 
      ggplot(aes(x=BMI)) +
      geom_boxplot()
  })
  output$glucoseboxplot <- renderPlot({
    data %>% 
      ggplot(aes(x=AvgGlucoseLevel)) +
      geom_boxplot()
  })
  #######################################################Histograms
  output$agehistogram <- renderPlot({
    data %>% 
      ggplot(aes(x=Age)) +
      geom_histogram()
  })
  output$bmihistogram <- renderPlot({
    data %>% 
      ggplot(aes(x=BMI)) +
      geom_histogram()
  })
  output$glucosehistogram <- renderPlot({
    data %>% 
      ggplot(aes(x=AvgGlucoseLevel)) +
      geom_histogram()
  })
  ################################################Frequency Bar Graphs
  output$genderfreq <- renderPlot({
    data %>% 
      ggplot(aes(x = Gender)) +
      geom_bar() +
      labs(title = "Gender Frequency", x = "Category", y = "Count")
  })
  output$hyperfreq <- renderPlot({
    data %>% 
      ggplot(aes(x = Hypertension)) +
      geom_bar() +
      labs(title = "Hypertension Frequency", x = "Category", y = "Count")
  })
  output$heartfreq <- renderPlot({
    data %>% 
      ggplot(aes(x = HeartDisease)) +
      geom_bar() +
      labs(title = "Heart Disease Frequency", x = "Category", y = "Count")
  })
  output$everfreq <- renderPlot({
    data %>% 
      ggplot(aes(x = EverMarried)) +
      geom_bar() +
      labs(title = "Ever Married Frequency", x = "Category", y = "Count")
  })
  output$workfreq <- renderPlot({
    data %>% 
      ggplot(aes(x = WorkType)) +
      geom_bar() +
      labs(title = "Work Type Frequency", x = "Category", y = "Count")
  })
  output$resfreq <- renderPlot({
    data %>% 
      ggplot(aes(x = ResidenceType)) +
      geom_bar() +
      labs(title = "Residence Type Frequency", x = "Category", y = "Count")
  })
  output$smokefreq <- renderPlot({
    data %>% 
      ggplot(aes(x = SmokingStatus)) +
      geom_bar() +
      labs(title = "Smoking Status Frequency", x = "Category", y = "Count")
  })
  output$strokefreq <- renderPlot({
    data %>% 
      ggplot(aes(x = SmokingStatus)) +
      geom_bar() +
      labs(title = "Stroke Frequency", x = "Category", y = "Count")
  })
  
  
  
  
  result <- reactiveVal(NULL)
  savedResults <- reactiveVal(list())
  
  
  
  observeEvent(input$runRegression, {
    
    # if (length(input$predictors) == 0) {
    #   regressionResult("Please select at least one predictor.")
    #   return()
    # }
    
    new_data <- data.frame(
      Age = if ("Age" %in% input$predictors) input$Age else 0,
      Gender = if ("Gender" %in% input$predictors) input$Gender else "",
      Hypertension = if ("Hypertension" %in% input$predictors) input$Hypertension else "",
      HeartDisease = if ("HeartDisease" %in% input$predictors) input$HeartDisease else "",
      AvgGlucoseLevel = if ("AvgGlucoseLevel" %in% input$predictors) input$AvgGlucoseLevel else "",
      BMI = if ("Height & Weight" %in% input$predictors) (703*input$Weight / ((input$Height) ^ 2)) else "",
      EverMarried = if ("EverMarried" %in% input$predictors) input$EverMarried else "",
      WorkType = if ("WorkType" %in% input$predictors) input$WorkType else "",
      ResidenceType = if ("ResidenceType" %in% input$predictors) input$ResidenceType else "",
      SmokingStatus = if ("SmokingStatus" %in% input$predictors) input$SmokingStatus else ""
    )
    
    # Ensure all required columns are present in new_data
    all_columns <- c("Age", "Gender", "Hypertension", "HeartDisease", "AvgGlucoseLevel", "BMI", "EverMarried", "WorkType", "ResidenceType", "SmokingStatus")
    for (col in all_columns) {
      if (!col %in% colnames(new_data)) {
        new_data[[col]] <- NA
      }
    }
    
    # Remove columns not selected by the user
    new_data <- new_data[, colSums(is.na(new_data)) == 0, drop = FALSE]
    
    # Check for empty data frame
    # if (nrow(new_data) == 0 || ncol(new_data) == 0) {
    #   output$prediction <- renderText({
    #     "Please select at least one variable and provide its value."
    #   })
    #   return()
    # }
    
    # Ensure factor levels are consistent with training data
    levels_gender <- c("Male", "Female")
    levels_hypertension <- c("0", "1")
    levels_heartdisease <- c("0", "1")
    levels_evermarried <- c("No", "Yes")
    levels_worktype <- c("Private", "Self-employed", "Govt_job")
    levels_residencetype <- c("Urban", "Rural")
    levels_smokingstatus <- c("formerly smoked", "never smoked", "smokes", "Unknown")
    
    new_data$Gender <- factor(new_data$Gender, levels = levels_gender)
    new_data$Hypertension <- factor(new_data$Hypertension, levels = levels_hypertension)
    new_data$HeartDisease <- factor(new_data$HeartDisease, levels = levels_heartdisease)
    new_data$EverMarried <- factor(new_data$EverMarried, levels = levels_evermarried)
    new_data$WorkType <- factor(new_data$WorkType, levels = levels_worktype)
    new_data$ResidenceType <- factor(new_data$ResidenceType, levels = levels_residencetype)
    new_data$SmokingStatus <- factor(new_data$SmokingStatus, levels = levels_smokingstatus)
    
    # Categorize BMI and GlucoseLevel
    if ("BMI" %in% colnames(new_data)) {
      new_data$BMI_Category <- case_when(
        new_data$BMI < 18.5 ~ "Underweight",
        new_data$BMI < 25 ~ "Healthy Weight",
        new_data$BMI < 30 ~ "Overweight",
        new_data$BMI < 35 ~ "Obese",
        TRUE ~ "Severely Obese"
      )
      new_data <- new_data %>% select(-BMI)
    }
    if ("AvgGlucoseLevel" %in% colnames(new_data)) {
      new_data$Glucose_Category <- case_when(
        new_data$AvgGlucoseLevel < 70 ~ "Very Low",
        new_data$AvgGlucoseLevel < 100 ~ "Low",
        new_data$AvgGlucoseLevel < 126 ~ "Healthy",
        new_data$AvgGlucoseLevel < 200 ~ "High",
        TRUE ~ "Very High"
      )
      new_data <- new_data %>% select(-AvgGlucoseLevel)
    }
    print(new_data)
    
    # Predict using the stroke model
    strokePrediction <- predict(model1, new_data, type = "prob")
    
    print("strokePrediction:")
    print(strokePrediction)
    
    # Saving results
    result(strokePrediction$.pred_1[1]*100)
    #result <- list(model_summary = summary(model1), strokePrediction, input$predictors)
    #regressionResult(result)
    
    # Output the prediction
    output$prediction <- renderText({
      if (nrow(strokePrediction) > 0) {
        paste("Probability of having a stroke:", round(result(), 4), "%")
      } else {
        "Prediction could not be made. Please check your inputs."
      }
    })
    
    
    
    # Calculate and display model coefficients
    coefs <- model1 %>%
      tidy() %>%
      filter(grepl(paste0("^(", paste0(colnames(new_data), collapse="|"), ")"), term) & term != "(Intercept)")
    
    output$importance_text <- renderUI({
      HTML(paste("Model Coefficients:<br>",
                 paste(coefs$term, round(coefs$estimate, 4), sep = ": ", collapse = "<br>")))
    })
    
    output$importance_plot <- renderPlot({
      ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
        geom_point() +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(x = "Coefficient Estimate",
             y = "Predictors") +
        theme_minimal()
    })
    
    # Generate sentences for each coefficient
    coefficient_sentences <- coefs %>%
      mutate(
        color = ifelse(estimate > 0, "red", "green"),
        sentence = paste0("<span style='color:", color, "'>", term, ": ", round(estimate, 4), "</span>")
      ) %>%
      pull(sentence) %>%
      paste(collapse = "<br>")
    
    output$analysis <- renderUI({
      HTML(paste("Analysis of Coefficients:<br>", coefficient_sentences))
    })
    
    # Calculate and display correlation matrix
    if (length(input$predictors) > 1) {
      # Convert all factor and character columns to numeric via one-hot encoding
      StrokeData_numeric <- new_data %>%
        mutate(across(where(is.character), as.factor)) %>%  # Ensure all character columns are factors
        mutate(across(where(is.factor), as.numeric)) %>%   # Convert factors to numeric if ordinal; otherwise use dummy variables
        # For non-ordinal factors, create dummy variables
        mutate(across(where(is.factor), ~ as.numeric(.x == levels(.x)[2]))) %>%
        # Optionally, drop original non-numeric columns if they were not transformed in-line
        select(where(is.numeric))
      
      # Compute the correlation matrix using complete cases to handle any NAs
      correlation_matrix <- cor(StrokeData_numeric)
      
      # Print the correlation matrix
      output$correlation_plot <- renderPlot({
        corrplot(correlation_matrix, method = "color")
      })
    } else {
      output$correlation_plot <- renderPlot({
        plot.new()
        text("Not enough variables selected for correlation matrix")
      })
    }
    
    
  })
  
  
  
  
  
  
  
  # Rendering regression results
  output$regressionResults <- renderPrint({
    if (is.null(result())) {
      "Run the regression to see the results."
    } else {
      paste("Stroke probability:", round(result(), 4), "%")
    }
  })
  
  # Rendering saved results table
  output$savedResultsTable <- renderDT({
    if (length(savedResults()) == 0) {
      return()
    } else {
      datatable(
        data.frame(
          paste(savedResults())
        ),
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          class = 'cell-border stripe'
        ),
        colnames = c("Probability"),
        style = 'jqueryui'
      )
      # %>% 
      #   formatStyle(
      #     'Probability',
      #     backgroundColor = styleInterval(20, c('lightblue', 'lightgreen'))
      #   )
    }
  })
  
  # Observing event to save results
  observeEvent(input$saveResult, {
    if (is.null(result())) {
      return ()
    } else {
      savedResults(c(savedResults(), result()))
    }
  })
  
  # Observing event to clear table
  observeEvent(input$clearTable, {
    output$prediction <- renderText({""})
    result(NULL)
    savedResults(list())
  })
  
  
  
  
})

