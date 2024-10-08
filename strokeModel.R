# Load necessary libraries
library(tidymodels)
library(dplyr)
library(ggeasy)
library(themis)

# Load the dataset
oldWd <- getwd()
oldWdLength <- nchar(oldWd)

newWd <- "a0123456789b0123456789c0123456789d0123456789e0123456789f0123456789"
newWdLength <- nchar(newWd)

addedWd <- "/StrokePredictionApplication"
addedWdLength <- nchar(addedWd)

substr(newWd, start = 0, stop = oldWdLength+1) <- oldWd
substr(newWd, start = oldWdLength+1, stop = newWdLength) <- addedWd

newWd <- substr(newWd, start = 0, stop = oldWdLength+addedWdLength)
print(newWd)

setwd(newWd)

#rm()
rm(newWd)
rm(newWdLength)
rm(oldWd)
rm(oldWdLength)
rm(addedWd)
rm(addedWdLength)


# if (oldwd = getwd())
# print(oldwd)


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

save(model1, file = "model.RData")

