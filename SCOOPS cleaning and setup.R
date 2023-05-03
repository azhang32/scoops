## Load libraries
library(tidyverse)
library(dplyr)

# Set directory and import raw dataset
setwd("C:/Users/azhang/Box/Amy Zhang's Files/Maya")
rawdata <- read.csv("Full data_05022023.csv")


## Prep weights - Calculate overall meat weights per day x meal x station
wts <- rawdata |> 
  # Rename dish to meat type
  rename(meat_type = before_dish) |>
  
  # Recode condition to numeric values
  mutate(condition = fct_recode(condition, "0" = "control", "1" = "intervention")) |>
  
  # Set missing numeric values to 0 so that weights can be added below
  mutate(across(after_empty_wt_f:after_part_wt2,~replace_na(.x, 0))) |>
  
  # Calculate total before weight and total after weights per row entered, subtracting weight of empty trays from each
  mutate(before_total_wt = before_num_trays*before_avg_wt - before_num_trays*before_empty_wt) |> 
  mutate(after_full_wt = (after_num_full*before_avg_wt - after_num_full*after_empty_wt_f)) |>   # Weight of full trays after meal
  mutate(total_part_wt= after_part_wt - after_empty_wt_p,    # Weight of partial tray 1
         total_part_wt2 = after_part_wt2 - after_empty_wt_p2,   # Weight of partial tray 2 if multiple partial trays were reported
         after_total_wt = after_full_wt + total_part_wt + total_part_wt2,    # Sum of full + partial trays after meal
         row_delta = before_total_wt-after_total_wt)    # row_delta is total amount consumed 
  

## Calculate total meat consumed in entire dining hall during each meal period, as well as at target station
wts_meal <- wts |>
  
  # Collapsed by day, meal, station
  group_by(condition, week, day, meal, before_station) |>
  summarise(weight_dinner=sum(row_delta[meal=="dinner"]), 
            weight_lunch=sum(row_delta[meal=="lunch"]), 
            weight_target_lunch = sum(row_delta[before_station=="Fireside"])) |>
  ungroup() 

## Collapsed by day, meal to get overall weights at lunch and dinner (regardless of station)
wts_total <- na.omit(wts_meal) |>
  group_by(condition, week, day, meal) |>
  summarise(weight_total_dinner = sum(weight_dinner[meal=="dinner"]),
            weight_total_lunch = sum(weight_lunch[meal=="lunch"] )) |>
  ungroup()
  



## Set up statistical models for primary and secondary hypotheses
# Separate out target lunch, overall lunch, and overall dinner weights
target_lunch <- wts_meal |> filter(before_station == "Fireside") |> select(condition, week, day, weight_target_lunch)
study_dinner <- wts_total |> filter(meal == "dinner")
study_lunch <- wts_total |> filter(meal == 'lunch')

  # Primary hypothesis 1: control/intervention weight difference for target station
  model1 <- lm(weight_target_lunch ~ condition, data = target_lunch)
  summary(model1)
      
  # Secondary hypothesis 2: control/intervention weight difference for entire dining hall at lunch and dinner
  model2 <- lm(weight_total_lunch ~ condition, data = study_lunch)
  summary(model2)
  
  model3 <- lm(weight_total_dinner ~ condition, data = study_dinner)
  summary(model3)
  
  # Secondary hypothesis 3: control/intervention satisfaction differences from survey. Use survey data set prepped in "Survey analysis prep.R"
  model4 <- lm(satisfaction_1 ~ condition, data=survey_clean)
  summary(model4)
  
  model5 <- lm(hunger_1 ~ condition, data = survey_clean)
  summary(model5)
  
  model6 <- lm(fullness_1 ~ condition, data = survey_clean)
  summary(model6)

  
  # Secondary hypothesis 4: all models above but with interaction of condition with meat type
    # Add in meat types to recalculate total weights consumed at meal times and target station
    wts_mealb <- wts |>
      
      # Collapsed by day, meal, station
      group_by(condition, week, day, meal, before_station, meat_type) |>
      summarise(weight_dinner=sum(row_delta[meal=="dinner"]), 
                weight_lunch=sum(row_delta[meal=="lunch"]), 
                weight_target_lunch = sum(row_delta[before_station=="Fireside"])) |>
      ungroup() 
    
    # Collapsed by day, meal to get overall weights at lunch and dinner (regardless of station)
    wts_totalb <- na.omit(wts_mealb) |>
      group_by(condition, week, day, meal, meat_type) |>
      summarise(weight_total_dinner = sum(weight_dinner[meal=="dinner"]),
                weight_total_lunch = sum(weight_lunch[meal=="lunch"] )) |>
      ungroup()
    
    # Separate out target lunch, overall lunch, and overall dinner weights
    target_lunchb <- wts_mealb |> filter(before_station == "Fireside") |> select(condition, week, day, weight_target_lunch, meat_type)
    study_dinnerb <- wts_totalb |> filter(meal == "dinner")
    study_lunchb <- wts_totalb |> filter(meal == 'lunch')
  
    # Model 1 x meat type
    model1b <- lm(weight_target_lunch ~ condition + meat_type + condition:meat_type, data = target_lunchb)
    summary(model1b)
    
    # Secondary hypothesis 2: control/intervention weight difference for entire dining hall at lunch and dinner
    model2b <- lm(weight_total_lunch ~ condition + meat_type + condition:meat_type, data = study_lunchb)
    summary(model2b)
    
    model3b <- lm(weight_total_dinner ~ condition + meat_type + condition:meat_type, data = study_dinnerb)
    summary(model3b)
    
  # Secondary hypothesis 3: control/intervention satisfaction differences from survey. Use survey data set prepped in "Survey analysis prep.R"
  ## Need to figure out how to add meat type to survey day/week
    model4 <- lm(satisfaction_1 ~ condition, data=survey_clean)
    summary(model4)
    
    model5 <- lm(hunger_1 ~ condition, data = survey_clean)
    summary(model5)
    
    model6 <- lm(fullness_1 ~ condition, data = survey_clean)
    summary(model6)
