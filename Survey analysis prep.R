library(tidyverse)
library(lubridate)
library(dplyr)

# Import survey file
survey_raw <- read_csv("SCOOPS_qualtrics_2023-05-02.csv")


# code the week number of each date
start_date <- ymd("2023-04-09")   # This is start date of week of study
end_date <- ymd("2023-05-06")     # Change end date as needed
weeks <- tibble(date = seq(start_date, end_date, by = "days")) |>
  mutate(week = rep(1:7, each = 7, length.out = n())) |>
  mutate(day = wday(date, label = TRUE))  # Add days of the week to each date

# Create subset of completed surveys and target measures
survey_clean <- survey_raw |>
  slice(3:n()) |> # first two rows are qualtrics metadata
  filter(Finished == 1) |> # Filter to those who finished the survey
  select(recorded = RecordedDate, target, meat_inclusion, satisfaction_1, hunger_1, fullness_1) |>  # Retain date of survey and satisfaction
  mutate(recorded =  as_datetime(recorded, format = "%m/%d/%Y %H:%M"), #recorded = as_datetime(recorded),
         hour = hour(recorded), date = date(recorded), after = recorded) |>
  left_join(weeks) |>
  mutate(week = if_else(date < start_date, as.integer(0), week)) |> # add week codes 
  mutate(condition = ifelse(week == 1 | week == 3| week == 4, "0", "1"))  # Code intervention condition: 1 = intervention, 0 = control week


