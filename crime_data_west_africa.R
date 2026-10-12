# Loading the dataset -----------------------------------------------------

pacman::p_load(tidyverse, skimr, car,
               babynames, palmerpenguins, janitor)

## loading the library -----------------------------------------------------

library(tidyverse)

library(dplyr)

## Import dataset ----------------------------------------------------------

crime_tbl <- read_csv("https://raw.githubusercontent.com/EU-Study-Assist/data-for-r4r/refs/heads/main/flatfiles/crime_trend_west_africa.csv") |> 
  janitor::clean_names()

# Data Preview ------------------------------------------------------------

head(crime_tbl)

tail(crime_tbl)

head(crime_tbl, n = 10)

# Random sampling preview of data -----------------------------------------

set.seed(3423)
car::some(crime_tbl)

# Data structure and summary ----------------------------------------------

str(crime_tbl)

summary(crime_tbl)

## Dimensions of the data --------------------------------------------------

dim(crime_tbl)

# Removing un-neccessary column -------------------------------------------

crime_tbl <- crime_tbl |> 
  select(-gtid, -attack_type)

crime_tbl |> head ()

# Data integrity ----------------------------------------------------------

crime_tbl <- crime_tbl |> 
  mutate( country = factor(country),
          fatalities = as.integer(fatalities),
          injured = as.integer(injured) 
  )

## Preview the transformed data --------------------------------------------

crime_tbl |> 
  select(country, 
         fatalities, injured) |> 
  head()

## Investigate countries to see the number of countries --------------------

unique(crime_tbl$country)

### Skimr package to get more detailed summary -----------------------------

skim(crime_tbl)

skim_without_charts(crime_tbl)   # alternatively

# Detecting missing values ------------------------------------------------

crime_tbl |> summarise( across(everything(),
                               ~ sum(is.na(.x))) 
) |> 
  pivot_longer( cols = everything(),
                names_to = "variable",
                values_to = "n_missing" ) |> 
  arrange(desc(n_missing)
  )

## Missing proportion ------------------------------------------------------

crime_tbl |> summarise( across(everything(),
                               ~ mean(is.na(.x))) 
) |> 
  pivot_longer( cols = everything(),
                names_to = "variable",
                values_to = "missing_proportion") |> 
  arrange(desc(missing_proportion)
  )

### Investigating the row with missing values -------------------------------

crime_tbl |> 
  filter(is.na(fatalities) | is.na(injured)
  )

# Removing missing values -------------------------------------------------

crime_tbl_casualty <- crime_tbl |> 
  filter( !is.na(fatalities) | 
            !is.na(injured) 
  )