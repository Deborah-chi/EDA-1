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

# Calculation of total casualty -------------------------------------------

crime_tbl_casualty <- crime_tbl_casualty |> 
  mutate( total_casualty = case_when
          ( !is.na(fatalities) & !is.na(injured) 
            ~ fatalities + injured,
            .default = NA_integer_ 
            ),
          .after = injured 
          )

crime_tbl_casualty

# Variable analysis (number of recorded attack by country) ---------------------

crime_tbl_casualty |> 
  count(country) |> 
  rename(number_of_attacks = n) |> 
  arrange(desc(number_of_attacks)
        )

table(crime_tbl_casualty$country)       # Alternatively

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

# Data Visualization ------------------------------------------------------

ggplot( 
  data = crime_tbl,
  mapping = aes(x = fct_infreq(country))
) + 
  geom_bar( 
    colour = "red", fill = "navy" 
  )

## Labeling the axis -------------------------------------------------------

ggplot( 
  data = crime_tbl,
  mapping = aes(x = fct_infreq(country))
) + 
  geom_bar( 
    colour = "red", fill = "navy" 
  ) + 
  labs( 
    x = "Country", 
    y = "Number of Attacks",
    title = "Number of recorded attacks by country from April 1974 to December 2020", 
    subtitle = "Nigeria and Cameroon have more recorded attacks than the other countries in this dataset.", 
    caption = "Data source: Kaggle | By: Deborah Elesie" 
  )

## Addition of theme -------------------------------------------------------

ggplot(
  data = crime_tbl,
  mapping = aes(x = fct_infreq(country))
) +
  geom_bar(
    colour = "red",
    fill = "navy"
  ) +
  labs(
    x = "Country",
    y = "Number of Attacks",
    title = "Number of recorded attacks by country from April 1974 to December 2020",
    subtitle = "Nigeria and Cameroon have more recorded attacks than the other countries in this dataset.",
    caption = "Data source: Kaggle | By: Deborah Elesie"
  ) +
  theme_bw()

## Numerical variable visualization using total_casualty ---------------------

crime_tbl_casualty <- crime_tbl_casualty |> 
  mutate(
    total_casualty = fatalities + injured
    )

crime_tbl_casualty |> 
  summarize( 
    average_casualty = mean(total_casualty, na.rm = TRUE),
    median_casualty = median(total_casualty, na.rm = TRUE),
    variance = var(total_casualty, na.rm = TRUE),
    sd = sd(total_casualty, na.rm = TRUE),
    minimum = min(total_casualty, na.rm = TRUE),
    maximum = max(total_casualty, na.rm = TRUE) 
  )


### Visualization of total_casualty -----------------------------------------

crime_tbl_casualty |> 
  filter(!is.na(total_casualty)) |> 
  ggplot(aes(x = total_casualty)) + 
  geom_histogram(
    binwidth = 2.5,
    fill = "#f94500"
  ) + 
  labs(
    x = "total casualties",
    y = "Count",
    title = "Distribution of total casualties",
    subtitle = "Distribution is strongly right-skewed, with many observations near zero",
    caption = "Data source: Kaggle | By: Deborah Elesie"
  ) + 
  theme_light()

### Alternatively -----------------------------------------------------------

crime_tbl_casualty |> 
  filter(!is.na(total_casualty)) |> 
  ggplot(aes(x = total_casualty)) + 
  geom_density(
    col = "purple",
    linewidth = 0.6
  ) + 
  labs(
    x = "total casualty",
    y = "Density",
    title = "Distribution of total casualties",
    caption = "Data source: Kaggle | By: Deborah Elesie"
  ) + 
  theme_light()

# Plotting a box plot -----------------------------------------------------

falcon_tbl <- read_tsv("https://raw.githubusercontent.com/EU-Study-Assist/data-for-r4r/refs/heads/main/flatfiles/falcon.tsv")

head(falcon_tbl)

falcon_tbl |> 
  ggplot(aes(weight)) +
  geom_boxplot( 
    fill = "dodgerblue" 
  ) + 
  labs( 
    x = "Weight",
    title = "Distribution of falcon weight",
    caption = "Data source: Stats4nr package by Matthew Russell | Graph By: Deborah Elesie"
  ) + 
  coord_flip() + 
  theme_light()