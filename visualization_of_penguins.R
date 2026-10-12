
# Loading the dataset -----------------------------------------------------

pacman::p_load(tidyverse, skimr, car,
               babynames, palmerpenguins, janitor)

# Bivariate analysis ------------------------------------------------------

penguins <- palmerpenguins::penguins

head(penguins)

# Visualization of dataset ------------------------------------------------

penguins |> 
  ggplot(
    aes(island, fill = species)) + 
  geom_bar() + 
  labs( x = "Island",
        y = "Count",
        title = "Frequency of Penguins Measured in Palmer Archipelago" ) + 
  theme_light()

## Comparing the number of observation for each specie ---------------------

penguins |> 
  ggplot(
    aes(island,
        fill = species)) + 
  geom_bar(position = "dodge") + 
  labs( x = "Island",
        y = "Count",
        title = "Frequency of Penguins Measured in Palmer Archipelago" ) + 
  theme_light()

## Two numerical variables -------------------------------------------------

penguins |> 
  count(island, species)

### Plotting scatter plot ---------------------------------------------------

penguins |> 
  ggplot(aes(bill_length_mm, body_mass_g)) + 
  geom_point( 
    col = "#041cAB" 
    ) + 
  labs( 
    x = "Bill length (mm)",
    y = "Body mass (g)",
    title = "Association between bill length and body mass",
    subtitle = "Bill length and body mass show a positive association in these data." 
    ) + 
  theme_light()

### Adding a fitted trend line to show the positive relationship ------------

penguins |> 
  ggplot(aes(bill_length_mm, body_mass_g)) + 
  geom_point( 
    col = "#041cAB" 
    ) + 
  geom_smooth( 
    se = FALSE,
    col = "#f94fe3" 
    ) + 
  labs( 
    x = "Bill length (mm)",
    y = "Body mass (g)",
    title = "Association between bill length and body mass",
    subtitle = "Bill length and body mass show a positive association in these data." 
    ) + 
  theme_light()

# Linear relationship -----------------------------------------------------

penguins |> 
  ggplot(aes(bill_length_mm, body_mass_g)) + 
  geom_point( 
    col = "#041cAB" 
    ) + 
  geom_smooth( 
    se = FALSE,
    col = "#f94fe3", 
    method = "lm",
    formula = y ~ x 
    ) + 
  labs( 
    x = "Bill length (mm)",
    y = "Body mass (g)",
    title = "Association between bill length and body mass",
    subtitle = "Bill length and body mass show a positive association in these data." 
    ) + 
  theme_light()

# One numerical and categorical visualization -----------------------------

penguins |> 
  summarise( 
    .by = species,
    average_body_mass = mean(body_mass_g, na.rm = TRUE)
    ) |> 
  ggplot(aes(species, average_body_mass)) + 
  geom_col( 
    fill = "lavenderblush4" 
    ) + 
  labs( 
    x = "Species",
    y = "Average body mass",
    title = "Average body mass by penguin species" 
    ) + 
  theme_light()

## Alternatively using a box plot ------------------------------------------

penguins |> 
  ggplot(aes(species, body_mass_g)) + 
  geom_boxplot( 
    fill = "#0e5fdc",
    col = "#041c43" 
    ) + 
  labs( 
    x = "Species", 
    y = "Body mass",
    title = "Distribution of body mass for each penguin species" 
    ) + 
  theme_light()

# Multivariate analysis ---------------------------------------------------

penguins |> 
  ggplot(aes(island, body_mass_g, fill = species)) + 
  geom_boxplot() + 
  labs( x = "Islands",
        y = "Body mass (g)",
        fill = "Species",
        title = "Body mass distribution of penguin species across the islands"
        ) + 
  theme_light(base_size = 7)
