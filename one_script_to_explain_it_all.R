
library(readr)
library(dplyr)
library(mvabund)

# get the data
bird_baths <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv")

# get a random subsample of 15 birds
bird_sample <- bird_baths |> 
  filter(!is.na(urban_rural)) |> 
  select(bird_type) |> 
  unique() |> 
  slice_sample(n = 15) |> 
  pull()

# only include subsample of species and convert counts to occurrence
bird_occurrence <- bird_baths |> 
  filter(!is.na(urban_rural)) |> 
  filter(bird_type %in% bird_sample) |> 
  group_by(urban_rural, bird_type) |> 
  summarise(count = sum(bird_count)) |> 
  mutate(occurrence = if_else(count == 0, 0, 1)) |> 
  select(-count)

# fit a logistic regression model to test if 
# occurrences are dependent on species and location (rural/urban)
mod <- manyglm(occurrence ~ urban_rural * bird_type, 
               family = "binomial", 
               data = bird_occurrence)  
anova.mod <- anova(mod) 
anova.mod$table


