
get_data <- function() {
  
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv") 
    
}

get_bird_sample <- function(df) {
  
 df |>
    filter(!is.na(urban_rural)) |> 
    select(bird_type) |> 
    unique() |> 
    slice_sample(n = 15) |> 
    pull()
  
}

get_small_dataset <- function(df, bird_sample) {
  
  df |> 
    filter(!is.na(urban_rural)) |> 
    filter(bird_type %in% bird_sample) |> 
    group_by(urban_rural, bird_type) |> 
    summarise(count = sum(bird_count)) |> 
    mutate(occurrence = if_else(count == 0, 0, 1)) |> 
    select(-count)
  
}

fit_glm <- function(small_dataset) {
  
  mod <- manyglm(occurrence ~ urban_rural * bird_type, 
                 family = "binomial", 
                 data = small_dataset) |> 
    anova() |> 
    extract2("table")
  
}
