library(tidyverse)
library(palmerpenguins)
library(janitor)
library(cluster) # This library have the function daisy() which we will use to
# calculate the distances between points.

penguins |> 
  mutate(key = str_c("id_", 1:344)) |> 
  select(key, everything()) -> penguins
# Let's just match with two variables "flipper_length_mm" and "flipper_length_mm"
penguins |> 
  select(key, bill_length_mm,  flipper_length_mm) -> penguins_sapales
# Need to scale the variables so that the method doesn't render ridiculous outcomes.
penguins_sapales |> 
  mutate(across(.cols = where(is.numeric),
                .fns = scale)) |> 
  drop_na() -> penguins_sapales

neighbours_to_match <-  3 # neighbors to match to each point. (3 is just an example,
# you can add any other number)

penguins_sapales |> 
  mutate(
    original_tibble = list(penguins_sapales) # Add the original tibble to each point.
  ) |> 
  mutate( # Add the point of each row to a new nested tibble called "row_point"
    row_point = pmap(
      list(.x = bill_length_mm, .y = flipper_length_mm, 
           .z = key),
      .f = function(.x, .y, .z){
        tibble(
          key = .z,
          bill_length_mm = .x,
          flipper_length_mm = .y
        )
      })
    )|> 
  # Let's prune the original tibble, so that it can be separated from the point
  # we want to match (That's why we use anti_join).
  mutate(
    original_tibble = map2(.x = original_tibble, .y = row_point,
                .f = function(.x, .y){
                  .x |> 
                    anti_join(.y, by = "key")
                })
  )  |> 
  mutate(
    original_tibble = map(original_tibble, function(original_tibble){
      original_tibble |> select(-key)
    }),
    row_point = map(row_point, function(row_point){
      row_point |> select(-key)
    })
  ) |> 
  select(-c(bill_length_mm, flipper_length_mm)) |> 
  mutate(both = map2(.x = row_point, .y = original_tibble,
                     .f = function(.x, .y){
                       .x |> 
                         bind_rows(.y) # Needed for the function daisy() 
  # All points must be on the same data.frame AND we need the row_point to be on the top.
                     })) |> 
  mutate(distance = map(.x = both, .f = function(.x){
    .x |> as.matrix() |> daisy(metric = "euclidean") 
  })) |> 
  mutate(neighbours = map(.x = distance, .f = function(.x){
    .x |> as.matrix() |> 
      as.data.frame() |> 
      rownames_to_column() |> 
      as_tibble() |> 
      filter(rowname != 1) |>  # First row is just the distance between the point with itself
      janitor::clean_names() |> 
      arrange(x1) |> 
      slice(1:neighbours_to_match) # This is the key.
  })) 
  
