library(tidyverse)
library(palmerpenguins)
library(janitor)
library(smotefamily)

penguins |> 
  mutate(key = str_c("id_", 1:344)) |> 
  select(key, everything()) -> penguins
# Let's just match with two variables "flipper_length_mm" and "flipper_length_mm"
penguins |> 
  select(key, bill_length_mm,  flipper_length_mm) -> penguins_sapales
#-----------------------------------
tidy_nearest <- function(.data, nearest_neighbours = 5,
                         key = "key"){
  .data <- .data |> drop_na() |> 
    mutate(across(.cols = where(is.numeric),
                  .fns = function(x){
                    (x - mean(x))/sd(x)
                  })) # scale the numeric variables, FIRST
  data_sapal <- .data |> 
    drop_na() |> 
    column_to_rownames(key) |> 
    as_tibble() 
  
  table_to_match <- data_sapal |> 
    as_tibble() |> 
    rownames_to_column(key)
  
  resultados_nearest_neighbour <- knearest(data_sapal,
           data_sapal,
           n_clust = nearest_neighbours) # STEP 1: Get the index of each row(knn)
  resultados <- resultados_nearest_neighbour |> 
    as_tibble() |> 
    rownames_to_column("key")  |> 
    nest(all_keys = -key) |>  # STEP 2
    mutate(all_keys = map(all_keys,
                            .f = function(all_keys){
                              all_keys |> unlist() |> 
                                unname()
                            })) |> 
    select(key, all_keys)
  resultados_finales <- table_to_match |> 
    left_join(resultados, by = "key") |> # STEP 3
    mutate(table_to_match = list(table_to_match)) 
  resultados_finales_2 <- resultados_finales |> 
    mutate(
      resultado_final = pmap(list(all_keys,
                                  table_to_match,
                                  key),
                             .f = function(all_keys, table_to_match,
                                           key){
                               keys_to_match <- c(all_keys, key)
                               table_to_match |> 
                                 filter(key %in% c(keys_to_match))
                             }) 
    )
  resultados_finales_2 |> 
    select(key, resultado_final)
}
all_keys

# DEMOSTRACIÓN

tidy_nearest(
  .data = penguins_sapales,
  nearest_neighbours = 3,
  key = "key"
) -> resultados_finalisimos

resultados_finalisimos |> pluck("resultado_final", 3)

