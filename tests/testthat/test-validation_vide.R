library(dplyr)
library(ecotools)
library(sf)
library(tibble)
library(testthat)

jeu_test <- tibble(x = 1:5) |> mutate(y = x)
jeu_test_avec_vide <- jeu_test |> add_row(x = 2, y = NA)
jeu_test_spatial <- tibble(x = 1:5) |> mutate(y = x) |> mutate(z = x) |> sf::st_as_sf(coords = c("x", "y"),
                                             crs = 4326)
jeu_test_spatial_avec_vide_geom <- 
  jeu_test_spatial |> 
  mutate(geometry = replace(geometry, 3, st_point()))  # 3e ligne -> POINT EMPTY
  
jeu_test_spatial_avec_vide_geom_et_data <- 
  jeu_test_spatial |> 
  mutate(z = ifelse(z == 2, NA, z)) |> 
  mutate(geometry = replace(geometry, 3, st_point())) # 3e ligne -> POINT EMPTY

test_that("jeu_test validite", {
  
  # Valeurs pas au bon format
  NA |>
    validation_vide() |>
    expect_error()
  
  # Valeurs pas au bon format
  c(1, 4) |>
    validation_vide() |>
    expect_error()
  
  # Valeurs valides
  jeu_test |>
    validation_vide() |>
    expect_no_error()
  
  # Valeurs simples absentes
  jeu_test_avec_vide |>
    validation_vide() |>
    expect_no_error()
  
  # Valeurs geom absentes
  jeu_test_spatial_avec_vide_geom |>
    validation_vide() |>
    expect_no_error()
  
  # Valeurs simples et geom absentes
  jeu_test_spatial_avec_vide_geom_et_data |>
    validation_vide() |>
    expect_no_error()
  
})