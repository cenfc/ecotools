library(testthat)
library(ecotools)

test_that("formatage_parametre gère les différents cas", {
  # Valeurs valides conservées
  c("SMDL", "EPAGE HDHL") |>
    formatage_parametre() |>
    expect_equal(c("SMDL", "EPAGE HDHL"))
  
  # Espaces superflus supprimés
  c("  SMDL  ", "EPAGE HDHL ") |>
    formatage_parametre() |>
    expect_equal(c("SMDL", "EPAGE HDHL"))
  
  # Chaînes vides ou blanches converties en NA
  c("", "   ") |>
    formatage_parametre() |>
    is.na() |>
    expect_equal(c(TRUE, TRUE))
  
  # NA conservés
  NA_character_ |>
    formatage_parametre() |>
    is.na() |> 
    expect_true()
  
  # Cas mixte
  c("SMDL", " EPAGE HDHL", "", NA, "  ") |>
    formatage_parametre() |>
    expect_equal(c("SMDL", "EPAGE HDHL", NA, NA, NA))
})