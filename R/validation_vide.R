#' Recherche d'éventuels NA/NULL dans l'ensemble des colonnes
#'
#' Recherche et dénombre les éventuels NA/NULL dans l'ensemble du jeu de données fourni (tibble/sf)
#' @name validation_vide
#' @param data Jeu de données à traiter
#' @import dplyr
#' @import glue
#' @import sf
#' @import tidyr
#' @export
#' @examples 
#' data |> validation_vide()
#' data |> validation_vide() |> mutate(liste = glue("Colonne {colonne} ({n_na} valeur(s) vide(s))")) |> formatage.liste("liste")

validation_vide <- function(  
    data = NA
    )
{

  #### Test de cohérence ####
  if(all(is.na(data)) == T) stop("Un jeu de données doit être fourni")
  if(!(any(class(data) %in% c("data.frame", "tibble", "sf")))) stop("Le jeu de données fourni doit être au format data.frame, tibble ou sf")
  
  #### Calcul ####
  ##### Colonnes non géométriques #####
  if(any(class(data) %in% c("data.frame", "tibble"))){
    resultat_non_geom <- 
      data |>
      st_drop_geometry() |>
      summarise(across(everything(), \(x) sum(is.na(x)))) |>
      pivot_longer(
        cols = everything(),
        names_to = "colonne",
        values_to = "n_na"
      ) |>
      filter(n_na > 0)
  }
  if(exists("resultat_non_geom") == FALSE) resultat_non_geom <- structure(list(colonne = character(0), n_na = integer(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                           "tbl", "data.frame"))
  
  ##### Colonnes non géométriques #####
  if(any(class(data) %in% c("sf"))){
    resultat_geom <- 
      data |>
      dplyr::rename(geom = dplyr::any_of("geometry")) |> 
      summarise(n_na = sum(st_is_empty(geom))) |>
      st_drop_geometry() |> 
      mutate(colonne = "geom", .before = n_na) |> 
      filter(n_na > 0)
  }
  if(exists("resultat_geom") == FALSE) resultat_geom <- structure(list(colonne = character(0), n_na = integer(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                     "tbl", "data.frame"))
                                                                                                                                                             
  ##### Regroupement #####
  resultat <- 
    resultat_non_geom |> 
    union(resultat_geom)
  
  #### Sortie ####
  return(resultat)

} # Fin de la fonction