#' Collecte de l'utilisateur courant
#'
#' Cette fonction permet de collecter le nom de l'utilisateur de la session en cours (Unix et Windows)
#' @name session_utilisateur
#' @export
#' @examples
#' utilisateur <- session_utilisateur()

session_utilisateur <- function(
)
{
  
  #### Collecte ####
  user <- if (.Platform$OS.type == "windows") {
    Sys.getenv("USERNAME")
  } else {
    Sys.getenv("USER")
  }
  
  # Repli si la variable d'environnement est vide
  if (user == "") {
    user <- Sys.info()[["user"]]
  }
  
  #### Sortie ####
  return(user)
  
} # Fin de la fonction
