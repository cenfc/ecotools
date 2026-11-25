#' Reformatage d'un paramètre de fonction
#'
#' Reformate un paramètre de fonction en NA si longueur nulle pour en faciliter le test ensuite
#' @name formatage_parametre
#' @param parametre Paramètre de fonction à reformater
#' @keywords donnees
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' formatage_parametre(parametre)
#' territoire <- territoire |> formatage_parametre()
#' territoire <- "SMDL" |> formatage_parametre()
#' territoire <- c("SMDL", "EPAGE HDHL") |> formatage_parametre()

formatage_parametre <- function(
  parametre)
  {

  #### Formatage ####
  parametre |>
    str_trim() |>
    na_if("")

} # Fin de la fonction
