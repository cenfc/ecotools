#' Chargement d'une connexion à une base de données
#'
#' Cette fonction permet de charger la connexion vers une base de données
#' @name BDD.connexion
#' @param dbname Nom de la base de données
#' @param hote Hôte de la base de données
#' @param port Port de la base de données
#' @param utilisateur Nom d'utilisateur
#' @param motdepasse Mot de passe du portefeuille
#' @import DBI
#' @import RPostgres
#' @export
#' @examples
#' bd_w <- BDD.connexion(dbname, hote, port, utilisateur, motdepasse)
#' bd_w <- BDD.connexion(dbname, hote, port, ecotools::session_utilisateur(), motdepasse)
#' bd_w <- BDD.connexion(dbname, hote, port, ecotools::session_utilisateur(), keyring::key_get(dbname, ecotools::session_utilisateur()))

BDD.connexion <- function(
    dbname = NA_character_,
    hote = NA_character_,
    port = NA_character_,
    utilisateur = NA_character_,
    motdepasse = NA_character_
)
{
  
  #### Test de cohérence ####
  test_dbname <- dbname %>% nchar();if(!(test_dbname %>% is.na())) {if(test_dbname == 0) dbname <- NA_character_}
  test_hote <- hote %>% nchar();if(!(test_hote %>% is.na())) {if(test_hote == 0) hote <- NA_character_}
  test_port <- port %>% nchar();if(!(test_port %>% is.na())) {if(test_port == 0) port <- NA_character_}
  test_utilisateur <- utilisateur %>% nchar();if(!(test_utilisateur %>% is.na())) {if(test_utilisateur == 0) utilisateur <- NA_character_}
  test_motdepasse <- motdepasse %>% nchar();if(!(test_motdepasse %>% is.na())) {if(test_motdepasse == 0) motdepasse <- NA_character_}
  
  if(is.na(dbname)) stop("Aucun dbname fourni")
  if(is.na(hote)) stop("Aucun hote fourni")
  if(is.na(port)) stop("Aucun port fourni")
  if(is.na(utilisateur)) stop("Aucun utilisateur fourni")
  if(is.na(motdepasse)) stop("Aucun motdepasse fourni")
  
  #### Calcul ####
  bd_c <- dbConnect(RPostgres::Postgres(),
                    dbname = dbname,
                    host = hote,
                    port = port,
                    user = utilisateur,
                    password = motdepasse,
                    base::list(sslmode = "require", connect_timeout = "10"),
                    service = NULL
  )
  
  #### Sortie ####
  return(bd_c)
  
} # Fin de la fonction
