#' Notification vocale
#'
#' Cette fonction permet de créer une notification vocale
#' @name notification.vocale
#' @param message Message à diffuser
#' @import beepr
#' @export
#' @examples
#' notification.vocale("Ça y est, le traitement est terminé")

notification.vocale <- function(
  message = NA_character_
) {
  
  #### Vérification ####
  if(is.na(message)) stop("Message vide")
  
  #### Traitement ####
  os <- Sys.info()[["sysname"]]
  
  message_utf8 <- enc2utf8(message)
  
  if (os == "Darwin") {
    
    system(
      paste0("say '", message_utf8, "'"),
      intern = TRUE
    )
    
  } else if (os == "Windows") {
    
    commande <- sprintf(
      'PowerShell -Command "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8; Add-Type -AssemblyName System.Speech; $synth = New-Object System.Speech.Synthesis.SpeechSynthesizer; $synth.Speak([System.Text.Encoding]::UTF8.GetString([System.Text.Encoding]::UTF8.GetBytes(\'%s\')))"',
      message_utf8
    )
      
  } else if (os == "Linux") {
    
    if (nzchar(Sys.which("espeak-ng"))) {
      
      commande <- sprintf(
        "espeak-ng -v fr '%s'",
        message_utf8
      )
      
    } else if (nzchar(Sys.which("espeak"))) {
      
      commande <- sprintf(
        "espeak -v fr '%s'",
        message_utf8
      )
      
    } else if (nzchar(Sys.which("spd-say"))) {
      
      commande <- sprintf(
        "spd-say -l fr '%s'",
        message_utf8
      )
      
    } else {
      
      message("Aucun moteur de synthèse vocale trouvé (espeak-ng, espeak ou spd-say).")
      message("Installe-en un avec : sudo apt install espeak-ng")
      return(invisible(NULL))
      
    }

  } else {
    
    message("Synthèse vocale non disponible sur ce système : ", os)
    
  }

  if(os == "Windows" | os == "Linux") {
        
    beepr::beep(sound = 2)
    system(commande, intern = TRUE, wait = TRUE)
  }
  
}