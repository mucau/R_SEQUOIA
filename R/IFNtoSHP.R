#' @title IFNtoSHP
#' Recuperation des donnees .shp de l'IFN
#' @encoding UTF-8
#' @description 
#' La fonction \code{IGNtoSHP} permet de télécharger les fichiers shapefiles disponibles depuis le serveur de l'IFN.
#' @usage IFNtoSHP(code)
#' @param code CHARACTER. Code du fichier à télécharger depuis le serveur IFN
#' 
#' @details 
#' Les codes sont les suivants: 
#' #' \itemize{
#'   \item 'RFN' Régions forestières nationales
#'   \item 'RFD' Régions forestières départementales
#'   \item 'SER' Sylvoécorégions
#'   \item 'SERAR' Sylvoécorégions d'alluvions récentes
#' }
#' @references 
#' Les données sont explicitées sur le portail de l'IFN: \url{http://inventaire-forestier.ign.fr/carto/carto/afficherCarto/}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   SER_polygon <- IFNtoSHP(code = 'SER')
#' @export
#' 
#' @import tcltk sf utils
#' @importFrom rlang is_empty

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}

IFNtoSHP <- function(code) {
  if(code=="RFN") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/rn250_l93_shp-2.zip"
  }
  if(code=="RFD") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/rf250_l93_shp-2.zip"
  }
  if(code=="SER") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/ser_l93.zip"
  }
  if(code=="SERAR") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/ser_ar_l93.zip"
  }
  if(is_empty(code)) {
    warning("Aucun code entré >> Traitement annulé.")
  } else {
    TF <- tempfile(fileext = ".zip")
    download.file(URL, TF, method="libcurl")
    OUT <- unzip(TF, exdir = tempdir())
    A <- grep(".shp$", OUT)
    SHP <- st_read(OUT[A], quiet=T)
    SHP <- st_transform(SHP, crs = 2154)
    return(SHP)
  }
}

# Not run
# RFN <- IGNtoSHP('RFN')
