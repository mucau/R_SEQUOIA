#' @title DEPonSHP
#' Recupération du département
#' @encoding UTF-8
#' @description
#' La fonction \code{DEPonSHP} permet de récupérer la localisation départementale d'un shapefile
#' @usage DEPonSHP(shp=F)
#' @param shp CHARACTER. Chemion du fichier shapefile d'emprise. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier
#' @details 
#' La fonction retourne un sf des département intersectés
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     DEPonSHP(rep = F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(sf)
# library(tcltk)

DEPonSHP <- function(shp=F) {
  options(warn=-1) # Déctivation des warnings
  message('- - - Départements ? - - -')
  
  message("\n        Lecture du shapefile")
  # Sélection du shapefile
  if(isFALSE(shp)){
    shp  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  SHP <- st_read(shp, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès \n")
  
  ADE_3.1_DEPARTEMENT <- st_transform(SEQUOIA::ADE_3.1_DEPARTEMENT, st_crs(SHP))
  
  message("\n        Départements intersectés")
  # Intersection
  
  departement <- st_intersection(ADE_3.1_DEPARTEMENT, st_union(SHP))
  
  return(departement)
}