#' @title LINEfromPOLYGON
#' Linearisation d'un polygon
#' @encoding UTF-8
#' @description
#' La fonction \code{LINEfromPOLYGON} convertit un polygon en ligne sans superpositions.
#' @usage LINEfromPOLYGON(polygon)
#' @param polygon Objet polygon de type POLYGON
#' @details La fonction retourne un fichier sf de type LINESTRING
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     LINEfromPOLYGON(polygon)
#' @export
#' 
#' @import sf lwgeom

# Lancement des library
# library(sf)
# library(lwgeom)

LINEfromPOLYGON <- function(polygon){
  options(warn=-1) # Déctivation des warnings
  if(!class(polygon)[1]=="sf"){stop("        Pas d'objet sf détecté")}
  
  # Récupère les noeuds de polygones
  point <- st_sf(st_cast(st_combine(st_cast(polygon,'MULTIPOINT')), 'POINT'))
  point <- unique(point)
  
  # Récupère les contours de polygones
  line <- st_cast(polygon, "MULTILINESTRING")
  
  # Segmentation : Decoupe les contours de polygones par les noeuds
  line <- st_split(line, point)
  
  # Sélectionne les objets de classe LINESTRING
  line <- st_collection_extract(line,"LINESTRING") 
  
  # Ajouter un champ longueur
  line$LENGTH <- st_length(line)
  
  # Supprime les doublons sur le champs longueur
  line <- line[!duplicated(line$LENGTH),]             
  st_crs(line) <-2154
  line <- line[0]
  return(line)
  
  options(warn=1) # Activation des warnings
}
