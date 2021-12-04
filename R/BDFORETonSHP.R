#' @title BDFORETonSHP
#' Intersection d'un shapefile avec la IGN (C) BD Foret V2 (r)
#' @encoding UTF-8
#' @description 
#' La fonction \code{BDFORETonSHP} intersecte un shapefile avec la  IGN© BD Foret V2®.
#' @usage BDFORETonSHP(shp, repBDFORET)
#' @param shp CHARACTER. Adresse du fichier .shp de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param repBDFORET CHARACTER. Répertoire du dossier de la BD. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @return 
#' \item{BDFORET_polygon}{Shapefile de la IGN© BD Foret V2® sur l'emprise. Généré dans le répertoire du fichier source.}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   BDFORETonSHP(shp = F, BDFORETonSHP = F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}

BDFORETonSHP <- function(shp = F, repBDFORET=F){
  message('- - - BD Foret V2 sur emprise shapefile - - -')

  if(isFALSE(shp)){
    shp_rep  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                       filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if(!length(shp)){stop("        Aucun fichier sélectionné >> Traitement annulé")}

  if(isFALSE(repBDFORET)) {
    repBDFORET <- tk_choose.dir(default= getwd(),
                               caption = "Choisir le répertoire de l'IGN© BD FORET V2®")
  }
  if (!length(repBDFORET)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture des données
  message('        Lecture des données')
  shp <- st_read(shp_rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès  \n \n")

  repBDFORET <- paste(repBDFORET,
                     list.files(path = repBDFORET, pattern = "FORMATION_VEGETALE.shp", recursive = T),
                     sep="/")

  FORET <- st_read(repBDFORET ,options = "ENCODING=windows-1252", quiet=T)
  cat("        La BD FORET V2 a été chargé avec succès  \n \n")

    # Intersection avec l'emprise
  message("        Intersection avec l'emprise")
  foret_shp <- st_intersection(st_transform(st_sf(st_union(shp)), 2154),
                              st_transform(FORET,2154))
  cat("        Intersection effectuée \n")
  foret_shp$SURF_SIG <- st_area(foret_shp)
  cat("        Calcul des SURF_SIG effectué \n \n")

  # Export des données
  message("        Export des données")
  if (Sys.info()["sysname"]=="Windows"){
    NAME <- utils::winDialogString("Entrer le nom du fichier de sortie:", "")
  }else {
    NAME <- readline(prompt="Entrer le nom du fichier de sortie:")
  }
  if(!length(NAME)){NAME <- ""} else {NAME <- paste0(NAME,'_')}

  SEQUOIA:::WRITE(foret_shp, dirname(shp_rep), paste0(NAME,"BDFORET_polygon.shp"))
}
