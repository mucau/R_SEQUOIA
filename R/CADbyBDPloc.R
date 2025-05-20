#' @title CADbyBDPloc
#' Chargement et export des donnees cadastrales depuis une IGN (c) BD_PARCELLAIRE (r) locale
#' @encoding UTF-8
#' @description 
#' La fonction \code{CADbyBDPloc} charge les données de la IGN© BD_PARCELLAIRE® autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) nécessaires ou utiles à la réalisation d'une cartographie forestière ponctuelle.
#' @usage CADbyBDPloc(rep, bdp)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @param bdp Character. Répertoire de l'IGN© BD_PARCELLAIRE®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @return 
#' \item{ROAD_polygon}{Shapefile Vides cadatrés environnants la propriété: routes+tronçons fluviaux}  
#' \item{PARCELLES_polygon}{Shapefile du parcellaire cadastral global des communes} 
#' \item{BATICA_polygon}{Shapefile des batiments cadastrés environnants la propriété} 
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   CADbyBDPloc(rep=F, bdp=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

CADbyBDPloc <- function(rep=F, bdp=F){
  # Lecture du shapefile
  message('        Lecture des données')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  cat("        Le fichier .shp a été chargé avec succès  \n")
  assign("PARCA", PARCA, envir=globalenv())
  
  # Cahrgement de la bdp
  message("\n        Chargement de l'IGN© BD_PARCELLAIRE®")
  if(isFALSE(bdp)){
    bdp  <- tk_choose.dir(default= getwd(),
                          caption = "Choisir le répertoire de l'IGN© BD_PARCELLAIRE®")
  }
  if (is.na(bdp)){stop("Aucun dossier sélectionnée !")}

  # Fonction de conversion des tempons en enveloppe
  BUF2CONV <- function(x, T) { 
    SHP <- st_sf(st_cast(st_union(st_buffer(x, T, 30)), 'POLYGON'))
    CONVEX_ALL <- st_sf(st_sfc(crs=st_crs(SHP)))
    for (a in 1:nrow(SHP)){
      CONVEX <- st_convex_hull(SHP[a,])
      CONVEX_ALL <- st_make_valid(rbind(CONVEX_ALL, CONVEX))
    }
    return(CONVEX_ALL)
  }
  
  # Creation des tempons
  T = 500
  TEMPON1 <- BUF2CONV(PARCA, T)
  TEMPON2 <- BUF2CONV(PARCA, T-1)
  TEMPON3 <- BUF2CONV(PARCA, T-2)
  TEMPON4 <- BUF2CONV(PARCA, T*2)
  TEMPON5 <- BUF2CONV(PARCA, T*4)

  # write function
  write <- function(nom, rep){
    name <- paste0(NAME, "_", deparse(substitute(nom)), ".shp")
    st_write(nom, rep, name, append=FALSE, delete_layer=TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }
  
  # Creation de BATICA_polygon
  shp_rep  <- list.files(bdp, "BATIMENT.SHP", recursive = T)
  BATICA_polygon <- st_read(paste(bdp, shp_rep, sep="/"), options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T)
  
  if(nrow(BATICA_polygon)>0){
    BATICA_polygon <- st_sf(st_intersection(BATICA_polygon, TEMPON1), agr="constant")
    write(BATICA_polygon, repout2)
  }

# Creation de PARCELLES_polygon
  shp_rep  <- list.files(bdp, "PARCELLE.SHP", recursive = T)
  PARCELLES_polygon <- st_read(paste(bdp, shp_rep, sep="/"), options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T)
  
  if(nrow(PARCELLES_polygon)>0){
    PARCELLES_polygon <- st_sf(st_intersection(st_make_valid(PARCELLES_polygon), TEMPON4), agr="constant")
    write(PARCELLES_polygon, repout2)
  }

# Creation de ROAD_polygon
  ROAD_polygon <- st_intersection(PARCELLES_polygon, TEMPON4)
  ROAD_polygon <- st_intersection(st_difference(TEMPON4, st_union(ROAD_polygon)), TEMPON5)
  if(!is.null(ROAD_polygon)){
    ROAD_polygon <- st_cast(ROAD_polygon, 'POLYGON')
    ROAD_polygon$TYPE <- as.character(NA)
    ROAD_polygon$NAME = as.character(NA)
    ROAD_polygon <- ROAD_polygon[, c("TYPE", "NAME")]
    write(ROAD_polygon, repout2)
  }
}
