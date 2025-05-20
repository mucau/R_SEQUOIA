#' @title VEGbyBDTloc
#' Chargement de vegetation depuis une IGN(c) BD TOPO(r) locale
#' @encoding UTF-8
#' @description 
#' La fonction \code{VEGbyBDTloc} charge les données de végétations depuis IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage VEGbyBDTloc(rep, bdt)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile
#' @param bdt Character. Répertoire de la IGN© BD TOPO®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier
#' @return
#' \item{VEG_polygon}{Objet sf ; Polygones des surfaces forestières ; TYPE='VEG'}
#' \item{VEG_line}{Objet sf ;Lignes contours surfaces forestières ; TYPE='VEG'}
#' \item{VEG_point}{Objet sf ;Points natures de culture ; TYPE='VEG'}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'     VEGbyBDTloc(rep=F, bdt=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

VEGbyBDTloc <- function(rep=F, bdt=F){
  options(warn=-1)
  
  # Lecture du shapefile
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n")
  assign("PARCA", PARCA, envir=globalenv())

  # Sélection de la bdt
  if(isFALSE(bdt)) {
    bdt <- tk_choose.dir(default= getwd(),
                         caption = "Choisir le répertoire de l'IGN (c) BD TOPO (r)")
  }
  if (is.na(bdt)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture des données
  bdt <- paste(bdt,
                     list.files(path = bdt, pattern = "ZONE_DE_VEGETATION.shp", recursive = T),
                     sep="/")

  SHP <- st_read(bdt ,options = "ENCODING=UTF-8", quiet=T)
  cat("        La BD TOPO a été chargé avec succès  \n")

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

  # Intersection
  VEG <- st_intersection(SHP, TEMPON4)
  VEG <- st_make_valid(st_transform(VEG, 2154))

  # Difference
  VEG <- st_difference(VEG, st_make_valid(st_union(PARCA)))
  if(exists("INFRA_polygon")){
    VEG <- st_difference(VEG, st_make_valid(st_buffer(st_combine(INFRA_polygon),5)))
  }
  VEG <- st_make_valid(st_cast(VEG, 'MULTIPOLYGON'))

  # Création de VEG_polygon
  VEG$TYPE = as.character('VEG')
  VEG$surf_sig = as.numeric(st_area(VEG))
  VEG_polygon <- VEG

  # Création de VEG_line
  VEG_line <- st_cast(st_union(VEG_polygon), 'MULTILINESTRING')
  VEG_line <- st_difference(VEG_line, st_buffer(st_union(PARCA), 2))
  VEG_line <- st_sf(st_intersection(VEG_line, st_buffer(TEMPON4, -2)))
  VEG_line <- st_cast(VEG_line, 'LINESTRING') 
  VEG_line$TYPE = as.character('VEG')
  VEG_line$NATURE = as.character(NA)
  VEG_line$NOM = as.character(NA)
  VEG_line$DECA = as.character(NA)
  st_geometry(VEG_line) <- "geometry"
  VEG_line <- VEG_line[, c("TYPE", "NATURE", "NOM", "DECA")]
  
  assign("VEG_line", VEG_line, envir=globalenv())
  cat("        L'object sf VEG_line a été ajouté à l'environnement \n")

  # Création de VEG_point
  VEG_point <- st_sf(st_sample(VEG_polygon, as.vector(sum(round(VEG_polygon$surf_sig/50000))), type = "hexagonal"))
  VEG_point <- st_join(VEG_point, VEG_polygon, join = st_intersects)
  VEG_point <- subset(VEG_point, !(VEG_point$NATURE %in% c("Verger","Lande ligneuse")))
  VEG_point$NOM <- as.character(NA)
  VEG_point$ROT <- as.integer(NA)
  st_geometry(VEG_point) <- "geometry"
  VEG_point <- VEG_point[, c("TYPE", "NATURE", "NOM", "ROT")]
  
  VEG_point$TYPE <- ifelse(VEG_point$NATURE == "Forêt fermée de conifères", "REV", VEG_point$TYPE)
  VEG_point$TYPE <- ifelse(VEG_point$NATURE == "Forêt fermée de feuillus", "FEV", VEG_point$TYPE)
  VEG_point$TYPE <- ifelse(VEG_point$NATURE == "Forêt fermée mixte", "FRV", VEG_point$TYPE)
  VEG_point$TYPE <- ifelse(VEG_point$NATURE == "Forêt ouverte", "FEV", VEG_point$TYPE)
  VEG_point$TYPE <- ifelse(VEG_point$NATURE == "Bois", "FEV", VEG_point$TYPE)
  VEG_point$TYPE <- ifelse(VEG_point$NATURE == "Haie", "FEV", VEG_point$TYPE)
  
  assign("VEG_point", VEG_point, envir=globalenv())
  cat("        L'object sf VEG_point a été ajouté à l'environnement \n")
  
  # Export de VEG_polygon
  VEG_polygon <- VEG[, c("TYPE", "NATURE")]
  
  assign("VEG_polygon", VEG_polygon, envir=globalenv())
  cat("        L'object sf VEG_polygon a été ajouté à l'environnement \n")
  
  options(warn=1)
}
