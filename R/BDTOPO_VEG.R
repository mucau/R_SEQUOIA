#' @title BDTOPO_VEG
#' Telechargement de vegetation depuis IGN(c) BD TOPO(r))
#' @encoding UTF-8
#' @description 
#' La fonction \code{BDTOPO_VEG} charge les données de végétations depuis IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage BDTOPO_VEG(PARCA, repBDTOPO)
#' @param PARCA sf du parcellaire cadastral. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile
#' @param repBDTOPO Répertoire de la IGN© BD TOPO®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier
#' @return
#' \item{VEG_polygon}{Objet sf ; Polygones des surfaces forestières ; TYPE='VEG'}
#' \item{VEG_line}{Objet sf ;Lignes contours surfaces forestières ; TYPE='VEG'}
#' \item{VEG_point}{Objet sf ;Points natures de culture ; TYPE='VEG'}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'     BDTOPO_VEG(PARCA=F, repBDTOPO=F)
#' @export
#' 
#' @import tcltk sf dplyr stringr lwgeom

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("lwgeom")) {install.packages("lwgeom")}

BDTOPO_VEG <- function(PARCA=F, repBDTOPO=F){
  options(warn=-1) # Désactivation des warnings

  # Lecture des données
  if(isFALSE(PARCA)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
    if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
    assign("PARCA", PARCA, envir=globalenv())
  }
  if (!length(PARCA)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  if(isFALSE(repBDTOPO)) {
    repBDTOPO <- tk_choose.dir(default= getwd(),
                               caption = "Choisir le répertoire de l'IGN© BD TOPO®")
    if (!length(repBDTOPO)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  }

  # Lecture des données
  repBDTOPO <- paste(repBDTOPO,
                     list.files(path = repBDTOPO, pattern = "ZONE_DE_VEGETATION.shp", recursive = T),
                     sep="/")

  SHP <- st_read(repBDTOPO ,options = "ENCODING=UTF-8", quiet=T)
  cat("        La BD TOPO a été chargé avec succès  \n")

  # Préparation des tempoms
  BUF2CONV <- function(x, T) { #Fonction de conversion des tempons en enveloppe
    SHP <- st_sf(st_cast(st_union(st_buffer(x, T, 30)), 'POLYGON'))
    CONVEX_ALL <- st_sf(st_sfc(crs=st_crs(SHP)))
    for (a in 1:nrow(SHP)){
      CONVEX <- st_convex_hull(SHP[a,])
      CONVEX_ALL <- st_make_valid(rbind(CONVEX_ALL, CONVEX))
    }
    return(CONVEX_ALL)
  }

  T = 500
  TEMPON1 <- BUF2CONV(PARCA, T)
  TEMPON2 <- BUF2CONV(PARCA, T-1)
  TEMPON3 <- BUF2CONV(PARCA, T-2)
  TEMPON4 <- BUF2CONV(PARCA, T*2)
  TEMPON5 <- BUF2CONV(PARCA, T*4)
  TEMPON6 <- BUF2CONV(PARCA, 0.5)

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
  VEG_polygon <- VEG %>%
    mutate(TYPE = 'VEG',
           surf_sig = as.numeric(st_area(.))) %>%
    filter(surf_sig > 1000) %>%
    select(TYPE, NATURE, surf_sig)

  assign("VEG_polygon", VEG_polygon %>%select(TYPE, NATURE), envir=globalenv())
  cat("        L'object sf VEG_polygon a été ajouté à l'environnement \n")

  # Création de VEG_line
  VEG_line <- st_cast(st_union(VEG_polygon), 'MULTILINESTRING')
  VEG_line <- st_difference(VEG_line, st_buffer(st_union(PARCA), 2))
  VEG_line <- st_sf(st_intersection(VEG_line, st_buffer(TEMPON4, -2)))
  VEG_line <- st_cast(VEG_line, 'LINESTRING') %>%
    mutate(TYPE = 'VEG',
           NATURE = as.character(NA),
           NAME = as.character(NA),
           DECA = as.character(NA),
           geometry=st_geometry(.))
  st_geometry(VEG_line) <- "geometry"
  VEG_line <- VEG_line  %>%
    select(TYPE, NATURE, NAME, DECA)

  assign("VEG_line", VEG_line, envir=globalenv())
  cat("        L'object sf VEG_line a été ajouté à l'environnement \n")

  # Création de VEG_point
  VEG_point <- st_sf(st_sample(VEG_polygon, as.vector(sum(round(VEG_polygon$surf_sig/50000))), type = "hexagonal"))
  VEG_point <- st_join(VEG_point, VEG_polygon, join = st_intersects)
  VEG_point <- VEG_point %>%
    mutate(NOM  = as.character(NA),
           ROT  = as.integer(NA)) %>%
    filter(!(NATURE %in% c("Verger","Lande ligneuse"))) %>%
    select(TYPE, NATURE, NOM, ROT)

  assign("VEG_point", VEG_point, envir=globalenv())
  cat("        L'object sf VEG_point a été ajouté à l'environnement \n")

  options(warn=1) # Activation des warnings
}
