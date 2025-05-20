#' @title HYDRObyBDTloc
#' Telechargement des donnees IGN(c) BD TOPO(r) HYDROGRAPHIE
#' @encoding UTF-8
#' @description 
#' La fonction \code{HYDRObyBDTloc} charge et exporte les données IGN© BD TOPO® HYDROGRAPHIE autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage HYDRObyBDTloc(rep, bdt)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param bdt Character. Répertoire de la IGN© BD TOPO®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @return
#' \item{HYDRO_polygon}{Objet sf ; SURFO: polygones hydrologique des surfaces hydrographiques permanentes, SURFOi: intermittentes, RESO: réservoirs d'eau}
#' \item{HYDRO_line}{Objet sf ; RU: lignes hydrologiques des tronçons fluviaux permanentes, RUi: intermittents, CANO: canalisations}
#' \item{HYDRO_point}{Objet sf ; PTSO : Points hydrologiques des points d'eau; NOMO: hydronymes}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   HYDRObyBDTloc(rep=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

HYDRObyBDTloc <- function(rep=F, bdt=F){
  options(warn=-1)
  # Lecture du PARCA
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

  # Fonction de chargement de la BD_TOPO
  load <- function(x, bdt){
    rep <- paste(bdt,
                  list.files(path = bdt, pattern = x, recursive = T)[1],
                  sep="/")

    shp <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
    st_crs(shp)<-st_crs(2154)
    return(shp)
  }
  
  # Création de HYDRO_polygon
  HYDRO_polygon <- st_sf(st_sfc(),crs=2154)
  HYDRO_polygon$TYPE <- character(0)
  HYDRO_polygon$NATURE <- character(0)
  
  ## Reservoir
  RSO <- st_intersection(load("RESERVOIR.shp", bdt), TEMPON4)
  if(nrow(RSO)>0){
    RSO$TYPE <- as.character('RSO')
    RSO <- RSO[, c("TYPE", "NATURE")]
    HYDRO_polygon <- rbind(HYDRO_polygon, RSO)
  }
  
  ## Surface 
  SFO <- st_intersection(load("SURFACE_HYDROGRAPHIQUE.shp", bdt), TEMPON4)
  if(nrow(SFO)>0){
    SFO$TYPE <- as.character(NA)
    SFO$TYPE <- ifelse(SFO$PERSISTANC == "Permanent", "SFO", SFO$TYPE)
    SFO$TYPE <- ifelse(SFO$PERSISTANC == "Intermittent", "SFI", SFO$TYPE)
    SFO$TYPE <- ifelse(SFO$PERSISTANC == "Inconnue", "SFI", SFO$TYPE)
    SFO <- SFO[,c("TYPE", "NATURE")]
    HYDRO_polygon <- rbind(HYDRO_polygon, SFO)
  }
  
  ## Export de HYDRO_polygon
  assign("HYDRO_polygon", HYDRO_polygon, envir=globalenv())
  cat("        L'object sf HYDRO_polygon a été ajouté à l'environnement \n")
  
  # Création de HYDRO_line
  HYDRO_line <- st_sf(st_sfc(),crs=2154)
  HYDRO_line$TYPE <- character(0)
  HYDRO_line$NATURE <- character(0)
  HYDRO_line$NOM <- character(0)
  HYDRO_line$DECA <- character(0)
  
  ## Tronçons
  TRONCON_EAU <- st_intersection(load("TRONCON_HYDROGRAPHIQUE.shp", bdt), TEMPON4)
  if(nrow(TRONCON_EAU)>0){
    TRONCON_EAU$TYPE <- as.character(NA)
    TRONCON_EAU$TYPE <- ifelse(TRONCON_EAU$PERSISTANC == "Permanent", "RUI", TRONCON_EAU$TYPE)
    TRONCON_EAU$TYPE <- ifelse(TRONCON_EAU$PERSISTANC == "Intermittent", "RIN", TRONCON_EAU$TYPE)
    TRONCON_EAU$TYPE <- ifelse(TRONCON_EAU$PERSISTANC == "Inconnue", "RIN", TRONCON_EAU$TYPE)
    TRONCON_EAU$NOM <- as.character(TRONCON_EAU$NOM_C_EAU)
    TRONCON_EAU$DECA <- as.character(NA)
    TRONCON_EAU <- TRONCON_EAU[,c("TYPE", "NATURE", "NOM", "DECA")]
    HYDRO_line <- rbind(HYDRO_line, TRONCON_EAU)
  }
  
  ## Export de HYDRO_line
  HYDRO_line <- st_difference(HYDRO_line, st_make_valid(st_combine(HYDRO_polygon)))
  assign("HYDRO_line", HYDRO_line, envir=globalenv())
  cat("        L'object sf HYDRO_line a été ajouté à l'environnement \n")
  
  # Création de HYDRO_point
  HYDRO_point <- st_sf(st_sfc(),crs=2154)
  HYDRO_point$TYPE <- character(0)
  HYDRO_point$NATURE <- character(0)
  HYDRO_point$NOM <- character(0)
  HYDRO_point$ROT <- character(0)
  
  ## Hydronyme
  NOO <- st_intersection(load("TOPONYMIE_HYDROGRAPHIE.shp", bdt), TEMPON4)
  if(nrow(NOO)>0){
    NOO$TYPE <- as.character("NOO")
    NOO$NATURE <- ifelse(NOO$CLASSE == 'Détail hydrographique', NOO$NATURE, NOO$NATURE)
    NOO$NATURE <- ifelse(NOO$CLASSE != 'Détail hydrographique', NOO$CLASSE, NOO$NATURE)
    NOO$NOM <- as.character(NOO$GRAPHIE)
    NOO$ROT <- as.character(NA)
    NOO <- NOO[,c("TYPE", "NATURE", "NOM", "ROT")]
    HYDRO_point <- rbind(HYDRO_point, NOO)
  }
  
  ## Détail hydrographique
  DETAILS_HYDRO <- st_intersection(load("DETAIL_HYDROGRAPHIQUE.shp", bdt), TEMPON4)
  if(nrow(DETAILS_HYDRO)>0){
    DETAILS_HYDRO$TYPE <- as.character("MAR")
    DETAILS_HYDRO$NOM <- as.character(DETAILS_HYDRO$TOPONYME)
    DETAILS_HYDRO$ROT <- as.character(NA)
    DETAILS_HYDRO <- DETAILS_HYDRO[,c("TYPE", "NATURE", "NOM", "ROT")]
    HYDRO_point <- rbind(HYDRO_point, DETAILS_HYDRO)
  }
  
  ## Export de HYDRO_point
  assign("HYDRO_point", HYDRO_point, envir=globalenv())
  cat("        L'object sf HYDRO_point a été ajouté à l'environnement \n")
  options(warn=1)
}

