#' @title INFRAbyBDTloc
#' Telechargement de donnees depuis IGN(c) BD TOPO(r))
#' @encoding UTF-8
#' @description 
#'La fonction \code{INFRAbyBDTloc} charge des données issues d'une IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage INFRAbyBDTloc(rep, bdt)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @param bdt Character. Répertoire de la IGN© BD TOPO®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier
#' @return
#' \item{COMS_polygon}{Fichier shapefile ; Communes environnantes ; Tempon de 2000m autour du PARCA_polygon}
#' \item{COMS_line}{Fichier shapefile ; Contours des communes environnantes ; Tempon de 2000m autour du PARCA_polygon}
#' \item{COM_line}{Fichier shapefile ; Contours des communes environnantes ; Tempon de 500m autour du PARCA_polygon}
#' \item{COMS_point}{Fichier shapefile ; Centroid des communes environnantes ; Tempon de 2000m autour du PARCA_polygon}
#' \item{COM_point}{Fichier shapefile ; Centroid des communes environnantes ; Tempon de 500m autour du PARCA_polygon}
#' \item{INFRA_polygon}{Objet sf ; Infrastructures environnantes ; Y sont intégrés TYPE='BT': les batis, TYPE='CIM' : les cimetières, TYPE='SP' : les terrains de sports, TYPE='CSTsurf' : les constructions surfaciques}
#' \item{INFRA_line}{Objet sf ; sf Infrastructures environnantes ; Y sont intégrés TYPE='CSTline' : les constructions linéaires, TYPE='ORO': les lignes orographiques, TYPE='LE' : les lignes électriques, TYPE='VF' : les voies ferrées}
#' \item{ROAD_line}{Fichier shapefile ; Routes environnantes. Y sont intégrés TYPE='RN' : routes nationnales & autoroutes, TYPE='RD' : routes départementales, TYPE='RC' : routes communales, TYPE='RF' : routes empierrées/forestières, TYPE='PN' : les pistes en terrain naturel}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INFRAbyBDTloc(rep=F, bdt=F))
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

INFRAbyBDTloc <- function(rep=F, bdt=F){
  options(warn=-1) # Désactivation des warnings
  
  # Lecture du shapefile
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
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
  load <- function(x, repertoire){
    rep <- paste(bdt,
                 list.files(path = repertoire, pattern = x, recursive = T)[1],
                 sep="/")

    sf <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
    st_crs(sf)<-st_crs(2154)
    return(sf)
  }

  # Création du .shp "COMS_polygon"
  COMS <-load("COMMUNE.shp", bdt)
  intersection <- st_intersects(COMS, st_union(TEMPON5), sparse = FALSE)
  chevauchements <- apply(intersection, 1, any)
  COMS_polygon <- COMS[chevauchements, c("NOM")]
  write(COMS_polygon, repout2)

  # Création du .shp "COMS_line"
  linearize <- function(x){
    line <- st_boundary(x)[0]
    line <- st_cast(line, "LINESTRING")
    line <- st_union(line)
    line <- st_cast(line, "LINESTRING")
    line <- st_sf(geometry = line)
    st_crs(line)<-2154
    return(line)
  }
  COMS_line <- linearize(COMS_polygon)
  write(COMS_line, repout2)

  COM_line <- st_intersection(COMS_line, TEMPON1)
  write(COM_line, repout2)
  
  # Création du .shp "COMS_point"
  COMS_point <- st_centroid(COMS_polygon)
  write(COMS_point, repout2)
  
  COM_point <- st_centroid(st_intersection(COMS_polygon, TEMPON1))
  write(COM_point, repout2)


  # Création de INFRA_polygon
  INFRA_polygon <- st_sf(st_sfc(),crs=2154)
  INFRA_polygon$TYPE <- character(0)
  INFRA_polygon$NATURE <- character(0)

  ## Batiments
  BAT <-load("BATIMENT.shp", bdt)
  BAT <- st_intersection(BAT, TEMPON4)
  if(nrow(BAT)>0){
    BAT$TYPE <- as.character('BAT')
    BAT <- BAT[, c("TYPE", "NATURE")]
    INFRA_polygon <- rbind(INFRA_polygon, BAT)
  }
  
  ## Cimetierre
  CIM <-load("CIMETIERE.shp", bdt)
  CIM <- st_intersection(CIM, TEMPON4)
  if(nrow(CIM)>0){
    CIM$TYPE <- as.character('CIM')
    CIM <- CIM[, c("TYPE", "NATURE")]
    INFRA_polygon <- rbind(INFRA_polygon, CIM)
  }
  
  ## Terrain de sport
  SPO <-load("TERRAIN_DE_SPORT.shp", bdt)
  SPO <- st_intersection(SPO, TEMPON4)
  if(nrow(SPO)>0){
    SPO$TYPE <- as.character('SPO')
    SPO <- SPO[, c("TYPE", "NATURE")]
    INFRA_polygon <- rbind(INFRA_polygon, SPO)
  }
  
  ## Terrain de sport
  CSTsurf <-load("CONSTRUCTION_SURFACIQUE.shp", bdt)
  CSTsurf <- st_zm(st_intersection(CSTsurf, TEMPON4))
  if(nrow(CSTsurf)>0){
    CSTsurf$TYPE <- as.character('CST')
    CSTsurf <- CSTsurf[, c("TYPE", "NATURE")]
    INFRA_polygon <- rbind(INFRA_polygon, CSTsurf)
  }

  if(nrow(INFRA_polygon)>0){
    assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
    cat("        L'object sf INFRA_polygon a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
    message("        BDTOPO : Pas de polygones détectés sur l'emprise \n")
  }


  # Création de INFRA_line
  INFRA_line <- st_sf(st_sfc(),crs=2154)
  INFRA_line$TYPE <- character(0)
  INFRA_line$NATURE <- character(0)
  INFRA_line$NOM <- character(0)
  INFRA_line$DECA <- character(0)
  
  ## Construction linérairE
  CSTline <-load("CONSTRUCTION_LINEAIRE.shp", bdt)
  CSTline <- st_intersection(CSTline, TEMPON4)
  if(nrow(CSTline)>0){
    CSTline$TYPE <- as.character('CST')
    CSTline$NOM <- as.character(NA)
    CSTline$DECA <- as.character(NA)
    CSTline <- CSTline[, c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, CSTline)
  }
  
  ## Orographie
  ORO <-load("LIGNE_OROGRAPHIQUE.shp", bdt)
  ORO <- st_intersection(ORO, TEMPON4)
  if(nrow(ORO)>0){
    ORO$TYPE <- as.character('ORO')
    ORO$NOM <- as.character(NA)
    ORO$DECA <- as.character(NA)
    ORO <- ORO[, c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, ORO)
  }
  
  ## Ligne électrique
  LEL <-load("LIGNE_ELECTRIQUE.shp", bdt)
  LEL <- st_intersection(LEL, TEMPON4)
  if(nrow(ORO)>0){
    LEL$TYPE <- as.character('LEL')
    LEL$NATURE <- as.character(LEL$VOLTAGE)
    LEL$NOM <- as.character(NA)
    LEL$DECA <- as.character(NA)
    LEL <- LEL[, c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, LEL)
  }
  
  ## Voie ferrée
  VFE <-load("TRONCON_DE_VOIE_FERREE.shp", bdt)
  VFE <- st_intersection(VFE, TEMPON4)
  if(nrow(VFE)>0){
    VFE$TYPE <- as.character('VFE')
    VFE$NOM <- as.character(VFE$TOPONYME)
    VFE$DECA <- as.character(NA)
    VFE <- VFE[, c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, VFE)
  }
  
  if(nrow(INFRA_line)>0){
    assign("INFRA_line", INFRA_line, envir=globalenv())
    cat("        L'object sf INFRA_line a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_line", INFRA_line, envir=globalenv())
    message("        BDTOPO : Pas de lignes détectés sur l'emprise \n")
  }

  # Création de INFRA_point
  INFRA_point <- st_sf(st_sfc(),crs=2154)
  INFRA_point$TYPE <- character(0)
  INFRA_point$NATURE <- character(0)
  INFRA_point$NOM <- character(0)
  INFRA_point$ROT <- character(0)
  
  ## Construction ponctuelle
  CSTpts <-load("CONSTRUCTION_PONCTUELLE.shp", bdt)
  CSTpts <- st_intersection(CSTpts, TEMPON4)
  if(nrow(CSTpts)>0){
    CSTpts$TYPE <- as.character('CST')
    CSTpts$TYPE <- ifelse(CSTpts$NATURE == "Antenne", "PYL", CSTpts$TYPE)
    CSTpts$TYPE <- ifelse(CSTpts$NATURE == "Clocher", "CLO", CSTpts$TYPE)
    CSTpts$TYPE <- ifelse(CSTpts$NATURE == "Croix", "CRX", CSTpts$TYPE)
    CSTpts$NOM <- as.character(NA)
    CSTpts$ROT <- as.character(NA)
    CSTpts <- CSTpts[, c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, CSTpts)
  }
  
  ## Construction ponctuelle
  PEL <-load("PYLONE.shp", bdt)
  PEL <- st_intersection(PEL, TEMPON4)
  if(nrow(PEL)>0){
    PEL$TYPE <- as.character('PEL')
    PEL$NATURE <- as.character(PEL$ETAT)
    PEL$NOM <- as.character(NA)
    PEL$ROT <- as.character(NA)
    PEL <- PEL[, c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, PEL)
  }

  ## Toponyme bati
  TOPOBT <-load("TOPONYMIE_BATI.shp", bdt)
  TOPOBT <- st_intersection(TOPOBT, TEMPON4)
  if(nrow(TOPOBT)>0){
    TOPOBT$TYPE <- as.character('NOM')
    TOPOBT$NOM <- as.character(TOPOBT$GRAPHIE)
    TOPOBT$ROT <- as.character(NA)
    TOPOBT <- TOPOBT[, c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, TOPOBT)
  }
  
  ## Toponyme lieux
  TOPOLD <-load("TOPONYMIE_LIEUX_NOMMES.shp", bdt)
  TOPOLD <- st_intersection(TOPOLD, TEMPON4)
  if(nrow(TOPOLD)>0){
    TOPOLD$TYPE <- as.character('NOM')
    TOPOLD$NOM <- as.character(TOPOLD$GRAPHIE)
    TOPOLD$ROT <- as.character(NA)
    TOPOLD <- TOPOLD[, c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, TOPOLD)
  }
  
  ## Toponyme transport
  TOPOTR <-load("TOPONYMIE_TRANSPORT.shp", bdt)
  TOPOTR <- st_intersection(TOPOTR, TEMPON4)
  if(nrow(TOPOTR)>0){
    TOPOTR$TYPE <- as.character('NOM')
    TOPOTR$NOM <- as.character(TOPOTR$GRAPHIE)
    TOPOTR$ROT <- as.character(NA)
    TOPOTR <- TOPOTR[, c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, TOPOTR)
  }

  if(nrow(INFRA_point)>0){
    assign("INFRA_point", unique(INFRA_point), envir=globalenv())
    cat("        L'object sf INFRA_point a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_point", INFRA_point, envir=globalenv())
    message("        BDTOPO : Pas de points détectés sur l'emprise \n")
  }

  # Création de ROAD_line
  ROAD <-load("TRONCON_DE_ROUTE.shp", bdt)
  ROAD <- st_intersection(ROAD, TEMPON4)
  if(!is.null(ROAD)){
    ROAD$TYPE <- as.character(NA)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Bretelle", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Chemin", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Escalier", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Piste cyclable", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Rond-point", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Route à 1 chaussée", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Route à 2 chaussées", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Route empierrée", "RF", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Sentier", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$CL_ADMIN == "Départementale", "RD", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$CL_ADMIN == "Autoroute", "RN", ROAD$TYPE)
    ROAD$NOM <- as.character(ROAD$NOM_COLL_G)
    ROAD$DECA <- as.character(NA)
    ROAD <- ROAD[, c("TYPE", "NATURE", "NOM", "DECA")]
    ROAD_line <- subset(ROAD, !(ROAD$NATURE %in% 'Piste cyclable'))
    write(ROAD_line, repout2)
  }
  options(warn=1) # Activation des warnings
}
