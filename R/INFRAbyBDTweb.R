#' @title INFRAbyBDTweb
#' Telechargement de donnees depuis IGN(c) BD TOPO(r))
#' @encoding UTF-8
#' @description 
#'La fonction \code{INFRAbyBDTweb} télécharge des données issues d'une IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage INFRAbyBDTloc(rep, bdt)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
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
#'   INFRAbyBDTweb(rep=F))
#' @export
#' 
#' @import tcltk sf happign

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)

INFRAbyBDTweb <- function(rep=F){ # function
  options(warn=-1)
  # Lecture du PARCA
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n")
  assign("PARCA", PARCA, envir=globalenv())
  
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
  
  # Création du .shp "COMS_polygon"
  invisible(capture.output(suppressMessages(COMS <- get_wfs(TEMPON5,
                                                            "BDTOPO_V3:commune",
                                                            NULL,
                                                            "intersects"))))
  if(!is.null(COMS)){
    COMS_polygon <- st_transform(COMS,2154)
    COMS_polygon$NOM <- as.character(COMS_polygon$nom_officiel)
    COMS_polygon <- COMS_polygon[,"NOM"]
  }
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
  INFRA_polygon <- st_sf(st_sfc(),crs=4326)
  INFRA_polygon$TYPE <- character(0)
  INFRA_polygon$NATURE <- character(0)
  
  ## Lecture fonction
  load <- function(layer_name, type_name, tempon=TEMPON4){
    invisible(capture.output(suppressMessages(sf <- get_wfs(tempon,
                                                            layer_name,
                                                            NULL,
                                                            "intersects"))))
    if(!is.null(sf)){
      sf$TYPE <- as.character(type_name)
      sf$NATURE <- as.character(sf$nature)
      sf <- sf[,c("TYPE", "NATURE")]
    }
    return(sf)
  }
  
  ## Batiment
  BAT <- load("BDTOPO_V3:batiment", "BAT")
  if(!is.null(BAT)){INFRA_polygon <- rbind(INFRA_polygon, BAT)}
  
  ## Cimetierre
  CIM <- load("BDTOPO_V3:cimetiere", "CIM") 
  if(!is.null(CIM)){INFRA_polygon <- rbind(INFRA_polygon, CIM)}
  
  ## Terrain de sport
  SPO <- load("BDTOPO_V3:terrain_de_sport", "SPO") 
  if(!is.null(SPO)){INFRA_polygon <- rbind(INFRA_polygon, SPO)}
  
  ## Terrain de sport
  CST <- load("BDTOPO_V3:construction_surfacique", "CST") 
  if(!is.null(CST)){INFRA_polygon <- rbind(INFRA_polygon, CST)}
  
  ## Export de INFRA_polygon
  if(nrow(INFRA_polygon)>0){
    assign("INFRA_polygon", st_zm(st_transform(INFRA_polygon, 2154)), envir=globalenv())
    cat("        L'object sf INFRA_polygon a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_polygon", st_transform(INFRA_polygon, 2154), envir=globalenv())
    message("        BDTOPO : Pas de polygones détectés sur l'emprise \n")
  }
  
  
  # Création de INFRA_line
  INFRA_line <- st_sf(st_sfc(),crs=4326)
  INFRA_line$TYPE <- character(0)
  INFRA_line$NATURE <- character(0)
  INFRA_line$NOM <- character(0)
  INFRA_line$DECA <- character(0)
  
  ## Lecture fonction
  load <- function(layer_name, type_name,  tempon=TEMPON4){
    invisible(capture.output(suppressMessages(sf <- get_wfs(tempon,
                                                            layer_name,
                                                            NULL,
                                                            "intersects"))))
    if(!is.null(sf)){
      sf$TYPE <- as.character(type_name)
      sf$NATURE <- as.character(NA)
      sf$NOM <- as.character(NA)
      sf$DECA <- as.character(NA)
    }
    return(sf)
  }
  
  ## Construction linéraire
  CST <- load("BDTOPO_V3:construction_lineaire", "CST")
  if(!is.null(CST)){
    CST$NATURE <- as.character(CST$nature)
    CST <- CST[,c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, CST)
  }
  
  ## Orographie
  ORO <- load("BDTOPO_V3:ligne_orographique", "ORO")
  if(!is.null(ORO)){
    ORO$NATURE <- as.character(ORO$nature)
    ORO <- ORO[,c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, ORO)
  }
  
  ## Ligne électrique
  LEL <- load("BDTOPO_V3:ligne_electrique", "LEL")
  if(!is.null(LEL)){
    LEL$NATURE <- as.character(LEL$voltage)
    LEL <- LEL[,c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, LEL)
  }
 
  ## Voie ferrée
  VFE <- load("BDTOPO_V3:troncon_de_voie_ferree", "VFE")
  if(!is.null(VFE)){
    VFE$NATURE <- as.character(VFE$nature)
    VFE$NOM <- as.character(VFE$cpx_toponyme)
    VFE <- VFE[,c("TYPE", "NATURE", "NOM", "DECA")]
    INFRA_line <- rbind(INFRA_line, VFE)
  }
  
  ## Export de INFRA_line
  if(nrow(INFRA_line)>0){
    assign("INFRA_line", st_zm(st_transform(INFRA_line,2154)), envir=globalenv())
    cat("        L'object sf INFRA_line a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_line", INFRA_line, envir=globalenv())
    message("        BDTOPO : Pas de lignes détectés sur l'emprise \n")
  }
  
  # Création de INFRA_point
  INFRA_point <- st_sf(st_sfc(),crs=4326)
  INFRA_point$TYPE <- character(0)
  INFRA_point$NATURE <- character(0)
  INFRA_point$NOM <- character(0)
  INFRA_point$ROT <- character(0)
  
  ## Lecture fonction
  load <- function(layer_name, type_name,  tempon=TEMPON4){
    invisible(capture.output(suppressMessages(sf <- get_wfs(tempon,
                                                            layer_name,
                                                            NULL,
                                                            "intersects"))))
    if(!is.null(sf)){
      sf$TYPE <- as.character(type_name)
      sf$NATURE <- as.character(NA)
      sf$NOM <- as.character(NA)
      sf$ROT <- as.character(NA)
      return(st_zm(sf))
    } else {
      return(sf)
    }
  }
  
  ## Construction ponctuelle
  CST <- load("BDTOPO_V3:construction_ponctuelle", "CST")
  if(!is.null(CST)){
    CST$NATURE <- as.character(CST$nature)
    CST$TYPE <- ifelse(CST$NATURE == "Antenne", "PYL", CST$TYPE)
    CST$TYPE <- ifelse(CST$NATURE == "Clocher", "CLO", CST$TYPE)
    CST$TYPE <- ifelse(CST$NATURE == "Croix", "CRX", CST$TYPE)
    CST <- CST[,c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, CST)
  }
  
  ## Pylone electrique
  PEL <- load("BDTOPO_V3:pylone", "PEL")
  if(!is.null(PEL)){
    PEL$NATURE <- as.character(PEL$etat_de_l_objet)
    PEL <- PEL[,c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, PEL)
  }
  
  ## Toponyme bati
  TOPOBT <- load("BDTOPO_V3:toponymie_bati", "NOM")
  if(!is.null(TOPOBT)){
    TOPOBT$NATURE <- as.character(TOPOBT$nature_de_l_objet)
    TOPOBT$NOM <- as.character(TOPOBT$graphie_du_toponyme)
    TOPOBT <- TOPOBT[,c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, TOPOBT)
  }
  
  ## Toponyme lieux
  TOPOLD <- load("BDTOPO_V3:toponymie_lieux_nommes", "NOM")
  if(!is.null(TOPOLD)){
    TOPOLD$NATURE <- as.character(TOPOLD$nature_de_l_objet)
    TOPOLD$NOM <- as.character(TOPOLD$graphie_du_toponyme)
    TOPOLD <- TOPOLD[,c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, TOPOLD)
  }
  
  ## Toponyme transport
  TOPOTR <- load("BDTOPO_V3:toponymie_transport", "NOM")
  if(!is.null(TOPOTR)){
    TOPOTR$NATURE <- as.character(TOPOTR$nature_de_l_objet)
    TOPOTR$NOM <- as.character(TOPOTR$graphie_du_toponyme)
    TOPOTR <- TOPOTR[,c("TYPE", "NATURE", "NOM", "ROT")]
    INFRA_point <- rbind(INFRA_point, TOPOTR)
  }
  
  if(nrow(INFRA_point)>0){
    assign("INFRA_point", st_zm(st_transform(unique(INFRA_point), 2154)), envir=globalenv())
    cat("        L'object sf INFRA_point a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_point", st_transform(INFRA_point, 2154), envir=globalenv())
    message("        BDTOPO : Pas de points détectés sur l'emprise \n")
  }
  
  # Création de ROAD_line
  invisible(capture.output(suppressMessages(ROAD <- get_wfs(TEMPON4,
                                     "BDTOPO_V3:troncon_de_route",
                                     NULL,
                                     "intersects"))))
  
  if(!is.null(ROAD)){
    ROAD$NATURE <- as.character(ROAD$nature)
    ROAD$TYPE <- as.character(NA)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Type autoroutier", "RN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Bretelle", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Chemin", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Escalier", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Piste cyclable", "PN", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Rond-point", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Route à 1 chaussée", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Route à 2 chaussées", "RC", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Route empierrée", "RF", ROAD$TYPE)
    ROAD$TYPE <- ifelse(ROAD$NATURE == "Sentier", "PN", ROAD$TYPE)
    ROAD$NOM <- as.character(ROAD$cpx_numero)
    ROAD$TYPE <- ifelse(!is.na(ROAD$NOM),ifelse(substr(ROAD$NOM,0,1)=="D","RD", ROAD$TYPE),ROAD$TYPE)
    ROAD$DECA <- as.character(NA)
    ROAD <- ROAD[, c("TYPE", "NATURE", "NOM", "DECA")]
    ROAD <- subset(ROAD, !(ROAD$NATURE %in% 'Piste cyclable'))
    ROAD_line <- st_zm(st_transform(ROAD,2154))
    write(ROAD_line, repout2)
  }
  
  options(warn=1)
} # end function
