#' @title HYDRObyBDTweb
#' Telechargement des donnees IGN(c) BD TOPO(r) HYDROGRAPHIE
#' @encoding UTF-8
#' @description 
#' La fonction \code{HYDRObyBDTweb} télécharge et exporte les données IGN© BD TOPO® HYDROGRAPHIE autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage HYDRObyBDTweb(rep)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{HYDRO_polygon}{Objet sf ; SURFO: polygones hydrologique des surfaces hydrographiques permanentes, SURFOi: intermittentes, RESO: réservoirs d'eau}
#' \item{HYDRO_line}{Objet sf ; RU: lignes hydrologiques des tronçons fluviaux permanentes, RUi: intermittents, CANO: canalisations}
#' \item{HYDRO_point}{Objet sf ; PTSO : Points hydrologiques des points d'eau; NOMO: hydronymes}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   HYDRObyBDTweb(rep=F)
#' @export
#' 
#' @import tcltk sf happign

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)

HYDRObyBDTweb<- function(rep=F){ # function
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
  
  # Récupération des ardresses web
  layers_data <- get_layers_metadata(apikey = "topographie", data_type = "wfs")
  
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
  
  # Création de HYDRO_polygon
  HYDRO_polygon <- st_sf(st_sfc(),crs=4326)
  HYDRO_polygon$TYPE <- character(0)
  HYDRO_polygon$NATURE <- character(0)
  
  ## Lecture fonction
  load <- function(layer_name, type_name, tempon=TEMPON4){
    invisible(capture.output(suppressMessages(sf <- get_wfs(tempon,
                                                            layer_name,
                                                            NULL,
                                                            "intersects"))))
    if(!is.null(sf)){
      sf$TYPE <- as.character(type_name)
      sf$NATURE <- as.character(sf$nature)
    }
    return(sf)
  }
  
  ## Reservoir
  RSO <- load("BDTOPO_V3:reservoir", "RSO")
  if(!is.null(RSO)){
    RSO <- RSO[,c("TYPE", "NATURE")]
    HYDRO_polygon <- rbind(HYDRO_polygon, RSO)
  }
  
  ## Surface 
  SFO <- load("BDTOPO_V3:surface_hydrographique", "SFO")
  if(!is.null(SFO)){
    SFO$TYPE <- ifelse(SFO$persistance == "Permanent", "SFO", SFO$TYPE)
    SFO$TYPE <- ifelse(SFO$persistance == "Intermittent", "SFI", SFO$TYPE)
    SFO$TYPE <- ifelse(SFO$persistance == "Inconnue", "SFI", SFO$TYPE)
    SFO <- SFO[,c("TYPE", "NATURE")]
    HYDRO_polygon <- rbind(HYDRO_polygon, SFO)
  }
  
  ## Export de HYDRO_polygon
  assign("HYDRO_polygon", st_zm(st_transform(HYDRO_polygon,2154)), envir=globalenv())
  cat("        L'object sf HYDRO_polygon a été ajouté à l'environnement \n")
  
  
  # Création de l'objet sf HYDRO_line
  HYDRO_line <- st_sf(st_sfc(),crs=4326)
  HYDRO_line$TYPE <- character(0)
  HYDRO_line$NATURE <- character(0)
  HYDRO_line$NOM <- character(0)
  HYDRO_line$DECA <- character(0)
  
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
  
  ## Tronçons
  RUI <- load("BDTOPO_V3:troncon_hydrographique", "RUI")
  if(!is.null(RUI)){
    RUI$NATURE <- as.character(RUI$nature)
    RUI$TYPE <- ifelse(RUI$persistance == "Permanent", "RUI", RUI$TYPE)
    RUI$TYPE <- ifelse(RUI$persistance == "Intermittent", "RIN", RUI$TYPE)
    RUI$TYPE <- ifelse(RUI$persistance == "Inconnue", "RIN", RUI$TYPE)
    RUI <- RUI[,c("TYPE", "NATURE", "NOM", "DECA")]
    HYDRO_line <- rbind(HYDRO_line, RUI)
  }
  
  ## Export de HYDRO_line
  HYDRO_line <- st_difference(st_zm(st_transform(HYDRO_line,2154)), 
                              st_zm(st_make_valid(st_combine(st_transform(HYDRO_polygon,2154)))))
  assign("HYDRO_line", st_transform(HYDRO_line, 2154), envir=globalenv())
  cat("        L'object sf HYDRO_line a été ajouté à l'environnement \n")
  
  
  # Création de l'objet sf HYDRO_point
  HYDRO_point <- st_sf(st_sfc(),crs=4326)
  HYDRO_point$TYPE <- character(0)
  HYDRO_point$NATURE <- character(0)
  HYDRO_point$NOM <- character(0)
  HYDRO_point$ROT <- character(0)
  
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

  ## Hydronyme
  NOO <- load("BDTOPO_V3:toponymie_hydrographie", "NOO")
  if(!is.null(NOO)){
    NOO$NATURE <- ifelse(NOO$classe_de_l_objet == 'Détail hydrographique', NOO$nature_de_l_objet, NOO$TYPE)
    NOO$NATURE <- ifelse(NOO$classe_de_l_objet != 'Détail hydrographique', NOO$classe_de_l_objet, NOO$TYPE)
    NOO$NOM <- as.character(NOO$graphie_du_toponyme)
    NOO <- NOO[,c("TYPE", "NATURE", "NOM", "ROT")]
    HYDRO_point <- rbind(HYDRO_point, NOO)
  }
  
  ## Détail hydrographique
  MAR <- load("BDTOPO_V3:detail_hydrographique", "MAR")
  if(!is.null(MAR)){
    MAR$NATURE <- as.character(MAR$nature)
    MAR$NOM <- as.character(MAR$toponyme)
    MAR <- MAR[,c("TYPE", "NATURE", "NOM", "ROT")]
    HYDRO_point <- rbind(HYDRO_point, MAR)
  }
  
  ## Export de HYDRO_point
  assign("HYDRO_point", st_zm(st_transform(HYDRO_point, 2154)), envir=globalenv())
  cat("        L'object sf HYDRO_point a été ajouté à l'environnement \n \n")
  options(warn=1)
} # end function




