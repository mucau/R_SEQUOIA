#' @title MNTonSHP
#' Recuperation d'un modele numerique de terrain et creation des contours d'elevation
#' @encoding UTF-8
#' @description 
#' Recuperation d'un modele numerique de terrain et creation des contours d'elevation
#' La fonction \code{MNTonSHP} récupère ou télécharge un modèle numérique de terrain pour une zone d'étude et génère des contours d'élévation topographiques (équidistances à 5 ou 10 mètres) enregistrés au format \code{.shp}.
#' Le MNT récupérée peut correspondre à celui proposer sur l'Amazon Web Service, à celui de l'IGN© BD Alti® ou à celui de l'IGN© RGE Alti 5m
#' @usage MNTonSHP(rep, buffer)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @param buffer NUMERIC. Valeur du tempon (buffer) pour l'emprise de travail. Défault = \code{200}
#' @param source_mnt Character. Choix de la source. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection.
#' @return
#' \item{MNT.tif}{Fichier raster .tiff ;  MNT enregistré dans le répertoire du fichier}
#' \item{TOPO_line}{Objet sf ; contours enregistré dans le répertoire du fichier}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   TOPO_line <- MNTonSHP(rep=F)
#' @export
#' 
#' @import tcltk sf terra happign

# Lancement des library
# library(tcltk)
# library(sf)
# library(terra)
# library(happign)

MNTonSHP <- function(rep=F, buffer=NULL, source_mnt=F){ # Function
  options(warn=-1) # Déctivation des warnings
  message('- - - MNT sur shapefile - - -')

  message("\n        Lecture du shapefile")
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                                filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout1}
  SHP <- st_read(rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès \n")

  # Création d'une emprise
  convex <- function(shp, buffer){
    comb <- st_buffer(st_combine(shp), buffer, nQuadSegs = 30)
    comb <- st_sf(st_cast(comb, 'POLYGON'))
    conv_all <- st_sf(st_sfc())

    for (a in 1:nrow(comb)){
      conv <- st_convex_hull(comb[a,])
      st_crs(conv_all) <- st_crs(conv)
      conv_all <- rbind(conv_all, conv)
    }
    return(conv_all)
  }
  if(!is.null(buffer)){buffer = buffer}else{buffer = 200}
  EMPRISE <- convex(SHP, buffer)

  # Choix de la source MNT
  message("\n        Choix de la source MNT")
  if(isFALSE(source_mnt)){
    form <- c("1 web IGN© altimetrie",
              "2 loc IGN© RGE ALTI 5m")
    
    source_mnt <- select.list(form,
                                  multiple = F,
                                  title = "Choix de la source cadastrale",
                                  graphics = T)
  }
  if (source_mnt==""){stop("Aucune source sélectionnée !")}
  cat(paste0("        ", source_mnt, " retenu \n"))

  # Récupération du MNT > IGN© RGE ALTI 5m"
  if ("1 web IGN© altimetrie" %in% source_mnt){ # § 1 web IGN© altimetrie
    layers <- get_layers_metadata("wms-r", "altimetrie")
    mnt_layer <- layers[3,1] # "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES"
    mns_layer <- layers[4,1] # "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS"
    
    invisible(capture.output(suppressMessages(MNT <- get_wms_raster(SHP, mnt_layer, res = 1, crs = 2154, rgb = FALSE))))
    invisible(capture.output(suppressMessages(MNS <- get_wms_raster(SHP, mns_layer, res = 1, crs = 2154, rgb = FALSE))))
    MNH = MNS - MNT
    MNH[MNH < 0] <- NA  # Remove negative value 
    MNH[MNH > 50] <- 40 # Remove height more than 50m
    
    writeRaster(MNT, paste(repout,paste0(NAME,"_MNT.tif"), sep="/"), overwrite=TRUE)
    writeRaster(MNS, paste(repout,paste0(NAME,"_MNS.tif"), sep="/"), overwrite=TRUE)
    writeRaster(MNH, paste(repout,paste0(NAME,"_MNH.tif"), sep="/"), overwrite=TRUE)
    cat("        Les MNT, MNS et MNH ont été exportés dans", repout, "\n")
  }# Fin § 1 web IGN© altimetrie
  
  
  if ("2 loc IGN© RGE ALTI 5m" %in% source_mnt){ # § 2 loc IGN© RGE ALTI 5m
    # Récupération du RGE 5m
    REP_MNT  <- tk_choose.dir(default = getwd(),
                              caption = "Choisir le dossier contenant les couches RASTER")
    if (is.na(REP_MNT)) {stop("Aucun fichier sélectionné >> Traitement annulé")}
    asc_list <- list.files(REP_MNT, "*.asc$", recursive = T, full.names = T)
    
    # Détection des dalles RGE
    ta <- SEQUOIA::RGEALTI_TA
    intersects  <- st_intersects(ta, EMPRISE, sparse = F)
    ta_intersecting <- ta[apply(intersects, 1, any), ]
    ta_filtred <- as.data.frame(ta_intersecting)[,1]
    
    # Creation du MNT
    if (length(ta_filtred) > 0){
      ## Filtrer les fichiers ASC correspondant à ta_filtred
      pattern <- paste0(ta_filtred, ".asc")
      asc_files <- asc_list[grepl(paste(pattern, collapse = "|"), asc_list)]
      crs_source <- "EPSG:2154"
      
      ## Lire les rasters
      raster_list <- lapply(asc_files, function(file) {
        raster <- rast(file)
        crs(raster) <- crs_source  # Définir le CRS si non défini
        return(raster)
      })
      
      ## Fusion des raster
      if (length(raster_list) > 0) {
        merged_raster <- do.call(merge, raster_list)
        crs_target <- "EPSG:2154"
        MNT <- project(merged_raster, crs_target)
        writeRaster(MNT, paste(repout,paste0(NAME,"_MNT.tif"), sep="/"), overwrite=TRUE)
        cat("        Le MNT a été exportés dans", repout, "\n")
      }
    }
  } # § 2 loc IGN© RGE ALTI 5m
  
  options(warn=1) # Activation des warnings
} # Fin function
