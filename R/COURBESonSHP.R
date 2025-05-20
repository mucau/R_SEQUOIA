#' @title COURBESonSHP
#' Création de ligne topohgraphique
#' @encoding UTF-8
#' @description
#' La fonction \code{COURBESonSHP} permet de génrer un shapefile de courbe topographique sur une emprise.
#' La fonction demande le département consernée.
#' 
#' @usage COURBESonSHP(shp=F, NAME=NULL, online=F)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @param source_courbes Character. Choix de la source. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection.
#' @details
#' La fonction télécharge les données depuis le serveur \url{https://geoservices.ign.fr/courbes-de-niveau}
#' 
#' @author 
#' Paul CARTERON <\email{carteronpaul@gmail.com}>
#' Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' 
#' @examples
#' ### Fonctionnement :
#'     COURBESonSHP(rep = F)
#' @export
#' 
#' @import tcltk happign sf terra

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)
# library(terra)

COURBESonSHP <- function(rep=F, source_courbes=F){ # Function
  options(warn=-1) # Déctivation des warnings
  message('- - - Courbes de niveau - - -')
  
  message("\n        Lecture du shapefile")
  # Sélection du shapefile
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                                filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout2}
  SHP <- st_read(rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès \n")
  
  # write function
  write <- function(nom, rep, name=NULL){
    if (is.null(name)){
      name <- paste0(NAME, "_", deparse(substitute(nom)), ".shp")
    } else {
      name <-  paste0(NAME, "_", name, ".shp")
    }
    st_write(nom, rep, name, append=FALSE, delete_layer=TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }
  
  # Préparation des emprises
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
  TEMPON1 <- BUF2CONV(SHP, 200) # enveloppe convexe sur tempon de 200m
  
  # Choix de la source
  message("\n        Choix de la source COURBE")
  if(isFALSE(source_courbes)){
    form <- c("1 web IGN© Courbes®",
              "2 loc IGN© Courbes®",
              "3 loc MNT raster")
    
    source_courbes <- select.list(form,
                              multiple = F,
                              title = "Choix de la source cadastrale",
                              graphics = T)
  }
  if (source_courbes==""){stop("Aucune source sélectionnée !")}
  cat(paste0("        ", source_courbes, " retenu \n"))
  
  
  if ("1 web IGN© Courbes®" %in% source_courbes){
    layers_data <- get_layers_metadata("wfs")
  
    invisible(capture.output(suppressMessages(courbes_export <- get_wfs(TEMPON1, 
                                                                        "ELEVATION.CONTOUR.LINE:courbe",
                                                                        spatial_filter = "intersects") |> st_intersection(st_transform(TEMPON1,4326)))))
    if (nrow(courbes_export)>0){
      courbes_export <- st_transform(courbes_export, 2154)
    }
  }
  
  if ("2 loc IGN© Courbes®" %in% source_courbes){
    # Sélection du dossier
    message("\n        Récupération des données")
    courbes_folder  <- tk_choose.dir(default = getwd(),
                                     caption = "Choisir le dossier contenant les courbes départementales")
    if (is.na(courbes_folder)) {stop("Aucun fichier sélectionné >> Traitement annulé")}
    cat("        Les données ont été localisées \n")
    
    message("        Compilation des données des données")
    courbes_shp <- list.files(courbes_folder, "*.shp$",recursive = T)[grep("1_DONNEES_LIVRAISON", list.files(courbes_folder, "*.shp$",recursive = T))]
    sfs <- st_sf(st_sfc())
    st_crs(sfs) <- 2154
    for (x in 1:length(courbes_shp)){
      sf <- st_read(paste0(courbes_folder, "/", courbes_shp[x]), quiet=T)
      sf <- st_transform(sf, 2154)
      sfs <- rbind(sfs, sf)
      cat("        ", x, " fichiers compilés sur ", length(courbes_shp),"\n")
    }
    sfs$lenght <- as.numeric(st_length(sfs))
    sfs <- subset(sfs, lenght!=0)
    cat("        Les courbes ont été compilées avec succès \n")
    
    message("\n        Récupération des courbes de niveau sur l'emprise")
    courbes_export <- st_intersection(st_make_valid(sfs), st_make_valid(TEMPON1))
    cat("        Intersection réalisée avec succès \n")
  }
  
  if ("3 loc MNT raster" %in% source_courbes){
    message("\n        Récupération du MNT")
    mnt_file   <- tk_choose.files(caption = "Choisir le raster MNT.tif",
                            filter = matrix(c("TIF MNT Raster", "MNT.tif"), 1, 2, byrow = TRUE))
    if (!length(mnt_file)){stop("Aucune sélection effectuée > Traitement annulé \n")}
    
    mnt <- rast(mnt_file)
    min_val <- round(global(mnt, min, na.rm = TRUE)$min/5)*5
    max_val <- round(global(mnt, max, na.rm = TRUE)$max/5)*5
    levels <- seq(floor(min_val), ceiling(max_val), by = 5)
    contours <- as.contour(mnt, levels = levels)
    
    contours_sf <- st_as_sf(contours)
    st_geometry_type(contours_sf)
    
    courbes_export <- st_intersection(contours_sf, TEMPON1)
    cat("        Les contours ont été obtenus \n")
  }
  
  # Export du fichier
  if((nrow(courbes_export)>0) & !(is.na(st_crs(courbes_export)))){
    TOPO_line <- courbes_export
    write(TOPO_line, repout)
  }
}
