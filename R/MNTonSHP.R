# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("elevatr")) {install.packages("elevatr")}
if (!require("raster")) {install.packages("raster")}
if (!require("smoothr")) {install.packages("smoothr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("stars")) {install.packages("stars")}

MNTonSHP <- function(REP_SHP=F, NAME=NULL, TEMP=NULL){ # Function
  options(warn=-1) # Déctivation des warnings
  message('- - - MNT et courbes de niveau - - -')

# Sélection du fichier .shp
  if(isFALSE(REP_SHP)){
    REP_SHP  <- tk_choose.files(caption = "Choisir le fichier .shp",
                                filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(REP_SHP)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture du fichier .shp
  SHP <- st_read(REP_SHP, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès \n")
  if (grepl("PARCA", REP_SHP )){
    NAME <- str_sub(REP_SHP,
                    str_locate_all(REP_SHP,'/')[[1]][nrow(str_locate_all(REP_SHP,'/')[[1]]),1]+1,
                    str_locate(REP_SHP,'_PARCA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())
  }
  if (grepl("UA", REP_SHP )){
    NAME <- str_sub(REP_SHP,
                    str_locate_all(REP_SHP,'/')[[1]][nrow(str_locate_all(REP_SHP,'/')[[1]]),1]+1,
                    str_locate(REP_SHP,'_UA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())
  }

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
  if(!is.null(TEMP)){buffer = TEMP}else{buffer = 200}
  EMPRISE <- convex(SHP, buffer)

  # Choix de la source de données
  form <- c("IGN© BD ALTI®",
            "IGN© RGE ALTI 5m",
            "Amazon Web Services")

  RES <- select.list(form,
                     multiple = T,
                     title = "Choix du MNT à utiliser",
                     graphics = T)
  if (!length(RES)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Récupération du MNT > IGN© RGE ALTI 5m"
  if(RES=="IGN© RGE ALTI 5m") {
    # Boucle IGN© RGE ALTI 5m"
    REP_MNT  <- tk_choose.dir(default = getwd(),
                              caption = "Choisir le dossier contenant les couches RASTER")
    if (!length(REP_MNT)) {stop("Aucun fichier sélectionné >> Traitement annulé")}

    if(RES=="IGN© RGE ALTI 5m") {larg=5000}else{larg=1000}
    LISTE_ASC <- list.files(REP_MNT, "*.asc$",recursive = T)
    REP_MNT <- paste(REP_MNT, dirname(LISTE_ASC[1]), sep="/")

    REP_ASC <- list()
    a=1
    for (x in 1:2){ # x min et x max
      for(y in 3:4) { # y min et y max
        options(scipen=999) # Evite la notation scientifique
        if(extent(SHP)[x]<round(extent(SHP)[x]/larg)*larg) {
          XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/larg)*larg-larg),1,3), 4, "left", pad = "0")
        } else {
          XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/larg)*larg),1,3), 4, "left", pad = "0")
        }

        if(extent(SHP)[y]>round(extent(SHP)[y]/larg)*larg) {
          YMIN <- str_sub(as.character(round(extent(SHP)[y]/larg)*larg+larg),1,4)
        } else {
          YMIN <- str_sub(as.character(round(extent(SHP)[y]/larg)*larg),1,4)
        }

        REP_ASC[[a]] <- paste(REP_MNT,
                              paste("RGEALTI_FXX", XMIN, YMIN, "MNT_LAMB93_IGN69.asc", sep="_"), sep="/")
        a = a+1
      }
    } # x min et x max
    REP_ASC <- unique(REP_ASC) # Suppression du doublons en cas de dalle unique
    cat("        Détection de", length(REP_ASC)," dalles \n")

    if (length(REP_ASC)>1){ # Merge des dalles
      MNT <- raster::raster(REP_ASC[[1]][1])
      for (a in 2:length(REP_ASC)){
        ASC <- raster::raster(REP_ASC[[a]][1])
        MNT <- raster::merge(MNT, ASC)
      }
    } else {
      MNT <- raster::raster(REP_ASC[[1]][1])
    }
    MNT <- raster::crop(MNT, as(st_transform(EMPRISE, st_crs(2154)), "Spatial"))
    raster::writeRaster(MNT, paste0(dirname(REP_SHP),"/","MNT.tif"), overwrite=TRUE)
    cat("        Le MNT a été récupéré avec succès \n")
  }# Fin boucle IGN© RGE ALTI 5m"

  # Récupération du MNT > IGN© BD ALTI®
  if(RES=="IGN© BD ALTI®") {
    # Boucle IGN© BD ALTI®
    REP_MNT  <- tk_choose.dir(default = getwd(),
                              caption = "Choisir le dossier contenant les couches RASTER")
    REP_MNT  <- str_replace(REP_MNT,"Ã©","é")
    if (!length(REP_MNT)) {stop("Aucun fichier sélectionné >> Traitement annulé")}

    LISTE_ASC <- list.files(REP_MNT, "*.asc$") # Détection des fichiers .asc dans le répertoire sélectionné
    IDU <- str_sub(LISTE_ASC[1], 1, 12) # Détection du MNT IGN© BD ALTI®
    cat("        Le MNT utilisé est",IDU,"\n")

    REP_ASC <- list()
    # Boucle en cas de chevauchement sur plusieurs dalles
    a=1
    for (x in 1:2){ # x min et x max
      for(y in 3:4) { # y min et y max
        options(scipen=999) # Evite la notation scientifique
        if (str_sub(IDU,10,11)=="75") { # Récupération de la dalle pour la IGN (C) BD ALTI 75 m (R)
          if(extent(SHP)[x]<round(extent(SHP)[x]/75000)*75000) {
            XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/75000)*75000-75000),1,3), 4, "left", pad = "0")
          } else {
            XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/75000)*75000),1,3), 4, "left", pad = "0")
          }

          if(extent(SHP)[y]>round(extent(SHP)[y]/75000)*75000) {
            YMIN <- str_sub(as.character(round(extent(SHP)[y]/75000)*75000+75000),1,4)
          } else {
            YMIN <- str_sub(as.character(round(extent(SHP)[y]/75000)*75000),1,4)
          }
        } else {
          if (str_sub(IDU,10,11)=="25") { # Récupération de la dalle pour la IGN (C) BD ALTI 25 m (R)
            if(extent(SHP)[x]<round(extent(SHP)[x]/25000)*25000) {
              XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/25000)*25000-25000),1,3), 4, "left", pad = "0")
            } else {
              XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/25000)*25000),1,3), 4, "left", pad = "0")
            }

            if(extent(SHP)[y]>round(extent(SHP)[y]/25000)*25000) {
              YMIN <- str_sub(as.character(round(extent(SHP)[y]/25000)*25000+25000),1,4)
            } else {
              YMIN <- str_sub(as.character(round(extent(SHP)[y]/25000)*25000),1,4)
            }
          }
        }
        REP_ASC[[a]] <- paste(REP_MNT, paste0(paste(IDU,"FXX", XMIN, YMIN, "MNT_LAMB93_IGN69", sep="_"), ".asc"), sep="/") # Récupération du répertoire de la dalle IGN
        a = a+1
      } # Fin y
    } # Fin x

    REP_ASC <- unique(REP_ASC) # Suppression du doublons en cas de dalle unique
    cat("        Détection de", length(REP_ASC)," dalles \n")

    if (length(REP_ASC)>1){ # Merge des dalles
      MNT <- raster::raster(REP_ASC[[1]][1])
      for (a in 2:length(REP_ASC)){
        ASC <- raster::raster(REP_ASC[[a]][1])
        MNT <- raster::merge(MNT, ASC)
      }
    } else {
      MNT <- raster::raster(REP_ASC[[1]][1])
    }
    #MNT <- spTransform(MNT, CRS("+init=epsg:2154"))
    MNT <- raster::crop(MNT, as(st_transform(EMPRISE, st_crs(2154)), "Spatial"))
    raster::writeRaster(MNT, paste0(dirname(REP_SHP),"/","MNT.tif"), overwrite=TRUE) # Créer ton raster MNT au format tiff, spécifie le chemin de sauvegarde, lisible ensuite sans problème sur QGis

    cat("        Le MNT a été récupéré avec succès \n")
  } # Fin boucle IGN© BD ALTI®
  # Récupération du MNT > Amazon Web Services
  if(RES=="Amazon Web Services") {
    # Boucle Amazon Web Services

    SiteMnt <- function(shp, zoom=14, epsg=NULL) {             # Fonction SiteMnt, shp=polygone de périmètre de la forêt, epsg qui par défaut est nul mais qui peut être précisé
      if (is.numeric(zoom) & zoom < 15) {            # Test pour être sur de pouvoir utiliser les variables d'entrée
        zoom = max(zoom, 9)                                               # Règle la précision
        x <- get_elev_raster(as(shp, "Spatial"), z = zoom, src = "aws")   # Transformation de l'objet sf en objet sp puis récupération du MNS
        if (!is.null(epsg)) {
          x <- projectRaster(x, crs=CRS(paste0('+init=EPSG:',epsg)))      # Si système de projection demandé, l'attribuer au MNT
        }
        return(x)
      } else {
        warning("buffer et zoom doivent etre des entiers.")
      }
    }

    MNT <- SiteMnt(EMPRISE, epsg=2154) # Création du MNT par appel de la fonction de récupération du MNT sur emprise + reprojection suivant l'EPSG souhaité
    MNT <- crop(MNT, as(st_transform(EMPRISE, 2154), "Spatial"))

    raster::writeRaster(MNT, paste0(dirname(REP_SHP),"/","MNT.tif"), overwrite=TRUE) # Créer ton raster MNT au format tiff, spécifie le chemin de sauvegarde, lisible ensuite sans problème sur QGis

    cat("Le MNT a été généré avec succès \n")
  } # Fin boucle Amazon Web Services
  if(!length(MNT)){stop("Une erreur est survenue dans la création du MNT")}


  # Création du .shp TOPO_line non lissé
  test = st_as_stars(MNT)
  ext = range(getValues(MNT), na.rm = TRUE)
  ext = round(ext,-1)
  
  if (Sys.info()["sysname"]=="Windows"){
    equid <- winDialogString("Entrer l'équidistance :", "10")
  }else {
    equid <- readline(prompt="Entrer l'équidistance :")
  }
  
  if(!length(equid)){equid <- as.numeric("10")} else {equid <- as.numeric(equid)}
  
  brk = seq(ext[1], ext[2]-equid, by = equid)
  courbeNiv = st_contour(test, contour_lines = T, breaks = brk)
  st_crs(courbeNiv) <- 2154
  colnames(courbeNiv) <- c("ELEVATION", "geometry")
  
  TOPO_line <- st_intersection(courbeNiv, EMPRISE)
  
  if(RES=="IGN© BD ALTI®" | RES=="IGN© RGE ALTI 5m") {
    TOPO_line <- smoothr::smooth(TOPO_line, method = "ksmooth") # Lissage du .shp TOPO_line
    cat("        Les contours ont été liséés avec succès \n")
  }
  
  if(is.null(NAME)){
    if (Sys.info()["sysname"]=="Windows"){
      NAME <- winDialogString("Entrer le nom du fichier de sortie:", "")
    }else {
      NAME <- readline(prompt="Entrer le nom du fichier de sortie:")
    }
  }
  if(!length(NAME)||NAME=="") {NAME <- ""} else {NAME <- paste0(NAME,"_")}
  
  SEQUOIA::WRITE(TOPO_line, dirname(REP_SHP), paste(NAME,"TOPO_line.shp",sep="_"))
  options(warn=1) # Activation des warnings
} # Fin function
