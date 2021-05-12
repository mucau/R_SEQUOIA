# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("osmdata")) {install.packages("osmdata")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}

GEOLonSHP <- function(shp = F, NAME=NULL){
  message('- - - Géologie sur emprise shapefile - - -')
  if(isFALSE(shp)){
    shp_rep  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if(!length(shp)){stop("        Aucun fichier sélectionné >> Traitement annulé")}

  # Lecture des données
  message('        Lecture des données')
  shp <- st_read(shp_rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès  \n \n")
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
  if(is.null(NAME)){
    NAME <- winDialogString("Entrer le nom du fichier de sortie (optionnel) : ", "")
  }
  if(!length(NAME)) {NAME <- ""} else {NAME <- paste0(NAME,"_")}

  # Récupération des départements
  message('        Récupération des départements')
  new_url <- "http://www.overpass-api.de/api/interpreter"
  set_overpass_url(new_url)

  DATA <- opq(bbox=st_bbox(st_transform(st_buffer(shp, 20000), "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'admin_level', value = '6') %>%
    osmdata_xml(filename = 'OSM_deps')

  deps <- st_transform(st_read('OSM_deps', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE), 2154)
  dep <- unique(st_intersection(st_make_valid(st_sf(st_combine(shp))), deps))
  cat("        ", nrow(dep), "départements détectés  \n \n")

  # Téléchargement des données géologiques
  message('        Téléchargement des données géologiques')
  for (a in 1:nrow(dep)){
    dep_code <- as.character(as.data.frame(dep[a,"other_tags"])[1])
    dep_code <- str_pad(str_sub(dep_code,
                                str_locate(dep_code, "INSEE")[2]+5,
                                str_locate(dep_code, "INSEE")[2]+6), 3, "left", '0')

    if (!(dep_code %in% c('024', '033', '047', NA))){
      url <- paste0('https://infoterre.brgm.fr/telechargements/BDCharm50/GEO050K_HARM_', dep_code, '.zip')
      TD = tempdir() # répertoire temporaire
      TF = tempfile(tmpdir=TD, fileext=".zip") # fichier temporaire
      download.file(url, TF, method="libcurl", quiet=F)
      unzip(TF, exdir=TD)
      cat("        Le département", dep_code, "a été téléchargé \n \n")
    } else {
      cat("        Le département", dep_code, "n'est pas disponible \n \n")
    }
  }

  # Compilation des fichiers géologique
  message('        Compilation des fichiers géologique')
  list_geol <- list.files(TD, "*S_FGEOL_2154.shp", recursive=T)

  geol <- st_sf(st_sfc())
  for (b in 1:length(list_geol)) {
    geol_dep <- st_read(paste(TD, list_geol[b], sep="/"),
                        options = "ENCODING=UTF-8", agr="constant", quiet=T)
    st_crs(geol) <- st_crs(geol_dep)
    geol <- rbind(geol, geol_dep)
  } # fin boucle geol
  cat("        Les BD_Charm départementales ont été compilées \n \n")

  # Intersection avec l'emprise
  message("        Intersection avec l'emprise")
  geol_shp <- st_intersection(st_transform(st_sf(st_union(shp)), 2154),
                              st_transform(geol,2154))
  cat("        Intersection effectuée \n")
  geol_shp$SURF_SIG <- st_area(geol_shp)
  cat("        Calcul des SURF_SIG effectué \n \n")

  # Export des données
  message("        Export des données")
  SEQUOIA:::WRITE(geol_shp, dirname(shp_rep), paste0(NAME,"geol_polygon.shp"))
  #st_write(geol_shp, dsn=dirname(shp_rep), layer ="geol.shp", update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
}
