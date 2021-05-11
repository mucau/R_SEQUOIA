# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("osmdata")) {install.packages("osmdata")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("lwgeom")) {install.packages("lwgeom")}

OSMonPARCA <- function(rep=F){
  options(warn=-1) # Désactivation des warnings
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de PARCA
  cat("        OpenStreetMap retenu  \n")
  PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n")
  assign("PARCA", PARCA, envir=globalenv())

  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_PARCA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

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

# Adresse du serveur
  #new_url <- "https://overpass.kumi.systems/api/interpreter"
  #new_url <- "http://overpass.openstreetmap.ie/api/interpreter"
  #new_url <- "https://lz4.overpass-api.de/api/interpreter"
  new_url <- "http://www.overpass-api.de/api/interpreter"
  set_overpass_url(new_url)
  options(warn=-1)

# Création du .shp "COMS_polygon"
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON5, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'admin_level', value = '8') %>%
    osmdata_xml(filename = 'OSM_COMS')

  COMS_OSM_POLY <- st_transform(st_read('OSM_COMS', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE), 2154)
  SEQUOIA:::WRITE(COMS_OSM_POLY, repout2, paste(NAME,"COMS_polygon.shp",sep="_"))

# Création du .shp "COMS_line"
  COMS_OSM_LINE <- st_transform(st_read('OSM_COMS', layer = 'lines', quiet=T, stringsAsFactors = FALSE), 2154)
  SEQUOIA:::WRITE(COMS_OSM_LINE, repout2, paste(NAME,"COMS_line.shp",sep="_"))

  COMS_OSM_LINE <- st_intersection(COMS_OSM_LINE, TEMPON1)
  SEQUOIA:::WRITE(COMS_OSM_LINE, repout2, paste(NAME,"COM_line.shp",sep="_"))

# Création du .shp "COMS_point"
  COMS_OSM_PTS  <- st_transform(st_read('OSM_COMS', layer = 'points', quiet=T, stringsAsFactors = FALSE), 2154)
  SEQUOIA:::WRITE(COMS_OSM_PTS, repout2, paste(NAME,"COMS_point.shp",sep="_"))

  COMS_OSM_PTS  <- st_centroid(st_intersection(COMS_OSM_POLY[,"name"], TEMPON1))
  SEQUOIA:::WRITE(COMS_OSM_PTS, repout2, paste(NAME,"COM_point.shp",sep="_"))


  # Création de INFRA_polygon
  INFRA_polygon <- st_sf(st_sfc(),crs=2154)%>%
    mutate(TYPE = as.character(NA),
           NATURE = as.character(NA))

  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'building') %>%
    osmdata_xml(filename = 'OSM_BATI')

  BT_polygon <- st_read('OSM_BATI', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE)
  if(nrow(BT_polygon)>0){
    BT_polygon <- st_transform(BT_polygon, 2154) %>%
      mutate(TYPE = 'BT',
             NATURE = as.character(NA)) %>%
      select(TYPE, NATURE)
    INFRA_polygon <- rbind(INFRA_polygon, BT_polygon)
  }

  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'leisure') %>%
    osmdata_xml(filename = 'OSM_SPORT')

  SP_polygon <- st_read('OSM_SPORT', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE)
  if(nrow(SP_polygon)>0){
    SP_polygon <- st_transform(SP_polygon, 2154) %>%
      mutate(TYPE = 'SP',
             NATURE = as.character(leisure)) %>%
      select(TYPE, NATURE)
    INFRA_polygon <- rbind(INFRA_polygon, SP_polygon)
  }

  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'landuse',
                    value = 'cemetery') %>%
    osmdata_xml(filename = 'OSM_CIM')

  CIM_polygon <- st_read('OSM_CIM', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE)
  if(nrow(CIM_polygon)>0){
    CIM_polygon <- st_transform(CIM_polygon, 2154) %>%
      mutate(TYPE = 'CIM',
             NATURE = as.character(NA)) %>%
      select(TYPE, NATURE)
    INFRA_polygon <- rbind(INFRA_polygon, CIM_polygon)
  }

  if(nrow(INFRA_polygon)>0){
    assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
    cat("        L'object sf INFRA_polygon a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
    message("        OSM : Pas de polygones détectés sur l'emprise \n")
  }

  # Création de INFRA_line
  INFRA_line <- st_sf(st_sfc(),crs=2154) %>%
    mutate(TYPE = as.character(NA),
           NATURE = as.character(NA),
           NAME = as.character(NA),
           DECA = as.character(NA))

  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'railway') %>%
    osmdata_xml(filename = 'OSM_RAIL')

  VF_line <- st_read('OSM_RAIL', layer = 'lines', quiet=T, stringsAsFactors = FALSE)
  if(nrow(VF_line)>0){
    VF_line <- st_transform(VF_line, 2154) %>%
      mutate(TYPE = 'VF',
             NATURE = as.character(NA),
             NAME = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    INFRA_line <- rbind(INFRA_line, VF_line)
  }

  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'power') %>%
    osmdata_xml(filename = 'OSM_POWER')

  LE_line <- st_read('OSM_POWER', layer = 'lines', quiet=T, stringsAsFactors = FALSE)
  if(nrow(LE_line)>0){
    LE_line <- st_transform(LE_line, 2154) %>%
      mutate(TYPE = 'LE',
             NATURE = as.character(NA),
             NAME = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    INFRA_line <- rbind(INFRA_line, LE_line)
  }

  if(nrow(INFRA_line)>0){
    assign("INFRA_line", INFRA_line, envir=globalenv())
    cat("        L'object sf INFRA_line a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_line", INFRA_line, envir=globalenv())
    message("        OSM : Pas de lignes détectés sur l'emprise \n")
  }

  # Création de INFRA_point
  INFRA_point <- st_sf(st_sfc(),crs=2154)%>%
    mutate(TYPE = as.character(NA),
           NATURE = as.character(NA),
           NOM = as.character(NA),
           ROT = as.character(NA))
  assign("INFRA_point", INFRA_point, envir=globalenv())
  cat("        L'object sf INFRA_point a été ajouté à l'environnement \n")

# Création du .shp "ROAD_line"
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_xml(filename = 'OSM_INFRA')

  ROUTE_line <- st_read('OSM_INFRA', layer = 'lines', quiet=T, stringsAsFactors = FALSE)
  if(nrow(ROUTE_line)>0){
    ROUTE_line <- st_transform(ROUTE_line, 2154, layer_options = "ENCODING=UTF-8")
    ROUTE_line$TYPE <- ROUTE_line$highway
    ROUTE_line$NAME <- as.character("")

    for(a in 1:nrow(ROUTE_line)){
      TAG <- as.character(ROUTE_line[a,"other_tags"])[1]
      N <- str_locate(TAG,'"ref\"=>\"')[,2]
      if (!is.na(N)){
        N2 <- str_locate_all(TAG,'\"')[[1]][,1]
        b=1
        while (N2[b]<N){
          b=b+1
        }
        REF <- str_sub(TAG, N+1, N2[b+1]-1)
        ROUTE_line[a, c("NAME")] <- REF
      } else {ROUTE_line[a, c("NAME")] <- ROUTE_line[a, c("name")]}
    }

    ROUTE_line <- ROUTE_line %>%
      mutate(NATURE = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)

    SEQUOIA:::WRITE(ROUTE_line, repout2, paste(NAME,"ROAD_line.shp",sep="_"))
  } else {
    message("        OSM : Pas de routes détectés sur l'emprise \n")
  }
  options(warn=1) # Activation des warnings
}
