# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("osmdata")) {install.packages("osmdata")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("lwgeom")) {install.packages("lwgeom")}

BDTOPOonPARCA <- function(rep=F, repBDTOPO=F){
  options(warn=-1) # Désactivation des warnings
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de PARCA
  cat("        IGN© BD TOPO® retenu  \n")
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

  if(isFALSE(repBDTOPO)) {
    repBDTOPO <- tk_choose.dir(default= getwd(),
                               caption = "Choisir le répertoire de l'IGN (c) BD TOPO (r)")
  }
  if (!length(repBDTOPO)){stop("Aucune sélection effectuée > Traitement annulé \n")}

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

  # Fonction de chargement de la BD_TOPO
  load <- function(x, repertoire){
    rep <- paste(repBDTOPO,
                 list.files(path = repertoire, pattern = x, recursive = T)[1],
                 sep="/")

    shp <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
    st_crs(shp)<-st_crs(2154)
    return(shp)
  }

  # Création du .shp "COMS_polygon"
  COMS <-load("COMMUNE.shp", repBDTOPO)
  COMS <- COMS %>%
    mutate(TMP = st_intersects(., TEMPON5, sparse=F)) %>%
    filter(TMP == TRUE)
  SEQUOIA:::WRITE(COMS, repout2, paste(NAME,"COMS_polygon.shp",sep="_"))

  # Création du .shp "COMS_line"
  linearize <- function(x){
    line <- st_cast(x[1],'MULTILINESTRING', warn = F)
    line <- st_intersection(line)
    line <- st_collection_extract(line,"LINESTRING")
    line$covered = line %>% st_covered_by(.) %>% lengths > 1
    line <- line %>% filter(!covered)
    line <- st_cast(line[1],'LINESTRING', warn = F)
    return(line)
  }
  COMS_line <- linearize(COMS)
  SEQUOIA:::WRITE(COMS_line, repout2, paste(NAME,"COMS_line.shp",sep="_"))

  COM_line <- st_intersection(COMS_line, TEMPON1)
  SEQUOIA:::WRITE(COM_line, repout2, paste(NAME,"COM_line.shp",sep="_"))

  # Création du .shp "COMS_point"
  COMS_point <- st_centroid(COMS) %>%
    mutate(name=NOM)%>%
    select(name)
  SEQUOIA:::WRITE(COMS_point, repout2, paste(NAME,"COMS_point.shp",sep="_"))

  COM <- st_intersection(COMS, TEMPON1)
  COM_point <- st_centroid(COM) %>%
    mutate(name=NOM)%>%
    select(name)
  SEQUOIA:::WRITE(COM_point, repout2, paste(NAME,"COM_point.shp",sep="_"))


  # Création de INFRA_polygon
  INFRA_polygon <- st_sf(st_sfc(),crs=2154)%>%
    mutate(TYPE = as.character(NA),
           NATURE = as.character(NA))

  BT <-load("BATIMENT.shp", repBDTOPO)
  BT <- st_intersection(BT, TEMPON4)
  if(nrow(BT)>0){
    BT <- BT%>%
      mutate(TYPE = as.character('BT')) %>%
      select(TYPE, NATURE)
  INFRA_polygon <- rbind(INFRA_polygon, BT)}

  CIM <-load("CIMETIERE.shp", repBDTOPO)
  CIM <- st_intersection(CIM, TEMPON4)
  if(nrow(CIM)>0){
    CIM <- CIM %>%
      mutate(TYPE = as.character('CIM')) %>%
      select(TYPE, NATURE)
    INFRA_polygon <- rbind(INFRA_polygon, CIM)}

  SP <-load("TERRAIN_DE_SPORT.shp", repBDTOPO)
  SP <- st_intersection(SP, TEMPON4)
  if(nrow(SP)>0){
    SP <- SP %>%
      mutate(TYPE = as.character('SP')) %>%
      select(TYPE, NATURE)
    INFRA_polygon <- rbind(INFRA_polygon, SP)}

  CSTsurf <-load("CONSTRUCTION_SURFACIQUE.shp", repBDTOPO)
  CSTsurf <- st_intersection(CSTsurf, TEMPON4)
  if(nrow(CSTsurf)>0){
    CSTsurf <- CSTsurf %>%
      mutate(TYPE = as.character('CSTsurf')) %>%
      select(TYPE, NATURE)
    INFRA_polygon <- rbind(INFRA_polygon, CSTsurf)}

  if(nrow(INFRA_polygon)>0){
    assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
    cat("        L'object sf INFRA_polygon a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
    message("        OSM : Pas de polygones détectés sur l'emprise \n")
  }


  # Création de INFRA_line
  INFRA_line <- st_sf(st_sfc(),crs=2154)%>%
    mutate(TYPE = as.character(NA),
           NATURE = as.character(NA),
           NAME = as.character(NA),
           DECA = as.character(NA))

  CSTline <-load("CONSTRUCTION_LINEAIRE.shp", repBDTOPO)
  CSTline <- st_intersection(CSTline, TEMPON4)
  if(nrow(CSTline)>0){
    CSTline <- CSTline %>%
      mutate(TYPE = as.character('CSTline'),
             NAME = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    INFRA_line <- rbind(INFRA_line, CSTline)}

  ORO <-load("LIGNE_OROGRAPHIQUE.shp", repBDTOPO)
  ORO <- st_intersection(ORO, TEMPON4)
  if(nrow(ORO)>0){
    ORO <- ORO %>%
      mutate(TYPE = as.character('ORO'),
             NAME = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    INFRA_line <- rbind(INFRA_line, ORO)}

  LE <-load("LIGNE_ELECTRIQUE.shp", repBDTOPO)
  LE <- st_intersection(LE, TEMPON4)
  if(nrow(LE)>0){
    LE <- LE %>%
      mutate(TYPE = as.character('LE'),
             NATURE = VOLTAGE,
             NAME = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    INFRA_line <- rbind(INFRA_line, LE)}

  VF <-load("TRONCON_DE_VOIE_FERREE.shp", repBDTOPO)
  VF <- st_intersection(VF, TEMPON4)
  if(nrow(VF)>0){
    VF <- VF %>%
      mutate(TYPE = as.character('VF'),
             NAME = TOPONYME,
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    INFRA_line <- rbind(INFRA_line, VF)}

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

  CSTpts <-load("CONSTRUCTION_PONCTUELLE.shp", repBDTOPO)
  CSTpts <- st_intersection(CSTpts, TEMPON4)
  if(nrow(CSTpts)>0){
    CSTpts <- CSTpts %>%
      mutate(TYPE = as.character('CSTpts'),
             NOM = as.character(NA),
             ROT = as.character(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    INFRA_point <- rbind(INFRA_point, CSTpts)}

  PY <-load("PYLONE.shp", repBDTOPO)
  PY <- st_intersection(PY, TEMPON4)
  if(nrow(PY)>0){
    PY <- PY %>%
      mutate(TYPE = as.character('PY'),
             NATURE = as.character(NA),
             NOM = as.character(NA),
             ROT = as.character(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    INFRA_point <- rbind(INFRA_point, PY)}

  TOPOBT <-load("TOPONYMIE_BATI.shp", repBDTOPO)
  TOPOBT <- st_intersection(TOPOBT, TEMPON4)
  if(nrow(TOPOBT)>0){
    TOPOBT <- TOPOBT %>%
      mutate(TYPE = as.character('NOM'),
             NOM = GRAPHIE,
             ROT = as.character(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    INFRA_point <- rbind(INFRA_point, TOPOBT)}

  TOPOLD <-load("TOPONYMIE_LIEUX_NOMMES.shp", repBDTOPO)
  TOPOLD <- st_intersection(TOPOLD, TEMPON4)
  if(nrow(TOPOLD)>0){
    TOPOLD <- TOPOLD %>%
      mutate(TYPE = as.character('NOM'),
             NOM = GRAPHIE,
             ROT = as.character(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    INFRA_point <- rbind(INFRA_point, TOPOLD)}

  TOPOLD <-load("TOPONYMIE_LIEUX_NOMMES.shp", repBDTOPO)
  TOPOLD <- st_intersection(TOPOLD, TEMPON4)
  if(nrow(TOPOLD)>0){
    TOPOLD <- TOPOLD %>%
      mutate(TYPE = as.character('NOM'),
             NOM = GRAPHIE,
             ROT = as.character(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    INFRA_point <- rbind(INFRA_point, TOPOLD)}

  TOPOTR <-load("TOPONYMIE_TRANSPORT.shp", repBDTOPO)
  TOPOTR <- st_intersection(TOPOTR, TEMPON4)
  if(nrow(TOPOTR)>0){
    TOPOTR <- TOPOTR %>%
      mutate(TYPE = as.character('NOM'),
             NOM = GRAPHIE,
             ROT = as.character(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    INFRA_point <- rbind(INFRA_point, TOPOTR)}

  if(nrow(INFRA_point)>0){
    assign("INFRA_point", INFRA_point, envir=globalenv())
    cat("        L'object sf INFRA_point a été ajouté à l'environnement \n")
  } else {
    assign("INFRA_point", INFRA_point, envir=globalenv())
    message("        OSM : Pas de points détectés sur l'emprise \n")
  }

  # Création de ROAD_line
  ROAD <-load("TRONCON_DE_ROUTE.shp", repBDTOPO)
  ROAD <- st_intersection(ROAD, TEMPON4)
  if(nrow(ROAD)>0){
    ROAD <- ROAD %>%
    mutate(TYPE = as.character(case_when(CL_ADMIN == 'Autoroute' ~ "RN",
                                         CL_ADMIN == 'Départementale' ~ "RD",
                                         CL_ADMIN == 'Nationale' ~ "RN",
                                         is.na(CL_ADMIN) & NATURE == 'Bretelle' ~ "RC",
                                         is.na(CL_ADMIN) & NATURE == 'Chemin' ~ "PN",
                                         is.na(CL_ADMIN) & NATURE == 'Escalier' ~ "PN",
                                         is.na(CL_ADMIN) & NATURE == 'Piste cyclable' ~ "PN",
                                         is.na(CL_ADMIN) & NATURE == 'Rond-point' ~ "RC",
                                         is.na(CL_ADMIN) & NATURE == 'Route à 1 chaussée' ~ "RC",
                                         is.na(CL_ADMIN) & NATURE == 'Route à 2 chaussées' ~ "RC",
                                         is.na(CL_ADMIN) & NATURE == 'Route empierrée' ~ "RF",
                                         is.na(CL_ADMIN) & NATURE == 'Sentier' ~ "PN")),
           NAME = as.character(case_when(is.na(NUMERO) ~ NOM_1_G,
                                         !is.na(NUMERO) ~ NUMERO)),
           DECA = as.character(NA)) %>%
    select(TYPE, NATURE, NAME, DECA) %>%
    filter(!(NATURE %in% 'Piste cyclable'))

  SEQUOIA:::WRITE(ROAD, repout2, paste(NAME,"ROAD_line.shp",sep="_"))}
  options(warn=1) # Activation des warnings
}
