# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}

BDTOPO_HYDRO <- function(PARCA=F, source=F){
  options(warn=-1) # Désactivation des warnings
  if(isFALSE(PARCA)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
    if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
    cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n")
    assign("PARCA", PARCA, envir=globalenv())
  }
  if (!length(PARCA)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  if(isFALSE(source)) {
    form <- c("IGN© BD TOPO® Hydrographie Départementale",
              "IGN© BD TOPO® Hydrographie Métropole")

    source <- select.list(form,
                       multiple = T,
                       title = "Source des données ?",
                       graphics = T)
  }
  if (!length(source)){stop("Aucune sélection effectuée > Traitement annulé \n")}

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

  # IGN© BD TOPO® Hydrographie Départementale
  if ("IGN© BD TOPO® Hydrographie Départementale" %in% source) {
    if(!exists("repTOPO")) {
      repBDTOPO <- tk_choose.dir(default= getwd(),
                                 caption = "Choisir le répertoire de l'IGN (c) BD TOPO (r)")
    } else {
      repBDTOPO <- repTOPO
    }
    if (!length(repBDTOPO)){stop("Aucune sélection effectuée > Traitement annulé \n")}

    # Fonction de chargement de la BD_TOPO
    load <- function(x, repBDTOPO){
      rep <- paste(repBDTOPO,
                   list.files(path = repBDTOPO, pattern = x, recursive = T)[1],
                   sep="/")

      shp <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
      st_crs(shp)<-st_crs(2154)
      return(shp)
    }

    # Création de HYDRO_polygon
    HYDRO_polygon <- st_sf(st_sfc(),crs=2154)%>%
      mutate(TYPE = as.character(NA),
             NATURE = as.character(NA))

    RESO <-load("RESERVOIR.shp", repBDTOPO)
    RESO <- st_intersection(RESO, TEMPON4)
    if(nrow(RESO)>0){
      RESO <- RESO %>%
        mutate(TYPE = as.character('RESO')) %>%
        select(TYPE, NATURE)
      HYDRO_polygon <- rbind(HYDRO_polygon, RESO)}

    SURFO <-load("SURFACE_HYDROGRAPHIQUE.shp", repBDTOPO)
    SURFO <- st_intersection(SURFO, TEMPON4)
    if(nrow(SURFO)>0){
      SURFO <- SURFO %>%
        mutate(TYPE = as.character(case_when(PERSISTANC == 'Permanent' ~ 'SURFO',
                                             PERSISTANC == 'Intermittent' ~ 'SURFOi',
                                             PERSISTANC == 'Inconnue' ~ 'SURFOi'))) %>%
        select(TYPE, NATURE)
      HYDRO_polygon <- rbind(HYDRO_polygon, SURFO)}

    assign("HYDRO_polygon", HYDRO_polygon, envir=globalenv())
    cat("        L'object sf HYDRO_polygon a été ajouté à l'environnement \n")

    # Création de HYDRO_line
    HYDRO_line <- st_sf(st_sfc(),crs=2154)%>%
      mutate(TYPE = as.character(NA),
             NATURE = as.character(NA),
             NAME = as.character(NA),
             DECA = as.character(NA))

    CANO <-load("CANALISATION.shp", repBDTOPO)
    CANO <- st_intersection(CANO, TEMPON4)
    if(nrow(CANO)>0){
      CANO <- CANO %>%
        mutate(TYPE = as.character('CANO'),
               NATURE = as.character(NA),
               NAME = as.character(NA),
               DECA = as.character(NA)) %>%
        select(TYPE, NATURE, NAME, DECA)
      HYDRO_line <- rbind(HYDRO_line, CANO)}

    TRONCON_EAU <-load("TRONCON_HYDROGRAPHIQUE.shp", repBDTOPO)
    TRONCON_EAU <- st_intersection(TRONCON_EAU, TEMPON4)
    if(nrow(TRONCON_EAU)>0){
      TRONCON_EAU <- TRONCON_EAU %>%
        mutate(TYPE = as.character(case_when(PERSISTANC == 'Permanent' ~ 'RU',
                                             PERSISTANC == 'Intermittent' ~ 'RUi',
                                             PERSISTANC == 'Inconnue' ~ 'RUi')),
               NAME = NOM_C_EAU,
               DECA = as.character(NA)) %>%
        select(TYPE, NATURE, NAME, DECA)
      HYDRO_line <- rbind(HYDRO_line, TRONCON_EAU)}

    HYDRO_line <- st_difference(HYDRO_line, st_make_valid(st_combine(HYDRO_polygon)))
    assign("HYDRO_line", HYDRO_line, envir=globalenv())
    cat("        L'object sf HYDRO_line a été ajouté à l'environnement \n")

    # Création de HYDRO_point
    HYDRO_point <- st_sf(st_sfc(),crs=2154)%>%
      mutate(TYPE = as.character(NA),
             NATURE = as.character(NA),
             NOM = as.character(NA),
             ROT = as.character(NA))

    POINT_EAU <-load("DETAIL_HYDROGRAPHIQUE.shp", repBDTOPO)
    POINT_EAU <- st_intersection(POINT_EAU, TEMPON4)
    if(nrow(POINT_EAU)>0){
      POINT_EAU <- POINT_EAU %>%
        mutate(TYPE = 'PTSO',
               NOM = TOPONYME,
               ROT = as.character(NA)) %>%
        select(TYPE, NATURE, NOM, ROT)
      HYDRO_point <- rbind(HYDRO_point, POINT_EAU)}

    HYDRONYME <-load("TOPONYMIE_HYDROGRAPHIE.shp", repBDTOPO)
    HYDRONYME <- st_intersection(HYDRONYME, TEMPON4)
    if(nrow(HYDRONYME)>0){
      HYDRONYME <- HYDRONYME %>%
        mutate(TYPE = 'NOMO',
               NOM = GRAPHIE,
               ROT = as.character(NA)) %>%
        select(TYPE, NATURE, NOM, ROT)
      HYDRO_point <- rbind(HYDRO_point, HYDRONYME)}

    assign("HYDRO_point", HYDRO_point, envir=globalenv())
    cat("        L'object sf HYDRO_point a été ajouté à l'environnement \n")

  } # fin dép

  # IGN© BD TOPO® Hydrographie Métropole
  if ("IGN© BD TOPO® Hydrographie Métropole" %in% source) {
    if(!exists("repBDTOPO")) {
      repBDTOPO <- tk_choose.dir(default= getwd(),
                                 caption = "Choisir le répertoire du dossier'IGN BD TOPO HYDRO'")}
    if (!length(repBDTOPO)){stop("Aucune sélection effectuée > Traitement annulé \n")}

    REG_CODE <- unique(as.data.frame(PARCA[,"REG_CODE"])[1])

    LISTE_SHP <- c("CANALISATION_EAU",
                   "HYDRONYME",
                   "POINT_EAU",
                   "RESERVOIR_EAU",
                   "SURFACE_EAU",
                   "TRONCON_EAU")

    HYDRO <- function(REG_CODE, N){ # Début fonction HYDRO
      SHPS <- st_sf(st_sfc())

      for (a in 1:length(REG_CODE)){
        rep <- paste(repBDTOPO, as.character(REG_CODE[a,1]), sep="/")
        SHP <- st_read(paste(rep, paste0(paste(LISTE_SHP[N], as.character(REG_CODE[a,1]), sep="_"),".shp"), sep="/"),
                       quiet=T,
                       options = "ENCODING=windows-1252")
        st_crs(SHPS) <- st_crs(SHP)
        SHPS <- rbind(SHPS, SHP)
      }

      if(nrow(SHPS)>0){
        SHPS <- st_make_valid(st_transform(SHPS, 2154))
        SHP <- st_intersection(SHPS, TEMPON4)
        return(SHP)
      }
    } # Fin fonction hydro

    # Création de l'objet sf HYDRO_polygon
    HYDRO_polygon <- st_sf(st_sfc(),crs=2154)%>%
      mutate(TYPE = as.character(NA),
             NATURE = as.character(NA))

    RESERVOIR_EAU <- HYDRO(REG_CODE, N=4) %>%
      mutate(TYPE = as.character("RESO")) %>%
      select(TYPE, NATURE)
    if(nrow(RESERVOIR_EAU)>0){
      HYDRO_polygon <- rbind(HYDRO_polygon, RESERVOIR_EAU)
    }

    SURFACE_EAU <- HYDRO(REG_CODE, N=5) %>%
      mutate(TYPE = as.character(case_when(REGIME == 'Permanent' ~ 'SURFO',
                                           REGIME == 'Intermittent' ~ 'SURFOi',
                                           REGIME == 'Inconnue' ~ 'SURFOi'))) %>%
      select(TYPE, NATURE)
    if(nrow(SURFACE_EAU)>0){
      HYDRO_polygon <- rbind(HYDRO_polygon, SURFACE_EAU)
    }

    assign("HYDRO_polygon", HYDRO_polygon, envir=globalenv())
    cat("        L'object sf HYDRO_polygon a été ajouté à l'environnement \n")

    # Création de l'objet sf HYDRO_line
    HYDRO_line <- st_sf(st_sfc(),crs=2154)%>%
      mutate(TYPE = as.character(NA),
             NATURE = as.character(NA),
             NAME = as.character(NA),
             DECA = as.character(NA))

    CANALISATION_EAU <- HYDRO(REG_CODE, N=1) %>%
      mutate(TYPE = as.character('CANO'),
             NATURE = as.character(NA),
             NAME = as.character(NA),
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    if(nrow(CANALISATION_EAU)>0){
      HYDRO_line <- rbind(HYDRO_line, CANALISATION_EAU)
    }

    TRONCON_EAU <- HYDRO(REG_CODE, N=6) %>%
      mutate(TYPE = as.character(case_when(REGIME == 'Permanent' ~ 'RU',
                                           REGIME == 'Intermittent' ~ 'RUi',
                                           REGIME == 'Inconnue' ~ 'RUi')),
             NATURE = as.character(NA),
             NAME = NOM,
             DECA = as.character(NA)) %>%
      select(TYPE, NATURE, NAME, DECA)
    if(nrow(TRONCON_EAU)>0) {
      HYDRO_line <- rbind(HYDRO_line, TRONCON_EAU)
    }

    HYDRO_line <- st_difference(HYDRO_line, st_make_valid(st_combine(HYDRO_polygon)))
    assign("HYDRO_line", HYDRO_line, envir=globalenv())
    cat("        L'object sf HYDRO_line a été ajouté à l'environnement \n")

    # Création de l'objet sf HYDRO_point
    HYDRO_point <- st_sf(st_sfc(),crs=2154)%>%
      mutate(TYPE = as.character(NA),
             NATURE = as.character(NA),
             NOM = as.character(NA),
             ROT = as.character(NA))

    HYDRONYME <- HYDRO(REG_CODE, N=2) %>%
      mutate(TYPE = as.character("NOMO"),
             ROT = as.integer(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    if(nrow(HYDRONYME)>0){
      HYDRO_point <- rbind(HYDRO_point, HYDRONYME)
    }

    POINT_EAU <- HYDRO(REG_CODE, N=3) %>%
      mutate(TYPE = as.character("PTSO"),
             NOM = as.character(NA),
             ROT = as.integer(NA)) %>%
      select(TYPE, NATURE, NOM, ROT)
    if(nrow(POINT_EAU)>0){
      HYDRO_point <- rbind(HYDRO_point, POINT_EAU)
    }

    assign("HYDRO_point", HYDRO_point, envir=globalenv())
    cat("        L'object sf HYDRO_point a été ajouté à l'environnement \n \n")
  } # fin mét
options(warn=1) # Activation des warnings
}

