# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("R.utils")) {install.packages("R.utils")}
if (!require("rvest")) {install.packages("rvest")}
if (!require("xml2")) {install.packages("xml2")}
if (!require("readxl")) {install.packages("readxl")}
if (!require("rlang")) {install.packages("rlang")}
if (!require("smoothr")) {install.packages("smoothr")}

XLSXtoPARCA <- function(rep=F){
  options(warn=-1) # Désactivation des warnings
  message('- - - Téléchargement de PARCA_polygon depuis .XLSX - - -')
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales",
                            filter = matrix(c("Excel", ".xlsx", "Excel 97-2003", ".xls"), 2, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucun fichier sélectionnée !")}

  # Import des données .xlsx
  message('        Lecture des données')
  XLSX <- read_excel(rep)
  cat("        Le fichier .xlsx a été chargé avec succès \n")

  # Création des répertoires de sorties
  dir.create(paste(dirname(rep),"SIG",sep="/"))
  dir.create(paste(dirname(rep),"SIG","0 SORTIE",sep="/"))
  dir.create(paste(dirname(rep),"SIG","1 RASTER",sep="/"))
  dir.create(paste(dirname(rep),"SIG","2 PSG",sep="/"))
  dir.create(paste(dirname(rep),"SIG","3 TEMPO",sep="/"))
  dir.create(paste(dirname(rep),"SIG","4 ZONAGE",sep="/"))

  # Détection d'erreur de saisie
  XLSX$DOUBLON <- duplicated(XLSX)
  DOUBLON <- XLSX %>% filter(DOUBLON %in% TRUE)
  if (nrow(DOUBLON)>0){
    cat("        Attention: Des doublons ont été détectés dans la saisie \n")}
  XLSX <- unique(XLSX)[,-18]
  NBR <- nrow(XLSX)
  cat(paste0("        ", NBR, " références cadastrales saisies \n"))
  SURF_CA <- unique(XLSX["SURF_CA"])
  if(nrow(SURF_CA)==1 || as.character(SURF_CA)=="0") {
    cat("        Attention: les surfaces cadastrales n'ont pas été saisies \n")}

  # Fonctions de chargement
  cadastre.Etalab <- function(XLSX, PARCA){
    if (nrow(PARCA)>0){
      IDS <- unlist(unique(as.data.frame(PARCA["IDU"])[1]))
      XLSX <- XLSX %>% filter(!(IDU %in% IDS))
      cat(paste0("        ", nrow(XLSX), " références cadastrales à chercher \n"))
    }
    message('\n        Téléchargement ETALAB')
    DEP_URL <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/departements"
    COM_URL <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes"

    XLSX <- XLSX %>%
      mutate(DEP_CODE = str_pad(DEP_CODE, 2, "left", pad = "0"),
             COM_CODE = str_pad(COM_CODE, 3, "left", pad = "0"),
             ID_CAD = paste(DEP_CODE,COM_CODE,sep = ""))
    ID_CAD <- as.data.frame(unique(XLSX["ID_CAD"]))

    PARCELLES_SF <- st_sf(st_sfc())
    DEP_ERR <- list()

    for (a in 1:nrow(ID_CAD)) { # Boucle de téléchargement
      DEP <- str_sub(ID_CAD[a,1],1,2)
      COM <- ID_CAD[a,1]
      URL <- paste(COM_URL, DEP, sep="/")
      LISTE_URL <- list(html_attr(html_nodes(read_html(URL), "a"), "href"))

      if(grepl(COM, LISTE_URL)!=F) {
        URL <- paste(COM_URL, DEP, COM, paste("cadastre-", COM, "-parcelles.json.gz", sep=""), sep="/")
        TD = tempdir() # répertoire temporaire
        TF = tempfile(tmpdir=TD, fileext=".gz") # fichier temporaire
        download.file(URL, TF, method="libcurl", quiet=T) # Téléchargement du fichier URL sous le nom TF dans TD
        R.utils::gunzip(TF, remove=F) # Extraction de TF dans TD
        PARCELLE_SF <- st_read(str_replace(TF, ".gz", ""), quiet=T) # Lecture du fichier sf
        st_crs(PARCELLES_SF) <- st_crs(PARCELLE_SF) # Changement du système de projection
        PARCELLES_SF <- rbind(PARCELLES_SF,PARCELLE_SF)
        cat("        La commune", COM, "a été téléchargée \n")
      } else {
        message("        La commune ", COM, " n'est pas disponible")
      }
    } # Fin de boucle de téléchargement

    message('\n        Sélection des parcelles')
    if(nrow(PARCELLES_SF )>0){ # Boucle de contenu
      PARCELLES_SF<- st_sf(st_transform(PARCELLES_SF , 2154), agr="constant")
      ## Sélection des PARCELLES de la propriété
      DATA_PARCAS <- PARCELLES_SF %>% # Création de l'IDU
        mutate(IDU = str_sub(id,3,14))
      DATA_PARCAS <- DATA_PARCAS %>%
        filter(DATA_PARCAS$IDU %in% XLSX$IDU) # Filtre des polygones sur IDU
      ## Création du .shp de la propriété
      SHP <- merge(x = DATA_PARCAS, y = XLSX, by = "IDU") # Jointure des tables
      DATA_PARCAS <- SHP %>%
        mutate(SURF_CA = contenance/10000) %>%
        dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU, PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs
      ET_PARCA <- st_transform(DATA_PARCAS, 2154)
      cat(paste0("        ", nrow(ET_PARCA), " parcelles cadastrales ont été sélectionnées \n"))
    } # Sélection

    if(nrow(ET_PARCA)>0){
      ET_PARCA <- st_transform(ET_PARCA, st_crs("+init=epsg:2154"))
      if (nrow(PARCA)==0){
        PARCA <- ET_PARCA
      } else {
        PARCA <- unique(rbind(PARCA, ET_PARCA))
      }
      CODE <- 2 # Code de création de l'habillage
      return(PARCA)
      assign("CODE", CODE, envir=globalenv())
    }
  } # Fin fonction

  cadastre.IGN <- function(XLSX, PARCA) {
    if (nrow(PARCA)>0){
      IDS <- unlist(unique(as.data.frame(PARCA["IDU"])[1]))
      XLSX <- XLSX %>% filter(!(IDU %in% IDS))
      cat(paste0("        ", nrow(XLSX), " références cadastrales à chercher \n"))
    }

    DEPS <- unique(XLSX["DEP_CODE"])
    cat(paste0("        Départements à charger: ", unlist(as.list(DEPS[1]))))
    cat("\n")

    shp_rep  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                       filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
    if (!length(shp_rep)){stop("Aucune BD Parcellaire sélectionnée !")}

    message('\n        Lecture IGN (c) BD Parcellaire (r)')
    SHP <- st_read(shp_rep, agr="constant", quiet=T, stringsAsFactors = F)
    cat('        La BD Parcellaire (r) a été chargée \n')
    SHP <- SHP %>%
      mutate(IDU = paste(CODE_DEP, CODE_COM,COM_ABS,SECTION,NUMERO, sep=""))
    XLSX_PC <- XLSX %>%
      mutate(IDU = paste(DEP_CODE, IDU, sep=""))
    cat("        L'IDU a été crée \n \n")

    message('        Sélection des parcelles')
    BD_PARCAS <- SHP %>%
      filter(SHP$IDU %in% XLSX_PC$IDU)

    BD_PARCA <- merge(x = BD_PARCAS[,"IDU"], y = XLSX_PC, by = "IDU", all.x=T, all.y=F) # Jointure des tables
    BD_PARCA <- unique(BD_PARCA) %>% # Suppression doublon résiduel
      dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU, PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs
    cat(paste0("        ", nrow(BD_PARCA), " parcelles cadastrales ont été sélectionnées \n"))

    if(nrow(BD_PARCA)>0){
      BD_PARCA <- st_transform(BD_PARCA, st_crs(2154))
      if (nrow(PARCA)==0){
        PARCA <- BD_PARCA
      } else {
        PARCA <- rbind(PARCA, BD_PARCA)
        PARCA <- PARCA %>%
          mutate(IDU = as.character(IDU),
                 REG_CODE = as.character(REG_CODE),
                 REG_NOM = as.character(REG_NOM),
                 DEP_CODE = as.character(DEP_CODE),
                 DEP_NOM = as.character(DEP_NOM),
                 COM_CODE = as.character(COM_CODE),
                 COM_NOM = as.character(COM_NOM),
                 PROP = as.character(PROP),
                 ADRESSE = as.character(ADRESSE),
                 PREFIXE = as.character(PREFIXE),
                 SECTION = as.character(SECTION),
                 N_PARCA = as.character(N_PARCA),
                 LIEUDIT = as.character(LIEUDIT),
                 OCCUP_SOL = as.character(OCCUP_SOL))
        PARCA <- unique(PARCA)
      }
      assign("rep_BDPARCA", dirname(shp_rep), envir=.GlobalEnv)
      CODE <- 2 # Code de création de l'habillage
      return(PARCA)
      assign("CODE", CODE, envir=globalenv())
    }
  } # Fin fonction

  cadastre.Edigeo <- function(XLSX, PARCA){
    if (nrow(PARCA)>0){
      IDS <- unlist(unique(as.data.frame(PARCA["IDU"])[1]))
      XLSX <- XLSX %>% filter(!(IDU %in% IDS))
      cat(paste0("        ", nrow(XLSX), " références cadastrales à chercher \n"))
    }

    message('\n        Import des EDIGEO')
    RES5 <- tk_choose.files(default = rep, caption = "Selectionner la commande .zip")
    unzip(RES5, exdir=dirname(RES5))

    LISTE_BZ <- list.files(dirname(RES5), "*.bz2", recursive=T)
    for (a in 1:length(LISTE_BZ)) {
      bunzip2(paste(dirname(RES5), LISTE_BZ[a], sep="/"))
    }
    LISTE_TAR <- list.files(dirname(RES5), "*.tar", recursive=T)
    for (a in 1:length(LISTE_TAR)) {
      untar(paste(dirname(RES5), LISTE_TAR[a], sep="/"), exdir=paste(dirname(RES5), dirname(LISTE_TAR[a]), sep="/"))
    }
    LISTE_THF <- list.files(dirname(RES5), "*.THF", recursive=T)

    PARCELLES_SF <- st_sf(st_sfc())
    for (a in 1:length(LISTE_THF)) {
      PARCELLE_SF <- st_read(paste(dirname(RES5),LISTE_THF[a],sep="/"),"PARCELLE_id", quiet=T)
      st_crs(PARCELLES_SF) <- st_crs(PARCELLE_SF) # Changement du système de projection
      PARCELLES_SF <- rbind(PARCELLES_SF,PARCELLE_SF)
    }
    message('\n        Sélection des parcelles')
    if(str_count(XLSX[1,9])>12){XLSX <- XLSX %>% # Création de l'IDU
      mutate(IDU = str_sub(IDU,3,14))}
    EDIGEO_PARCAS <- PARCELLES_SF %>%
      filter(PARCELLES_SF$IDU %in% XLSX$IDU) # Filtre des polygones sur IDU
    SHP <- merge(x = EDIGEO_PARCAS, y = XLSX, by = "IDU") # Jointure des tables
    EDIGEO_PARCA <- unique(SHP) %>% # Suppression doublon résiduel
      dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU, PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs

    if(nrow(EDIGEO_PARCA)>0){
      EDIGEO_PARCA <- st_transform(EDIGEO_PARCA, st_crs("+init=epsg:2154"))
      if (nrow(PARCA)==0){
        PARCA <- EDIGEO_PARCA
      } else {
        PARCA <- unique(rbind(PARCA, EDIGEO_PARCA))
      }
      rep_EDIGEO <- paste(dirname(RES5),dirname(dirname(LISTE_THF[1])), sep="/")
      assign("rep_EDIGEO", rep_EDIGEO, envir=.GlobalEnv)
      CODE <- 3 # Code de création de l'habillage
      assign("CODE", CODE, envir=globalenv())
      return(PARCA)
    }
  } # Fin fonction

  PARCA <- st_sf(st_sfc())

  repeat{
    # Choix de la source
    message('\n        Choix de la source')
    form <- c("1 cadastre.data.gouv.fr",
              "2 IGN (c) BD Parcellaire (r)",
              "3 cadastre.gouv.fr")

    RES1 <- select.list(form,
                        multiple = F,
                        title = "Choix de la source cadastrale",
                        graphics = T)
    if (!length(RES1)){stop("Aucune source sélectionnée !")}

    if ("1 cadastre.data.gouv.fr" %in% RES1) {
      cat("        cadastre.gouv.fr retenu \n") ; PARCA <- cadastre.Etalab(XLSX, PARCA) ; CODE=1}
    if ("2 IGN (c) BD Parcellaire (r)" %in% RES1) {
      cat("        IGN (c) BD Parcellaire (r) retenu \n") ;PARCA <- cadastre.IGN(XLSX, PARCA) ; CODE=2}
    if ("3 cadastre.gouv.fr" %in% RES1) {
      cat("        cadastre.gouv.fr retenu \n") ;PARCA <- cadastre.Edigeo(XLSX, PARCA) ; CODE=3}

    ERR <- NBR-nrow(PARCA)

    if(ERR>0){
      message("\n        Une ou plusieurs références sont manquantes")
      ask <- askYesNo("Voulez-vous utiliser une autre source de données?")
      if(isFALSE(ask)){break}
      if(is.na(ask)){break}
    } else {
      break
    }
  } # fin repeat

  # Exportation du fichier .shp "PARCA"
  if(nrow(PARCA)>0){ # Boucle création "PARCA"
    message("\n        Export des fichiers")

    PARCA <- unique(PARCA) # Suppression des doublons résiduels

    repout2 <- paste(dirname(rep),"SIG","2 PSG",sep="/")
    repout3 <- paste(dirname(rep),"SIG","3 TEMPO",sep="/")
    assign("repout2", repout2, envir=globalenv())
    assign("repout3", repout3, envir=globalenv())
    
    if (Sys.info()["sysname"]=="Windows"){
      NAME <- winDialogString("Entrer le nom du fichier de sortie:", "")
    }else {
      NAME <- readline(prompt="Entrer le nom du fichier de sortie:")
    }
    assign("NAME", NAME, envir=globalenv())

    SEQUOIA:::WRITE(PARCA, repout2, paste(NAME,"PARCA-ARCHIVE_polygon.shp",sep="_"))
    SEQUOIA:::WRITE(PARCA, repout2, paste(NAME,"PARCA_polygon.shp",sep="_"))

    assign("repPARCA", paste(repout2, paste(NAME,"PARCA_polygon.shp",sep="_"), sep="/"), envir=globalenv())

    assign("PARCA", PARCA, envir=globalenv())
    assign("CODECA", CODE, envir=globalenv())
  } # Fin boucle création PARCA
  options(warn=1) # Désactivation des warnings
} # Fin fonction
