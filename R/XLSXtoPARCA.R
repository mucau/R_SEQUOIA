#' @title XLSXtoPARCA
#' Creation d'un parcellaire cadastral .shp a partir d'une matrice cadastrale .xlsx
#' @encoding UTF-8
#' @description 
#' La fonction \code{XLSXtoPARCA} génère deux parcellaires cadastrals au format .shp (EPSG 2154) à partir d'une matrice cadastral formatée au format .xlsx .
#' @usage XLSXtoPARCA(rep)
#' @param rep Character. Adresse du fichier \code{.xlsx}, tableur Excel contenant la matrice cadastrale. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param source_cadastre Character. Source du casdastre utilisée. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection de la source.
#' @return
#' \item{PARCA_polygon}{Fichier shapefile ; Parcellaire cadastrale .shp EPSG 2154 pour post-traitement}
#' \item{PARCA-ARCHIVE_polygon}{Fichier shapefile ; Parcellaire cadastrale .shp EPSG 2154 pour archivage}
#' \item{PARCA_polygon}{Objet sf ; "PARCA_polygon" dans l'environnement de travail}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'    XLSXtoPARCA(rep=F, source_cadastre=F)
#' @export
#' @import tcltk sf openxlsx

# Lancement des library
# library(tcltk)
# library(openxlsx)
# library(sf)

XLSXtoPARCA <- function(rep=F, source_cadastre=F){
  message('- - - Téléchargement de PARCA_polygon depuis .XLSX - - -')
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales",
                            filter = matrix(c("Excel", ".xlsx", "Excel 97-2003", ".xls"), 2, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucun fichier sélectionnée !")}

  # Import des données .xlsx
  message('        Lecture des données')
  xlsx <- read.xlsx(rep)
  cat("        Le fichier .xlsx a été chargé avec succès \n")

  # Création des répertoires SEQUOIA
  if (!(dir.exists(paste(dirname(rep),"SIG",sep="/")))) {dir.create(paste(dirname(rep),"SIG",sep="/"))}
  folders <- c("0_OUTPUT", "1_RASTER", "2_VECTOR", "3_OTHER")
  for (a in 1:length(folders)){
    if (!dir.exists(paste(dirname(rep),"SIG",folders[a],sep="/"))){dir.create(paste(dirname(rep),"SIG",folders[a],sep="/"))}
  }

  # Détection des doublons
  doublons <- xlsx[duplicated(xlsx), ]
  if (nrow(doublons)>0){
    xlsx <- unique(xlsx)
    cat("        Attention: Des doublons ont été détectés dans la saisie \n")}
  cat(paste0("        ", nrow(xlsx), " références cadastrales saisies \n"))
  
  # Détection des surfaces
  contient_zero <- any(xlsx$SURF_CA == 0, na.rm = TRUE)
  if(contient_zero) {
    cat("        Attention: des surfaces cadastrales n'ont pas été saisies \n")}
  assign("XLSX", xlsx, envir=globalenv())

  # Création du PARCA
  parca <- data.frame()
  parca$geometry <- st_sfc()
  parca <- st_as_sf(parca)
  parca$REG_CODE = character(0)
  parca$REG_NOM = character(0)
  parca$DEP_CODE = character(0)
  parca$DEP_NOM = character(0)
  parca$COM_CODE = character(0)
  parca$COM_NOM = character(0)
  parca$PROP = character(0)
  parca$ADRESSE = character(0)
  parca$IDU = character(0)
  parca$PREFIXE = character(0)
  parca$SECTION = character(0)
  parca$N_PARCA = character(0)
  parca$LIEUDIT = character(0)
  parca$OCCUP_SOL = character(0)
  parca$TX_BOISE = numeric(0)
  parca$REV_CA = numeric(0)
  parca$SURF_CA = numeric(0)
  parca$SURF_CA = numeric(0)
  st_crs(parca)<-2154
  errors<- xlsx[0,]
  
  repeat{
    # Choix de la source
    if(isFALSE(source_cadastre)){
      form <- c("1 web IGN© BD Parcellaire®",
                "2 loc IGN© BD Parcellaire®",
                "3 web PCI Etalab from cadastre.data.gouv.fr")
      
      source_cadastre <- select.list(form,
                                     multiple = F,
                                     title = "Choix de la source cadastrale",
                                     graphics = T)
    }
    if (source_cadastre==""){stop("Aucune source sélectionnée !")}
    assign("source_cadastre", source_cadastre, envir=globalenv())
    cat("\n")
    
    # Choix de la matrice
    if (nrow(parca)>0){
      matrice <- errors
    } else {
      matrice <- xlsx
    }
    
    if ("1 web IGN© BD Parcellaire®" %in% source_cadastre) {
      PARCAbyBDPweb(matrice)
      parca <- rbind(parca, PARCA_bdpweb_sf)
      errors<- PARCA_bdpweb_errors
    }
    
    if ("2 loc IGN© BD Parcellaire®" %in% source_cadastre) {
      PARCAbyBDPloc(matrice)
      parca <- rbind(parca, PARCA_bdploc_sf)
      errors<- rbind(errors, PARCA_bdploc_errors)
    }

    if ("3 web PCI Etalab from cadastre.data.gouv.fr" %in% source_cadastre) {
      PARCAbyETALAB(matrice)
      parca <- rbind(parca, PARCA_etalab_sf)
      errors<- PARCA_etalab_errors
    }

    if (nrow(errors>0)){
      message("\n        Une ou plusieurs références sont manquantes")
      ask <- askYesNo("Voulez-vous utiliser une autre source de données?")
      if(isFALSE(ask)){break}
      if(is.na(ask)){break}
      source_cadastre <- F
    } else {
      break
    }
  } # fin repeat

  # Exportation du fichier .shp "PARCA"
  if (length(parca)==0){stop("Vérifier vos références")}
  if(nrow(parca)>0){ # Boucle création "PARCA"
    message("\n        Export des fichiers")
    
    ## Suppression des doublons résiduels
    parca <- unique(parca) 
    
    ## Creation prefixe
    if (Sys.info()["sysname"]=="Windows"){
      NAME <- utils::winDialogString("Entrer le nom de la forêt (préfixe)", "")
    }else {
      NAME <- readline(prompt="Entrer le nom de la forêt (préfixe) :")
    }
    assign("NAME", NAME, envir=globalenv())
    
    ## PARCA-ARCHIVE
    repout <- paste(dirname(rep),"SIG", folders[3], sep="/")
    nom <- paste(NAME,"PARCA-ARCHIVE_polygon.shp",sep="_")
    st_write(parca, repout, nom, append=FALSE, delete_layer = TRUE,
             driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier",nom,"a été exporté dans",repout),"\n")
    
    ## PARCA
    nom <- paste(NAME,"PARCA_polygon.shp",sep="_")
    st_write(parca, repout, nom, append=FALSE, delete_layer = TRUE,
             driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier",nom,"a été exporté dans",repout),"\n")

    assign("repPARCA", paste(dirname(rep),"SIG", folders[3], paste(NAME,"PARCA_polygon.shp",sep="_"), sep="/"), envir=globalenv())
    assign("PARCA", parca, envir=globalenv())
    
  } # Fin boucle création PARCA
} # Fin fonction
# 