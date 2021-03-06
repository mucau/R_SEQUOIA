#' @title SEQUOIA
#' Appel rapide des fonctions SEQUOIA
#' @encoding UTF-8
#' @description La fonction \code{SEQUOIA} génère une interface utilisateur permettant de faire appel aux fonctions du package.
#' @usage SEQUOIA(enrg)
#' @param enrg LOGICAL. Si \code{FALSE}, la fonction lance l'interface utilisateur.
#' @details La fonction ajoute les champs suivants : \code{SURF_SIG} (Surface cartographique selon R), \code{SURF_COEFF} (coefficient de correction) et \code{SURF_COR} (Surface cadastrale corrigée)
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     SEQUOIA(F)
#' @export
#' 
#' @import tcltk stringr utils

SEQUOIA <- function(enrg=FALSE) {
  # Lancement des library
  #packages <- c("devtools", "prettydoc",
  #         "data.table", "dplyr", "plyr", "foreign",
  #         "gdalUtils", "lwgeom", "osmdata", "R.utils",
  #         "raster", "RCurl", "readxl", "rlang", "rlist",
  #        "rvest", "sf", "smoothr", "stringr", "tcltk",
  #         "tidyverse", "openxlsx", "XML", "xml2",
  #         "measurements", "nngeo", "sp", "elevatr", "svGUI", "installr",
  #         "ClimClass", "geosphere", "plotly", "stars")
  #package.check <- lapply(
  #  packages,
  #  FUN = function(x) {
  #    if (!require(x, character.only = TRUE)) {
  #      install.packages(x, dependencies = TRUE)
  #      library(x, character.only = TRUE)
  #    }
  #  })

  vers <- packageVersion("SEQUOIA")
  message("\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n ")
  message(paste0("SEQUOIA ", vers, " est prêt ! \n"))

  form <- c("Téléchargements de données",
            "Aide à la création d'une cartographie ponctuelle",
            "Outils cartographiques")

  RES1 <- select.list(form,
                      multiple = FALSE,
                      title = paste0("SEQUOIA ",
                                     vers,
                                     " : Prenez de la hauteur sur votre forêt !"),
                      graphics = T)

  if (!length(RES1)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  if ("Téléchargements de données" %in% RES1) { # Téléchargements des données SEQUOIA
    message("Téléchargements de données")

    form <- c("1 Données INPN/INSEE               ",
              "2 Données IGN                      ")
    RES3 <- select.list(form,
                        multiple = T,
                        title = "Quelles données voulez-vous ?",
                        graphics = T)
    if (!length(RES3)){stop("Aucune sélection effectuée > Traitement annulé \n")}

    if ("1 Données INPN/INSEE               " %in% RES3) {
      message("1 Données INPN/INSEE")
      repRdata <- tk_choose.dir(default = getwd(),
                                caption = "Choisir le répertoire des archives .Rdata")
      if (!length(repRdata)){stop("Traitement annulé","\n")}
      SEQUOIA:::INSEEtoRDATA(repRdata)
      SEQUOIA:::INPNtoRDATA(repRdata)
    }

    if ("2 Données IGN                      " %in% RES3) {
      message("2 Données IGN")
      SEQUOIA:::LOADfromIGN(F)
    }
  } # Fin Téléchargements des données SEQUOIA

    if ("Aide à la création d'une cartographie ponctuelle" %in% RES1) { # Aide à la création d'une cartographie ponctuelle

      form <- c("1 Conversion .html vers .xlsx",
                "2 Création PARCA",
                "3 Création UA",
                "4 Finalisation UA",
                "Op1 Création du fond vectoriel",
                "Op2 Conversion ROAD vers ROUTE")

      RES2 <- select.list(form,
                          multiple = T,
                          title = "Aide à la création d'une cartographie ponctuelle",
                          graphics = T)
      if (!length(RES2)){stop("Aucune sélection effectuée > Traitement annulé \n")}

        if ("1 Conversion .html vers .xlsx" %in% RES2) {
          message("1 Conversion .html vers .xlsx")
          SEQUOIA:::HTMLtoXLSX(F, if(exists("repRdata")){repRdata}else{FALSE})
        }
        if ("2 Création PARCA" %in% RES2) {
          message("2 Création PARCA")
          SEQUOIA:::XLSXtoPARCA(F)
        }
        if ("Op1 Création du fond vectoriel" %in% RES2) {
          message("3 Création du fond vectoriel")
          SEQUOIA:::CAGEF(if(exists("repPARCA")){repPARCA}else{FALSE},
                          if(exists("CODE")){CODE}else{1})
        }
        if ("3 Création UA" %in% RES2) {
          message("4 Création UA")
          SEQUOIA:::PARCAtoUA(F)
        }
        if ("Op2 Conversion ROAD vers ROUTE" %in% RES2) {
          message("5 Conversion ROAD vers ROUTE")
          SEQUOIA:::ROADtoROUTE(F)
        }
        if ("4 Finalisation UA" %in% RES2) {
          message("6 Finalisation UA")
          Filters <- matrix(c("Shapefile", "*.shp"),1, 2, byrow = TRUE)
          repUA <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))

          if (!length(repUA)){stop("Traitement annulé \n")}
            SEQUOIA:::UAtoUA(repUA)
            if(Erreurs=="OK"){
              SEQUOIA:::UAtoSSPF(repUA)}
        }

    } # Fin Aide à la création d'une cartographie ponctuelle

    if ("Outils cartographiques" %in% RES1) { # Outils cartographiques

      form <- c("MNT sur shapefile",
                "Zonage environnementaux",
                "MH sur shapefile",
                "AAC sur shapefile",
                "Création d'une fiche Climatologique",
                "Géologie sur shapefile",
                "BD Foret sur shapefile")

      RES3 <- select.list(form,
                          multiple = T,
                          title = "Outils cartographiques",
                          graphics = T)
      if (!length(RES3)){stop("Aucune sélection effectuée > Traitement annulé \n")}

        if ("MNT sur shapefile" %in% RES3) {
          if(!exists("repPARCA")) {repPARCA <- F}
          SEQUOIA:::MNTonSHP(repPARCA, NAME)
        }

        if ("MH sur shapefile" %in% RES3) {
          SEQUOIA:::MHonSHP(F)
        }
      
        if ("AAC sur shapefile" %in% RES3) {
          SEQUOIA:::AAConSHP(F)
        }

        if ("Zonage environnementaux" %in% RES3) {
          if(!exists("repRdata")) {repRdata <- F}
          SEQUOIA:::INPNonSHP(F, repRdata)
        }
      
        if ("Création d'une fiche Climatologique" %in% RES3) {
          if(!exists("repPARCA")) {repPARCA <- F}
          SEQUOIA:::CLIMonSHP(repPARCA)
        }

        if ("Géologie sur shapefile" %in% RES3) {
          if(!exists("repPARCA")) {repPARCA <- F}
          SEQUOIA:::GEOLonSHP(F)
        }

      if ("BD Foret sur shapefile" %in% RES3) {
        if(!exists("repPARCA")) {repPARCA <- F}
        SEQUOIA:::BDFORETonSHP(F)
      }
    }
}
