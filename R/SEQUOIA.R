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
#' @import tcltk

# library(tcltk)

SEQUOIA <- function(enrg=FALSE) {
  options(warn=-1)
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

  if (!length(RES1)|RES1==""){stop("Aucune sélection effectuée > Traitement annulé \n")}

  if ("Téléchargements de données" %in% RES1) { # Téléchargements des données SEQUOIA
    message("Téléchargements de données")

    form <- c("1 Données INPN/INSEE               ",
              "2 Données IGN                      ")
    RES3 <- select.list(form,
                        multiple = T,
                        title = "Quelles données voulez-vous ?",
                        graphics = T)
    if (!length(RES3)|RES3==""){stop("Aucune sélection effectuée > Traitement annulé \n")}

    if ("1 Données INPN/INSEE               " %in% RES3) {
      repRdata <- tk_choose.dir(default = getwd(),
                                caption = "Choisir le répertoire des archives .Rdata")
      if (is.na(repRdata)){stop("Traitement annulé","\n")}
      INSEEtoRDATA(repRdata)
      INPNtoRDATA(repRdata)
    }

    if ("2 Données IGN                      " %in% RES3) {
      LOADfromIGN(F)
    }
  } # Fin Téléchargements des données SEQUOIA
  if ("Aide à la création d'une cartographie ponctuelle" %in% RES1) { # Aide à la création d'une cartographie ponctuelle
    
    form <- c("1 Conversion .html vers .xlsx",
              "2 Création PARCA",
              "3 Création UA",
              "4 Finalisation UA",
              "Op1 Création du fond vectoriel",
              "Op2 Conversion ROAD vers ROUTE",
              "Op3 Compilation de donnée SEQUOIA",
              "Op4 Lancement des codes SEQUOIA",
              "Op5 Diagnostic terrain")
    
    RES2 <- select.list(form,
                        multiple = T,
                        title = "Aide à la création d'une cartographie ponctuelle",
                        graphics = T)
    if (!length(RES2)|RES2==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
    
    if ("1 Conversion .html vers .xlsx" %in% RES2) {
      message("1 Conversion .html vers .xlsx")
      HTMLtoXLSX(F, if(exists("repRdata")){repRdata}else{FALSE})
    }
    if ("2 Création PARCA" %in% RES2) {
      message("2 Création PARCA")
      XLSXtoPARCA(F)
    }
    if ("3 Création UA" %in% RES2) {
      message("4 Création UA")
      PARCAtoUA(F)
    }
    if ("4 Finalisation UA" %in% RES2) {
      message("6 Finalisation UA")
      Filters <- matrix(c("Shapefile", "*.shp"),1, 2, byrow = TRUE)
      repUA <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                               filter = matrix(c("ESRI Shapefile", "UA_polygon.shp"), 1, 2, byrow = TRUE))
      
      if (is.na(repUA)){stop("Traitement annulé \n")}
      UAtoUA(repUA)
      if(Erreurs=="OK"){
        UAtoSSPF(repUA)}
    }
    if ("Op1 Création du fond vectoriel" %in% RES2) {
      message("Création du fond vectoriel")
      if(!exists("repPARCA")) {repPARCA <- F}
      CAGEF(repPARCA, F, F)
    }
    if ("Op2 Conversion ROAD vers ROUTE" %in% RES2) {
      message(" Conversion ROAD vers ROUTE")
      ROADtoROUTE(F)
    }
    if ("Op3 Compilation de donnée SEQUOIA" %in% RES2) {
      message("Compilation de donnée SEQUOIA")
      UAtoUAS(F, F)
    }
    if ("Op4 Lancement des codes SEQUOIA" %in% RES2) {
      message("Alllllllllll  SEQUOIA")
      if(!exists("repPARCA")) {repPARCA <- F}
      CAGEF(repPARCA, source_bdt="1 web IGN© BD TOPO®", source_cadastre="1 web IGN© BD Parcellaire®")
      GEOLonSHP(repPARCA)
      INPNonSHP(repPARCA, source_inpn="1 web IGN©")
      MNTonSHP(repPARCA, source_mnt="1 web IGN© altimetrie")
      COURBESonSHP(repPARCA, source_courbes="1 web IGN© Courbes®")
      CLIMonSHP(repPARCA, aurelhy=F, drias=F, dsn=NA)
    }
    if ("Op5 Diagnostic terrain" %in% RES2) {
      message("Diagnostic terrain")
      if(!exists("repPARCA")) {
        Filters <- matrix(c("Shapefile", "*.shp"),1, 2, byrow = TRUE)
        repPARCA <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                                 filter = matrix(c("ESRI Shapefile", "PARCA_polygon.shp"), 1, 2, byrow = TRUE))
      }
      IRConSHP(repPARCA)
      RGBonSHP(repPARCA)
      SCAN25onSHP(repPARCA)
      MNTonSHP(repPARCA, source_mnt="1 web IGN© altimetrie")
    }
    
  } # Fin Aide à la création d'une cartographie ponctuelle
  
  if ("Outils cartographiques" %in% RES1) { # Outils cartographiques
    form <- c("Altimétrie",
              "Climatologie",
              "Enjeux",
              "Géologie: BRGM© BD Charm50® sur shapefile",
              "Recherche de parcelles de personne morale")
    
    RES3 <- select.list(form,
                        multiple = F,
                        title = "Outils cartographiques",
                        graphics = T)
    if (!length(RES3)|RES3==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
    
    if ("Altimétrie" %in% RES3) {
      form <- c("MNT sur shapefile",
                "Courbe de niveau sur shapefile")
      
      RES4 <- select.list(form,
                          multiple = T,
                          title = "Altimétrie",
                          graphics = T)
      if (!length(RES4)|RES4==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
      
      if ("MNT sur shapefile" %in% RES4) {
        if(!exists("repPARCA")) {repPARCA <- F}
        MNTonSHP(repPARCA)
      }
      
      if ("Courbe de niveau sur shapefile" %in% RES4) {
        if(!exists("repPARCA")) {repPARCA <- F}
        COURBESonSHP(F, F)
      }
    }
    
    if ("Climatologie" %in% RES3) {
      if(!exists("repPARCA")) {repPARCA <- F}
      CLIMonSHP(repPARCA)
    }
    
    if ("Enjeux" %in% RES3) {
      form <- c("INPN© Zonage environnementaux sur shapefile",
                "MH sur shapefile")
      
      RES5 <- select.list(form,
                          multiple = T,
                          title = "Enjeux",
                          graphics = T)
      if (!length(RES5)|RES5==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
      
      if ("INPN© Zonage environnementaux sur shapefile" %in% RES5) {
        INPNonSHP(F, F)
      }
      
      if ("MH sur shapefile" %in% RES5) {
        MHonSHP(F)
      }
      
    }
    
    if ("Géologie: BRGM© BD Charm50® sur shapefile" %in% RES3) {
      if(!exists("repPARCA")) {repPARCA <- F}
      GEOLonSHP(F)
    }
    
    if ("Recherche de parcelles de personne morale" %in% RES3) {
      if(!exists("repPPM")) {
        repPPM <- download_lpPM(T)
        assign("repPPM", repPPM, envir=globalenv())
      }
      askme_lpPM(repPPM)
    }
    
  } # fin outils
  options(warn=1)
}
