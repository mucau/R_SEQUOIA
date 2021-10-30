#' @title CLIMonSHP
#' Recuperation des donnees climatiques sur l'emprise d'un .shp
#' @encoding UTF-8
#' @description 
#' La fonction \code{CLIMonSHP} fournit les données nécéssaire à la production d'une fiche \code{.html} via le Rmakdown \code{CLIM.Rmd}. La fiche fait synthèse des données climatologiques sur une zone détude (\code{.shp} quelconque).
#' La synthèse fournit:
#' \enumerate{
#'   \item Les normales en se basant sur les données issues des stations METEO-FRANCE (ou des données AURELHY)
#'   \item Les prévisions attendues en se basant sur les données Drias.
#' }
#' @usage CLIMonSHP(shp)
#' @param shp Adresse du fichier \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{METEOFRANCE_df}{Dataframe des données METEO-FRANCE}
#' \item{AURELHY_df}{Dataframe des données AURELHY}
#' \item{DRIAS_df}{Dataframe des données DRIAS}
#' \item{RFN_sf_polygon}{Objet sf ; polygones des régions forestières nationales identifiées sur la zone}
#' \item{SER_sf_polygon}{Objet sf ; polygones des sylvoécorégions identifiées sur la zone}
#' @details
#' La fonction seule produit une multitude d'objet dans l'environnement nécessaire à la fonction \code{CLIM.Rmd}.
#' @seealso
#' Pour mieux comprendre, voir les notices des fonctions suivantes:
#' [METEOFRANCEonSHP],[AURELHYonSHP],[BHbyDRIAS]
#' @examples 
#' #' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' ### Fonctionnement :
#'   CLIMonSHP(shp = F)
#' @export
#' 
#' @import tcltk sf dplyr stringr prettydoc plotly utils

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("dplyr")) {install.packages("dplyr")}

CLIMonSHP <- function(shp = F){
  message("- - - Création d'une fiche Climatologique - - -")
  if(isFALSE(shp)){
    shp  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Pas de shapefile sélectionné")}

  # Bloc Aurelhy
  AURELHY <- utils::askYesNo("Voulez-vous intégrer les données AURELHY ?")
  if(is.na(AURELHY)){break}
  
  if(isTRUE(AURELHY)){
    if(exists("repRdata")) {Rdata <- repRdata} else {
      Rdata <- tcltk::tk_choose.dir(caption = "Choisir le dossier contenant les données AURELHY.Rdata")
      if(is.na(Rdata)){stop("Pas de dossier sélectionné")}
    }
  } else {Rdata<-""}

  # Bloc DRIAS
  DRIAS <- askYesNo("Voulez-vous intégrer les données DRIAS ?")
  if(is.na(DRIAS)){break}
  
  if(isTRUE(DRIAS)){
    txt  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .txt de donnée",
                                   filter = matrix(c("Fichier texte", ".txt"), 1, 2, byrow = TRUE))
    if (!length(txt)){stop("Pas de txt sélectionné")}
  } else {txt<-""}

  # Répertoire
  dsn <- setwd("~/")
  ask <- askYesNo(paste0("Le fichier sera exporté dans ", dsn, "\nC'est bon pour vous ?"))
  if(is.na(ask)){break}
  
  if(isFALSE(ask)) {
    dsn <- tcltk::tk_choose.dir(default = dirname(shp), caption = "Choisir le dossier de destination")
    if(is.na(dsn)){dsn <- setwd("~/")}
  }

  # Nom de fichier
  if (Sys.info()["sysname"]=="Windows"){
    name <- utils::winDialogString("Entrer le nom du fichier de sortie:", "")
  }else {
    name <- readline(prompt="Entrer le nom du fichier de sortie:")
  }

  if(!length(name)){name <- paste0("Fiche_du_", format(Sys.time(), '%y.%m.%d'))}

  # Affectation du wd
  setwd("~/")
  save(shp, Rdata, txt, name, file = "PREPA-FICHE.Rdata")

  # Lancement du Rmarkdown
  library(SEQUOIA)
  rep <- path.package("SEQUOIA", quiet = FALSE)
  rmd <- paste(rep, "CLIM.Rmd", sep='/')
  rmarkdown::render(rmd, prettydoc::html_pretty(theme = "cayman"),
                    paste0(name,"_FICHE-CLIM.html"),
                    dsn, encoding = "UTF-8")

  # Export des tableurs de données
  if (isFALSE(AURELHY) & isFALSE(DRIAS)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp)
  }
  if (isTRUE(AURELHY) & isFALSE(DRIAS)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "AURELHY_ombro"=AURELHY_ombro,
                   "AURELHY_etp"=AURELHY_etp)
  }
  if (isFALSE(AURELHY) & isTRUE(DRIAS)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "DRIAS_ombro"=DRIAS_ombro,
                   "DRIAS_etp"=DRIAS_etp)
  }
  if (isTRUE(AURELHY) & isTRUE(DRIAS)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "AURELHY_ombro"=AURELHY_ombro,
                   "AURELHY_etp"=AURELHY_etp,
                   "DRIAS_ombro"=DRIAS_ombro,
                   "DRIAS_etp"=DRIAS_etp)
  }
  openxlsx::write.xlsx(export, paste(dsn, paste0(name, "_climat.xlsx"),sep="/"))
}
