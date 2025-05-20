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
#' \code{\link{METEOFRANCEonSHP}}, \code{\link{AURELHYonSHP}}, \code{\link{BHbyDRIAS}}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   CLIMonSHP(shp = F)
#' @export
#' 
#' @import tcltk openxlsx rmarkdown prettydoc

# Lancement des library
# library(tcltk)
# library(openxlsx)
# library(rmarkdown)
# library(prettydoc)

CLIMonSHP <- function(shp=F, aurelhy=T, drias=T, dsn=T){
  message("- - - Création d'une fiche Climatologique - - -")
  if(isFALSE(shp)){
    shp  <- tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Pas de shapefile sélectionné")}
  NAMEofSHP(shp, actu=F)

  # Bloc Aurelhy
  if(isTRUE(aurelhy)){
    aurelhy <- askYesNo("Voulez-vous intégrer les données AURELHY ?")
    if(is.na(aurelhy)){stop("Pas de réponse sélectionnée")}
  }
  if(isTRUE(aurelhy)){
    if(exists("repRdata")) {Rdata <- repRdata} else {
      Rdata <- tk_choose.dir(caption = "Choisir le dossier contenant les données AURELHY.Rdata")
      if(is.na(Rdata)){stop("Pas de dossier sélectionné")}
    }
  } else {Rdata<-""}

  # Bloc DRIAS
  if(isTRUE(drias)){
    drias <- askYesNo("Voulez-vous intégrer les données DRIAS ?")
    if(is.na(drias)){stop("Pas de réponse sélectionnée")}
  }
  if(isTRUE(drias)){
    txt  <- tk_choose.files(default = "~", caption = "Selectionner le fichier .txt de donnée",
                                   filter = matrix(c("Fichier texte", ".txt"), 1, 2, byrow = TRUE))
    if (is.na(txt)){stop("Pas de txt sélectionné")}
  } else {txt<-""}

  # Répertoire
  if(isTRUE(dsn)){
    ask <- askYesNo(paste0("Le fichier sera exporté dans ", setwd("~/"), "\nC'est bon pour vous ?"))
    if(is.na(ask)){stop("Pas de réponse sélectionnée")}
  }
  if(isFALSE(dsn)) {
    dsn <- tk_choose.dir(default = dirname(shp), caption = "Choisir le dossier de destination")
  } else {dsn <- setwd("~/")}
  if(is.na(dsn)) {
    dsn <- setwd("~/")
  }

  # Nom de fichier
  if(is.na(NAME)){
    if (Sys.info()["sysname"]=="Windows"){
      name <- utils::winDialogString("Entrer le nom du fichier de sortie:", "")
    }else {
      name <- readline(prompt="Entrer le nom du fichier de sortie:")
    }
    if(is.na(name)){name <- paste0("Fiche_du_", format(Sys.time(), '%y.%m.%d'))}
  } else {
    name <- NAME
  }

  # Affectation du wd
  setwd("~/")
  save(shp, Rdata, txt, name, file = "PREPA-FICHE.Rdata")

  # Lancement du Rmarkdown
  library(SEQUOIA)
  rep <- path.package("SEQUOIA", quiet = FALSE)
  rmd <- paste(rep, "CLIM.Rmd", sep='/')
  render(rmd, html_pretty(theme = "cayman"),
         paste0(name,"_FICHE-CLIM.html"),
         dsn, 
         encoding = "UTF-8")

  # Export des tableurs de données
  if (isFALSE(aurelhy) & isFALSE(drias)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp)
  }
  if (isTRUE(aurelhy) & isFALSE(drias)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "AURELHY_ombro"=AURELHY_ombro,
                   "AURELHY_etp"=AURELHY_etp)
  }
  if (isFALSE(aurelhy) & isTRUE(drias)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "DRIAS_ombro"=DRIAS_ombro,
                   "DRIAS_etp"=DRIAS_etp)
  }
  if (isTRUE(aurelhy) & isTRUE(drias)){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "AURELHY_ombro"=AURELHY_ombro,
                   "AURELHY_etp"=AURELHY_etp,
                   "DRIAS_ombro"=DRIAS_ombro,
                   "DRIAS_etp"=DRIAS_etp)
  }
  write.xlsx(export, paste(dsn, paste0(name, "_climat.xlsx"),sep="/"))
}
