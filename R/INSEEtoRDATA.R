#' @title INSEEtoRDATA
#' Recuperation des donnees administratives et creation d'une archive R.data
#' @encoding UTF-8
#' @description 
#' La fonction \code{INSEEtoRDATA} télécharge depuis la plateforme ouverte des données publiques françaises (data.gouv) et du site de l'INSEE plusieurs fichiers de référence administratives. La fonction les sauvegarde ensuite dans une archive .Rdata si un répertoire de sauvegarde est fournit.
#' @usage INSEEtoRDATA(rep)
#' @param rep Répertoire de sortie de l'archive .Rdata. Si \code{NULL}, les données sont justes ajoutées à l'environnement.
#' @details 
#' La fonction produit une archive .Rdata nécessaire à la fonction \code{\link{HTMLtoXLSX}}.
#' @return
#' \item{INSEE_REGS}{Liste des régions. Codes officiels géographiques au 1er janvier 2018.}
#' \item{INSEE_DEPS}{Liste des départements. Codes officiels géographiques au 1er janvier 2018.}
#' \item{INSEE_COMS}{Liste des communes. Codes officiels géographiques au 1er janvier 2018.}
#' @references Les codes des champs de l'INSEE sont explicités sur le site de l'institut : \url{https://www.insee.fr/fr/information/3363504}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INSEEtoRDATA(rep=NULL)
#' @export
#' 
#' @import tcltk

# Lancement des library
# library(tcltk)

INSEEtoRDATA <- function(rep=F){
  message('- - - Téléchargement de données INSEE - - -')
  if(isFALSE(rep)){
    rep  <- tk_choose.dir(default= getwd(),
                          caption = "Choisir le répertoire d'enregistrement")
  }
  if (is.na(rep)){stop("Aucun dossier sélectionnée !")}

  # Téléchargement des données régionale
  url <- "https://www.insee.fr/fr/statistiques/fichier/7766585/v_region_2024.csv"
  TF = tempfile(tmpdir=tempdir(), fileext=".csv")
  download.file(url, TF, method="libcurl")
  INSEE_REGS <- read.csv(TF, header = T, sep = ",")
  
  # Téléchargement des données départementales
  url <- "https://www.insee.fr/fr/statistiques/fichier/7766585/v_departement_2024.csv"
  TF = tempfile(tmpdir=tempdir(), fileext=".csv")
  download.file(url, TF, method="libcurl")
  INSEE_DEPS <- read.csv(TF, header = T, sep = ",")
  
  # Téléchargement des données communales
  url <- "https://www.insee.fr/fr/statistiques/fichier/7766585/v_commune_2024.csv"
  TF = tempfile(tmpdir=tempdir(), fileext=".csv")
  download.file(url, TF, method="libcurl")
  INSEE_COMS <- read.csv(TF, header = T, sep = ",")

  # Ajout des données à l'environnement
  assign("INSEE_REGS", INSEE_REGS,envir=globalenv())
  assign("INSEE_DEPS", INSEE_DEPS,envir=globalenv())
  assign("INSEE_COMS", INSEE_COMS,envir=globalenv())

  # Enrigistrement des fichiers sous un .Rdata
  save(INSEE_REGS, INSEE_DEPS, INSEE_COMS, file=paste(rep,"INSEE.Rdata",sep="/"))
  cat("Les fichiers téléchargés ont été sauvegardés dans ", rep, "\n")
}



