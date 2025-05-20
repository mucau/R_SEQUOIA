#' @title LOADfromIGN
#' Recuperation des donnees administratives et creation d'une archive R.data
#' @encoding UTF-8
#' @description
#' La fonction \code{LOADfromIGN} permet de télécharger rapidement les données de l'IGN (C).
#' \itemize{
#'   \item BD Parcellaire®
#'   \item BD TOPO®
#'   \item BD FORET® V2
#'   \item RGE ALTI 5m
#'   \item BD ALTI® V2 25M
#' }
#' La fonction demande le département consernée.
#' @usage LOADfromIGN(rep)
#' @param rep CHARACTER. Répertoire du dossier de téléchargement. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @details
#' La fonction télécharge les données depuis le serveur \url{http://files.opendatarchives.fr/professionnels.ign.fr/}
#' @author 
#' Paul CARTERON <\email{carteronpaul@gmail.com}>
#' Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     LOADfromIGN(rep = F)
#' @export
#' 
#' @import tcltk xml2 archive


# Lancement des library
# library(tcltk)
# library(xml2)
# library(archive)

LOADfromIGN <- function(rep=F){
  options(timeout=300)
  options(warn=-1) # Déctivation des warnings
  
  message('- - - Téléchargement de données IGN - - -')
  
  if(isFALSE(rep)) {
    rep <- tk_choose.dir(default = getwd(),caption = "Choisir le répertoire où télécharger les données")
    if (is.na(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  }

  #Sélection de la données
  form <- c("BD Parcellaire®",
            "BD TOPO®",
            "RGE ALTI 5m",
            "Courbes de niveau")
  
  Res <- select.list(form,
                     multiple = F,
                     title = "Que voulez-vous télécharger ?",
                     graphics = T)
  if (is.na(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  # Filtre sur la donnée
  if ("BD Parcellaire®" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/BDPARCELLAIRE_1-2_VECTEUR/"
    BD <- "BDPARCELLAIRE_1-2_VECTEUR"}
  
  if ("BD TOPO®" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/bdtopo/latest/"
    BD <- "BDTOPO_3-3_TOUSTHEMES_SHP_LAMB93_"}
  
  if ("RGE ALTI 5m" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/rgealti/"
    BD <- "RGEALTI_2-0_5M"}
  
  if ("Courbes de niveau" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/bdalti/"
    BD <- "COURBE_1-0"}
  
  # Lecture url
  page <- read_html(url)
  links <- xml_find_all(page, ".//a")
  urls <- xml_attr(links, "href")
  urls <- urls[!is.na(urls) & urls != ""]
  urlss <- urls[grepl(BD, urls)]
  
  # Sélection du département
  if (Sys.info()["sysname"]=="Windows"){
    NAME <- winDialogString("Entrer le n° de département: ", "")
  }else {
    NAME <- readline(prompt="Entrer le n° de département: ")
  }
  if(is.na(NAME)||NAME==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
  Dep <- as.integer(NAME)
  if(is.na(Dep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  Dep <- paste0("D", sprintf("%03d", as.numeric(Dep)))
  
  #Filtre sur la donnée
  urlsss <- urlss[grepl(Dep, urlss)]
  form <- unlist(urlsss)
  
  Res3 <- select.list(form,
                      multiple = F,
                      title = "Que voulez-vous télécharger ?",
                      graphics = T)
  if (is.na(Res3)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  file <- Res3
  lien <- paste0(url, Res3)
  
  #Téléchargement de la donnée
  destfile <- paste(rep, file, sep="/")
  download.file(lien, destfile, mode= "wb", quiet=F)
  cat("        Fin de téléchargement \n")
  
  #Extraction de la donnée de la donnée
  zip <- archive(destfile)
  archive_extract(destfile, rep)
  cat(paste0("        L'archive ", file, " a été extraite dans ", rep, "\n"))
  
  options(warn=1) # Activation des warnings
  }
  