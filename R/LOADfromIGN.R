#' @title LOADfromIGN
#' Recuperation des donnees administratives et creation d'une archive R.data
#' @encoding UTF-8
#' @description 
#' La fonction \code{LOADfromIGN} permet de télécharger rapidement les données de l'IGN (C).
#' \tabular{ll}{
#'   \tab >>  BD Parcellaire® \cr
#'   \tab >>  BD TOPO® \cr
#'   \tab >>  BD FORET® V2 \cr
#'   \tab >>  RGE ALTI 5m \cr
#'   \tab >>  BD ALTI® V2 25M \cr
#' }
#' La fonction demande le département consernée.
#' @usage LOADfromIGN(rep)
#' @param rep CHARACTER. Répertoire du dossier de téléchargement. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @details 
#' La fonction télécharge les données depuis le serveur \url{http://files.opendatarchives.fr/professionnels.ign.fr/}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   LOADfromIGN(rep = F)
#' @export
#' 
#' @import tcltk dplyr stringr

# Lancement des library
# library(stringr)
# library(dplyr)
# library(tcltk)

LOADfromIGN <- function(rep=F){
  if(isFALSE(rep)) {
    rep <- tk_choose.dir(default = getwd(),caption = "Choisir le répertoire où télécharger les données")
    if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  }

  #Sélection de la données
  form <- c("BD Parcellaire®",
            "BD TOPO®",
            "BD FORET® V2",
            "RGE ALTI 5m",
            "BD ALTI® V2 25M")
  
  Res <- select.list(form,
                     multiple = F,
                     title = "Que voulez-vous télécharger ?",
                     graphics = T)
  if (!length(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  #Filtre sur la donnée
  if ("BD Parcellaire®" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/BDPARCELLAIRE_1-2_VECTEUR/"
    BD <- "BDPARCELLAIRE_1-2_VECTEUR"}
  
  if ("BD TOPO®" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/bdtopo/"
    BD <- "BDTOPO_3-0_"}
  
  if ("BD FORET® V2" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/bdforet/BDFORET_V2/"
    BD <- "BDFORET_2-0"}
  
  if ("RGE ALTI 5m" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/rgealti/"
    BD <- "RGEALTI_2-0_5M"}
  
  if ("BD ALTI® V2 25M" %in% Res) {
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/bdalti/"
    BD <- "BDALTIV2_2-0_25M"}
  
  html <- paste(readLines(url), collapse="\n")
  
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- as.data.frame(matched[[1]][, 2])
  colnames(links)<-"urls"
  
  urlss <- links%>%
    filter(grepl(BD,urls))
  
  #Sélection du département
  if (Sys.info()["sysname"]=="Windows"){
    NAME <- winDialogString("Entrer le n° de département: ", "")
  }else {
    NAME <- readline(prompt="Entrer le n° de département: ")
  }
  if(!length(NAME)||NAME==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
  Dep <- as.integer(NAME)
  if(is.na(Dep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  Dep <- str_pad(str_pad(Dep, 3, "left", '0'), 4, "left", 'D')
  
  #Filtre sur la donnée
  if ("BD TOPO®" %in% Res) {
    topo_df <- data.frame()%>% mutate(date=character(), fichier=character())
    n=1
    for (a in 1:nrow(urlss)){
      date <- urlss[a,]
      html <- paste(readLines(paste0(url, date)), collapse="\n")
      matched <- str_match_all(html, "<a href=\"(.*?)\"")
      links <- as.data.frame(matched[[1]][, 2])
      colnames(links)<-"urls"
      
      topo_urlss <- links%>%
        filter(grepl("TOUSTHEMES_SHP",urls))%>%
        filter(grepl(Dep,urls))
      
      if (nrow(topo_urlss)>0) {
        for (b in 1:nrow(topo_urlss)){
          topo_df[n,1] <- date
          topo_df[n,2] <- topo_urlss[b,]
          n<-n+1 
        }
      }
    }
    
    form <- unlist(topo_df[,2])
    
    Res <- select.list(form,
                       multiple = F,
                       title = "Que voulez-vous télécharger ?",
                       graphics = T)
    if (!length(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}
    
    choix <- topo_df %>%
      filter(fichier %in% Res)
    
    lien <- paste0(url, choix[1], choix[2])
    
  } else {
    urls <- urlss%>%
      filter(grepl(Dep,urls))
    
    form <- unlist(urls)
    
    Res <- select.list(form,
                       multiple = F,
                       title = "Que voulez-vous télécharger ?",
                       graphics = T)
    if (!length(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}
    
    lien <- paste0(url, Res)
  }
  
  #Téléchargement de la donnée
  destfile <- paste(rep, Res, sep="/")
  download.file(lien, destfile, mode = "wb", quiet=F)
  message("\n        Fin de téléchargement")
}