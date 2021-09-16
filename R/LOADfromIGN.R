library(stringr)
library(dplyr)
library(tcltk)

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
    url <- "http://files.opendatarchives.fr/professionnels.ign.fr/bdtopo/latest/"
    BD <- "BDTOPO_3-0"}
  
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
  urls <- urlss%>%
    filter(grepl(Dep,urls))
  
  form <- unlist(urls)
  
  Res <- select.list(form,
                     multiple = F,
                     title = "Que voulez-vous télécharger ?",
                     graphics = T)
  if (!length(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  #Téléchargement de la donnée
  #nom <- str_sub(Res, str_locate_all(Res,'/')[[1]][nrow(str_locate_all(Res,'/')[[1]]),1]+1, -1)
  destfile <- paste(rep, Res, sep="/")
  lien <- str_replace(paste(url, Res), " ", "")
  download.file(lien, destfile, mode = "wb", quiet=F)
  message("\n        Fin de téléchargement")
}