library(stringr)
library(dplyr)
library(tcltk)

LOADfromIGN <- function(rep=F){
  if(isFALSE(rep)) {
    rep <- tk_choose.dir(default = getwd(),caption = "Choisir le répertoire où télécharger les données")
    if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  }

  url <- "https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html"
  html <- paste(readLines(url), collapse="\n")

  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- as.data.frame(matched[[1]][, 2])
  colnames(links)<-"urls"

  #Sélection de la données
  form <- c("BD Parcellaire®",
            "BD TOPO®",
            "BD FORET® V2",
            "RGE ALTI 5m")

  Res <- select.list(form,
                     multiple = F,
                     title = "Que voulez-vous télécharger ?",
                     graphics = T)
  if (!length(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  #Filtre sur la donnée
  if ("BD Parcellaire®" %in% Res) {BD <- "BDPARCELLAIRE_1-2_VECTEUR"}
  if ("BD TOPO®" %in% Res) {BD <- "BDTOPO_3-0"}
  if ("BD FORET® V2" %in% Res) {BD <- "BDFORET_V2"}
  if ("RGE ALTI 5m" %in% Res) {BD <- "RGEALTI_2-0_5M"}
  urls <- links%>%
    filter(grepl(BD,urls))

  #Sélection du département
  NAME <- winDialogString("Entrer le n° de département: ", "")
  if(!length(NAME)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  Dep <- as.integer(NAME)
  if(is.na(Dep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  Dep <- str_pad(str_pad(Dep, 3, "left", '0'), 4, "left", 'D')

  #Filtre sur la donnée
  url <- urls%>%
    filter(grepl(Dep,urls))

  #Téléchargement de la donnée
  lien <- as.character(url[1,1])
  nom <- str_sub(lien, str_locate_all(lien,'/')[[1]][nrow(str_locate_all(lien,'/')[[1]]),1]+1, -1)
  destfile <- paste(rep, nom, sep="/")
  download.file(lien, destfile, mode = "wb", quiet=F)
  message("\n        Fin de téléchargement")
}




