#' @title INPNtoRDATA
#' Creation d'une archive .Rdata des enjeux environnementaux
#' @encoding UTF-8
#' @description 
#' La fonction \code{INPNtoRDATA} télécharge depuis le site de l'INPN l'ensemble des fichiers .shp des enjeux environnementaux et sauvegarde ces couches dans une archive .Rdata 
#' @usage INPNtoRDATA(rep)
#' @param rep Répertoire de sortie de l'archive .Rdata Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @details 
#' La fonction produit une archive .Rdata nécessaire à la fonction \code{\link{INPNonSHP}}.
#' @return
#' \item{list_INPN}{Liste de fichiers cartographiques sf}
#' \item{list_INPN_NOM}{Liste des noms des fichiers cartographiques sf de la liste précédente}
#' @references Vous pouvez retrouvé les données environnementales sur le site de l'Institut national pour la protection de la nature (INPN) : \url{https://inpn.mnhn.fr/accueil/index}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INPNtoRDATA(rep=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

INPNtoRDATA <- function(rep=F){
  message('- - - Téléchargement de données INPN - - -')
  if(isFALSE(rep)) {rep <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire de telechargement des archives .Rdata")}
  if(is.na(rep)){warning("Aucun fichier sélectionné >> Traitement annulé")}

# Adresses de téléchargement et de destination
  INPN_URL <- "https://inpn.mnhn.fr/docs/Shape"
  LISTE_ZONE <- c("znieff2", "znieff1", "zico", "sic", "zps", "apb", "aspim", "bpm","pn", "ripn", "pnr", "rb", "bios", "rnn", "rncfs", "rnr", "cen")
  TD = tempdir() # répertoire temporaire

# Téléchargement des données .shp depuis INPN
  for (a in LISTE_ZONE) {
    URL <- paste(INPN_URL, paste0(a,".zip"), sep="/")

    TF = tempfile(tmpdir=TD, fileext=".zip") # fichier temporaire
    download.file(URL, TF, method="libcurl") # Téléchargement du fichier URL sous le nom TF dans TD
    unzip(zipfile = TF, exdir=TD) # Extraction de TF dans TD
    cat("Le fichier", a, "a été téléchargé", "\n")
  }

# Détection des . shp et création des listes vierges
  list_dirs <- list.dirs(TD)
  cat(length(list_dirs), "dossiers ont été détectés \n")

  list_INPN     <- list()
  list_INPN_NOM <- list()
  d <- 1 # Variable d'indexation dans les listes

# Lecture des .shp dans les dossiers
  for (b in list_dirs) {
    list_shp <- list.files(b, "*.shp$")

    for (c in list_shp) {
      SHP <- st_read(paste(b, c, sep="/"), options = "ENCODING=UTF-8", quiet=T) # Lecture du fichier .shp
      SHP <- st_transform(SHP, 2154)

      SUPR <- c(".shp","N_ENP_","_S_000","1712","2013","2013_09")
      NAME <- c
      for (e in 1:length(SUPR)) {
        NAME <- gsub(SUPR[e], "", NAME)
      }
      NOM <- paste0("INPN_", toupper(NAME),"_polygon")

      list_INPN[[d]] <- SHP # Ajout du .shp à la liste
      list_INPN_NOM[[d]] <- NOM # Ajout du nom à la liste

      d=d+1 # Avancé dans l'index

      cat("Le fichier", NOM, "a été ajouté à l'archive", "\n")
    }
  }

# Enrigistrement des liste sous un .Rdata
  save(list_INPN, list_INPN_NOM, file=paste(rep,"INPN.Rdata",sep="/"))
  cat("Les", d-1, "fichiers téléchargés ont été sauvegardés dans ", rep, "\n")
  assign("list_INPN",list_INPN,envir=globalenv())
  assign("list_INPN_NOM",list_INPN_NOM,envir=globalenv())
}



