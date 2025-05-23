#' @title MHonSHP
#' Detection des monuments historiques
#' @encoding UTF-8
#' @description 
#' La fonction \code{MHonSHP} détecte la présence d'un monument historique dans l'environnement immédiat d'un shapefile.
#' @usage MHonSHP(rep)
#' @param rep CHARACTER. Adresse du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @details
#' La fonction réclame le package \code{cadastreAnalysis}.
#' @return
#' La fonction renvoit un affichage des éléments.
#' La fonction retourne un dataframe des données METEOFRANCE. Ces renseignements sont fournies au mois et cumulés/moyennés à l'année.
#' @author 
#' Paul CARTERON <\email{carteronpaul@gmail.com}>
#' Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   MHonSHP(rep=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(sf)
# library(tcltk)

MHonSHP <- function(rep=F){
  message('- - - Détection MH - - -')
  options(warn=-1) # Désactivation des warnings
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  message('        Lecture des données')
  SHP <- st_read(rep, options = "ENCODING=UTF-8",
                agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  cat("        Le fichier .shp a été chargé avec succès  \n \n")
  assign("SHP", SHP, envir=globalenv())
  
  # Création d'une emprise
  EMPRISE <- st_union(st_buffer(SHP, 2500))
  
  # Téléchargement des données
  message('        Téléchargement des données MH')
  URL = "http://files.opendatarchives.fr/data.culture.gouv.fr/liste-des-immeubles-proteges-au-titre-des-monuments-historiques.geojson.gz"
  
  TD = tempdir() # répertoire temporaire
  TF = tempfile(tmpdir=TD, fileext=".gz") # fichier temporaire
  download.file(URL, TF, method="libcurl", quiet=T) # Téléchargement du fichier URL sous le nom TF dans TD
  cat('        Extraction des données MH  \n')
  R.utils::gunzip(TF, remove=F) # Extraction de TF dans TD
  cat('        Lecture des données MH  \n')
  MH <- st_read(gsub(".gz", "", TF), quiet=T) # Lecture du fichier sf
  cat("        Données MH chargées avec succès  \n")
  
  # Récupération des données MH sur l'emprise
  MHs <- st_intersection(st_transform(MH, 2154) , EMPRISE)
  
  if (nrow(MHs)>0){
    msgBox <- tkmessageBox(title = "Monument historique",
                           message = paste0(nrow(MHs), " MH détecté(s) à proximité.\n",
                           "Nous allons ouvrir l'atlas pour vérifier !"),
                           icon = "info", type = "ok")
    
    browseURL("http://atlas.patrimoines.culture.fr/atlas/trunk/")
  }
  options(warn=1) # Activation des warnings
}





