if (!require("cadastreAnalysis")) {devtools::install_github("paul-carteron/cadastreAnalysis")}
library(cadastreAnalysis)
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}

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
  MH <- st_read(str_replace(TF, ".gz", ""), quiet=T) # Lecture du fichier sf
  cat("        Données MH chargées avec succès  \n")
  
  # Récupération des données MH sur l'emprise
  MHs <- st_intersection(MH%>% st_transform(2154), EMPRISE)
  
  if (nrow(MHs)>0){
    cadastreAnalysis::plotMH(MHShp = MH,
                             zoneEtude = SHP,
                             mapBackground = "OpenStreetMap",
                             bufferMH = 2500)
    
    msgBox <- tkmessageBox(title = "Monument historique",
                           message = paste0("Un MH a été détecté à proximité.\n",
                           "Nous allons ouvrir l'atlas pour vérifier !"),
                           icon = "info", type = "ok")
    
    browseURL("http://atlas.patrimoines.culture.fr/atlas/trunk/")
  }
  options(warn=1) # Activation des warnings
}





