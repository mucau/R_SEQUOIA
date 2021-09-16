if (!require("cadastreAnalysis")) {devtools::install_github("paul-carteron/cadastreAnalysis")}
library(cadastreAnalysis)
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("leaflet")) {install.packages("leaflet")}
library(leaflet)

AAConSHP <- function(rep=F){
  message("- - - Détection des aires d'alimentation de captages - - -")
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
  
  # Importation des aires d'alimentation de captages
  message('        Téléchargement des AAC')
  cat("        Téléchargement en cours \n")
  adress <- "https://transcode.geo.data.gouv.fr/services/5e2a1e6dfa4268bc255309c4/feature-types/sa:AAC?format=SHP&projection=WGS84"
  temp <- tempfile()
  curl::curl_download(adress, temp, quiet = T)
  temp2 <- tempfile()
  unzip(zipfile = temp, exdir = temp2)
  zoneCaptageData  <- st_read(temp2, quiet = T) %>% st_transform(2154)
  cat("        Téléchargement terminé \n \n")

  
  # Récupération des données MH sur l'emprise
  message('        Détection des AAC')
  AACs <- st_intersection(zoneCaptageData %>% st_transform(2154), EMPRISE)
  AACs <- st_as_sf(AACs)
  zoneEtude <- st_union(SHP)
  
  if (nrow(AACs)>0){
    res = leaflet() %>%
      addProviderTiles("OpenStreetMap.France") %>%
      addPolylines(
        data = zoneEtude%>% st_transform(4326),
        opacity = 1,
        stroke = TRUE,
        weight = 1,
        color = "black"
      ) %>%
      addPolygons(data = AACs%>% st_transform(4326),
                 label = AACs$NomDeAAC_A)
    
    print(res)
    cat("        Une AAC a été détecté à proximité")
  } else {
    cat("        Pas d'AAC détectées")
  }
  options(warn=1) # Activation des warnings
}
