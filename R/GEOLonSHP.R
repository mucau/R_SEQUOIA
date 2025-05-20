#' @title GEOLonSHP
#' Import de la BRGM (C) BD_Charm50 (r) sur une emprise shapefile
#' @encoding UTF-8
#' @description 
#' La fonction \code{GEOLonSHP} téléchargement la BRGM (C) BD_Charm50 (r) départementale détecté sur l'emprise du shapefile d'entrée, réalise une intersection des couches et calcul la surface SIG par entité géologique.
#' @usage GEOLonSHP(shp, NAME)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' \item{geol_polygon}{Shapefile de la géologie sur l'emprise. Généré dans le répertoire du fichier source.}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   GEOLonSHP(rep=F)
#' @export
#' 
#' @import tcltk sf archive

# Lancement des library
# library(tcltk)
# library(sf)
# library(archive)

GEOLonSHP <- function(rep=F){
  message('- - - Géologie sur emprise shapefile - - -')
  
  # Lecture du shapefile
  message('        Chargement du .shp')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout2}
  shp <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  
  # write function
  write <- function(nom, rep, name=NULL){
    if (is.null(name)){
      name <- paste0(NAME, "_", deparse(substitute(nom)), ".shp")
    } else {
      name <-  paste0(NAME, "_", name, ".shp")
    }
    st_write(nom, rep, name, append=FALSE, delete_layer=TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }

  # Récupération des départements
  message('\n        Récupération des départements')
  suppressMessages(dep <- DEPonSHP(rep))
  cat("        ", nrow(dep), "département(s) détecté(s)  \n \n")

  # Téléchargement des données géologiques
  message('        Téléchargement des données géologiques')
  for (a in 1:nrow(dep)){
    dep_code <- as.character(dep[a, 4])[1]
    
    url <- paste0('http://infoterre.brgm.fr/telechargements/BDCharm50/GEO050K_HARM_', formatted_code <- sprintf("%03d", as.integer(dep_code)), '.zip')
    TD = tempdir() # répertoire temporaire
    TF = tempfile(tmpdir=TD, fileext=".zip") # fichier temporaire
    download.file(url, TF, method="libcurl", quiet=F)
    archive_extract(TF, TD)

  }

  # Compilation des fichiers géologique
  message('        Compilation des fichiers géologique')
  list_geol <- list.files(TD, "*S_FGEOL_2154.shp", recursive=T)

  geol <- st_sf(st_sfc())
  for (b in 1:length(list_geol)) {
    geol_dep <- st_read(paste(TD, list_geol[b], sep="/"), agr="constant", quiet=T)
    st_crs(geol) <- st_crs(geol_dep)
    geol <- rbind(geol, geol_dep)
  } # fin boucle geol
  cat("        Les BD_Charm départementales ont été compilées \n \n")

  # Intersection avec l'emprise
  message("        Intersection avec l'emprise")
  GEOL_polygon <- st_intersection(st_transform(st_sf(st_union(shp)), 2154),
                              st_transform(geol,2154))
  cat("        Intersection effectuée \n")
  GEOL_polygon$SURF_SIG <- st_area(GEOL_polygon)
  cat("        Calcul des SURF_SIG effectué \n \n")

  # Export des données
  message("        Export des données")
  write(GEOL_polygon, repout)
}
