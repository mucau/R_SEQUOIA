#' @title INPNbyIGNweb
#' Telechargement des zonages environnementaux a partir de l'IGN(C) web service
#' @encoding UTF-8
#' @description 
#' La fonction \code{INPNbyIGNweb} charge et exporte les zonages environnementaux intersectés sur une zone d'étude
#' Pour chaque enjeux, une couche .shp contenant les zones intersectant la zone d'étude est exportée.
#' @usage INPNbyIGNweb(shp)
#' @param shp Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' La fontion retourne un ensemble de .shp dans le dossier source.
#' @references Vous pouvez retrouvé les données environnementales sur le site de l'Institut national pour la protection de la nature (INPN) : \url{https://inpn.mnhn.fr/accueil/index}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INPNbyIGNweb(shp=F, rdata=F)
#' @export
#' 
#' @import tcltk sf happign

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)

INPNbyIGNweb <- function(shp=F){
  message('- - - Zonage environnementaux sur emprise shapefile - - -')
  options(warn=-1)
  
  # Lecture du shapefile
  message('        Chargement du .shp')
  if(isFALSE(shp)) {
    shp  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(shp)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout2}
  SHP <- st_read(shp, options = "ENCODING=UTF-8", quiet=T, stringsAsFactors = F)
  TEMPON <- st_union(st_buffer(SHP, 10))
  cat("        Le fichier .shp a été chargé avec succès  \n")
  
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
  
  # Récupération des ardresses web
  layers_data <- get_layers_metadata("wfs")[]
  layers_data <- layers_data[grep("PROTECTEDAREAS", layers_data$Name, ignore.case = TRUE), ]
  
  # Intersection
  message('\n        Détection des intersections')
  for (a in 1:nrow(layers_data)) {
    data <- layers_data[a, 1]
    position_colon <- regexpr(":", data)
    name_data <- substr(data, position_colon + 1, nchar(data))
    name_shp <- paste0(toupper(name_data), "_polygon")
    
    invisible(capture.output(suppressMessages(sf <-   get_wfs(TEMPON,
                                                              layers_data[a, 1],
                                                              NULL,
                                                              "intersects"))))
    
    if(!is.null(sf)){
      sf <- st_transform(sf, 2154)
      write(sf, repout, name_shp)
    } else {
      cat(paste("        Aucune correspondance entre le .shp et ", name_data, "\n"))
    }
  }
  options(warn=1)
}


