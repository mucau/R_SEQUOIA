#' @title INPNbyRdata
#' Extraction des zonages environnementaux a partir d'une .Rdata
#' @encoding UTF-8
#' @description 
#' La fonction \code{INPNbyRdata} charge et exporte les zonages environnementaux intersectés sur une zone d'étude
#' Pour chaque enjeux, une couche .shp contenant les zones intersectant la zone d'étude est exportée.
#' @usage INPNonSHP(shp, rdata)
#' @param shp Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @param rdata Répertoire du fichier .Rdata contenant les données INPN. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @details 
#' La fonction réclame une archive .Rdata externe produite par la fonction \code{\link{INPNtoRDATA}}.
#' @return
#' La fontion retourne un ensemble de .shp dans le dossier source.
#' @references Vous pouvez retrouvé les données environnementales sur le site de l'Institut national pour la protection de la nature (INPN) : \url{https://inpn.mnhn.fr/accueil/index}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INPNbyRdata(shp=F, rdata=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

INPNbyRdata <- function(shp = F, rdata = F){
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
  
  # Import des données .Rdata
  message('\n        Chargement de la .Rdata')
  if(isFALSE(rdata)){
    rdata <- tk_choose.dir(caption = "Choisir le dossier contenant les données INPN.Rdata")
  }
  if (is.na(rdata)){stop("Pas de dossier sélectionné > Traitement annulé \n")} 
  load(paste(rdata,"INPN.Rdata",sep="/"))
  cat("        L'archive .Rdata a été chargé avec succès \n \n")
  
  # Intersection
  message('        Détection des intersections')
  for (a in 1:length(list_INPN)) {
    data <- st_sf(list_INPN[[a]], stringsAsFactors = F)
    TEMPON <- st_transform(TEMPON, st_crs(data))
    
    intersects  <- st_intersects(data, TEMPON, sparse = F)
    sf <- data[apply(intersects, 1, any), ]
    
    name_shp <- substr(list_INPN_NOM[[a]], 6, nchar(list_INPN_NOM[[a]]))
    
    if (nrow(sf)>=1) {
      write(sf, repout, name_shp)
    } else {
      cat(paste("        Aucune correspondance entre le .shp et ", name_shp), "\n")
    }
  }
}


