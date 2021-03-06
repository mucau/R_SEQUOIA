#' @title INPNonSHP
#' Detection des enjeux environnementaux sur une zone d'etude
#' @encoding UTF-8
#' @description 
#' La fonction \code{INPNonSHP} recherche les intersections entre une zone d' étude (.shp quelconque) et les zonnages environnementaux référencées sur le site de l'INPN.
#' Pour chaque enjeux, une couche .shp contenant les zones intersectant la zone d'étude est exportée.
#' @usage INPNonSHP(shp, repRdata)
#' @param shp Adresse du \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param repRdata Répertoire du fichier .Rdata contenant les données INPN. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @param NAME Préfixe des fichers shapefile générés.
#' @details 
#' La fonction réclame une archive .Rdata externe produite par la fonction \code{\link{INPNtoRDATA}}.
#' @return
#' La fontion retourne un ensemble de .shp dans le dossier source.
#' @references Vous pouvez retrouvé les données environnementales sur le site de l'Institut national pour la protection de la nature (INPN) : \url{https://inpn.mnhn.fr/accueil/index}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INPNonSHP(shp=F, repRdata=F, NAME=NULL)
#' @export
#' 
#' @import tcltk sf dplyr stringr utils

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}

INPNonSHP <- function(shp = F, repRdata = F, NAME=NULL){
  message("- - - Zonnage environnementaux - - -")
  if(isFALSE(shp)){
    shp  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if(isFALSE(repRdata)){
    repRdata <- tcltk::tk_choose.dir(caption = "Choisir le dossier contenant les données INPN.Rdata")
  }

  if (!length(shp) | !length(repRdata)){
    stop("Pas de fichiers sélectionné >> Traitement annulé")} 
  
  # Import des données .shp
  message('        Lecture des données')
  SHP <- st_read(shp, options = "ENCODING=UTF-8", quiet=T, stringsAsFactors = F)  # Lecture du shapefile
  SHP <- st_sf(st_combine(SHP))
  cat("        Le fichier .shp a été chargé avec succès \n")
  if (grepl("PARCA", shp )){
    NAME <- str_sub(shp,
                    str_locate_all(shp,'/')[[1]][nrow(str_locate_all(shp,'/')[[1]]),1]+1,
                    str_locate(shp,'_PARCA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())
  }
  if (grepl("UA", shp )){
    NAME <- str_sub(shp,
                    str_locate_all(shp,'/')[[1]][nrow(str_locate_all(shp,'/')[[1]]),1]+1,
                    str_locate(shp,'_UA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())
  }
  if(is.null(NAME)){
    if (Sys.info()["sysname"]=="Windows"){
      NAME <- winDialogString("Entrer le nom du fichier de sortie:", "")
    }else {
      NAME <- readline(prompt="Entrer le nom du fichier de sortie:")
    }
  }
  if(!length(NAME)) {NAME <- ""} else {NAME <- paste0(NAME,"_")}
  
  # Import des données .Rdata
  load(paste(repRdata,"INPN.Rdata",sep="/"))
  cat("        L'archive .Rdata a été chargé avec succès \n \n")
  
  # Intersection
  message('        Détection des intersections')
  for (a in 1:length(list_INPN)) {
    INPN_POLY <- st_sf(list_INPN[[a]], stringsAsFactors = F)
    SHP <- st_transform(SHP,st_crs(list_INPN[[a]]))
    SHP["INPN"] <- "INPN"
    
    INPN_POLY   <- st_join(list_INPN[[a]],SHP["INPN"])
    
    INPN_POLY   <- INPN_POLY %>% filter(!is.na(INPN))
    
    NOM <- list_INPN_NOM[[a]]
    
    if (nrow(INPN_POLY)>=1) {
      SEQUOIA:::WRITE(INPN_POLY, dirname(shp), str_replace(paste0(NAME, NOM),".shp",""))
    } else {
      cat(paste("        Aucune correspondance entre le .shp et ", NOM), "\n")
    }
  }
}


