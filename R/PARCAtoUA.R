#' @title PARCAtoUA
#' Creation d'un ensemble de .shp a partir d'un parcellaire cadastral .shp
#' @encoding UTF-8
#' @description 
#' La fonction \code{PARCAtoUA} génère un ensemble de .shp (EPSG 2154) nécessaires à la réalisation d'une cartographie forestière ponctuelle à partir d'un parcellaire cadastral .shp formatée (EPSG 2154).
#' @usage PARCAtoUA(rep)
#' @param rep CHARACTER. Adresse du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' Les fichiers .shp suivants sont produits :
#' @return
#' \item{UA_polygon}{Fichier shapefile ; Unités d'analyse}
#' \item{PROP_polygon}{Fichier shapefile ; Propriété unifiée}
#' \item{PROP_line}{Fichier shapefile ; Contours de la propriété unifiée}
#' \item{PROP_point}{Fichier shapefile ; Centroide de la propriété unifiée}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   PARCAtoUA(rep=F)
#' @export
#' 
#' @import tcltk dplyr stringr sf
#' @importFrom stats aggregate

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("dplyr")) {install.packages("dplyr")}

PARCAtoUA <- function(rep=F) {
  message('- - - Création de UA_polygon à partir de PARCA_polygon - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de PARCA
  message('        Lecture des données')
  PARCA <- st_read(rep, options = "ENCODING=UTF-8",
                   agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n \n")
  assign("PARCA", PARCA, envir=globalenv())

  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_PARCA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())
  
  # Correction de PARCA
  PARCA_cor <- PARCA[!st_is_empty(PARCA),,drop=FALSE]
  if (nrow(PARCA_cor)<nrow(PARCA)){
    cat("        Des polygones vides ont été détectés dans PARCA  \n")
    SEQUOIA:::WRITE(PARCA_cor, repout2, paste(NAME,"PARCA_polygon.shp",sep="_"))
    PARCA <- PARCA_cor
  }

  # Création du .shp "UA_polygon"
  message('        Création de UA_polygon')
  UA_polygon <- PARCA %>%
    mutate(PARCA      = paste0(SECTION, ' ', N_PARCA),
           PARFOR     = as.character(NA),
           N_PARFOR   = as.character(NA),
           N_SSPARFOR = as.character(NA),
           GEOL_TYPE  = as.character(NA),
           PLT_TYPE   = as.character(NA),
           PLT_ESS    = as.character(NA),
           PLT_STR    = as.character(NA),
           PLT_TSE    = as.character(NA),
           AME_TYPE   = as.character(NA),
           SURF_SIG   = as.double(NA),
           SURF_COR   = as.double(NA)) %>%
    dplyr::select(REG_CODE:ADRESSE,IDU,PREFIXE:N_PARCA, PARCA, LIEUDIT,PARFOR,N_PARFOR,N_SSPARFOR,GEOL_TYPE, OCCUP_SOL,PLT_TYPE, PLT_ESS, PLT_STR, PLT_TSE, AME_TYPE,SURF_CA,SURF_SIG,SURF_COR,geometry)


  UA_polygon <- unique(UA_polygon) # Simplification du shapefile

  SEQUOIA:::WRITE(UA_polygon, repout2, paste(NAME,"UA_polygon.shp",sep="_"))

  # Création du .shp "PROP_polygon"
  message('\n        Création de PROP_polygon')
  PROP_polygon <- aggregate(x = PARCA[, "SURF_CA"], by = list(PARCA$PROP),
                            FUN = sum, na.rm = TRUE)
  names(PROP_polygon) <- c("PROP","SURF_CA","geometry")

  SEQUOIA:::WRITE(PROP_polygon, repout2, paste(NAME,"PROP_polygon.shp",sep="_"))

  # Création du .shp "PROP_line"
  message('\n        Création de PROP_line')
  options(warn=-1)
  PROP_point <- st_combine(st_cast(PROP_polygon,'MULTIPOINT')) # Récupère les noeuds de polygones

  PROP_line <- st_cast(PROP_polygon[,-2],'MULTILINESTRING') # Récupère les contours de polygones
  PROP_line <- st_split(PROP_line, PROP_point) # Segmentation : Decoupe les contours de polygones par les noeuds
  PROP_line <- st_collection_extract(PROP_line,"LINESTRING") # Sélectionne les objets de classe LINESTRING
  PROP_line$LENGTH <- st_length(PROP_line) # Ajouter un champ longueur
  PROP_line <- PROP_line[!duplicated(PROP_line$LENGTH),] # Supprime les doublons sur le champs longueur
  st_crs(PROP_line) <-2154

  SEQUOIA:::WRITE(PROP_line, repout2, paste(NAME,"PROP_line.shp",sep="_"))

  # Création du .shp "PROP_point"
  message('\n        Création de PROP_point')
  PROP_point <- st_centroid(PROP_polygon, of_largest_polygon=F) # Création du centroid

  SEQUOIA:::WRITE(PROP_point, repout2, paste(NAME,"PROP_point.shp",sep="_"))
  options(warn=1)
}
