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
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

PARCAtoUA <- function(rep=F) {
  message('- - - Création de UA_polygon à partir de PARCA_polygon - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("SEQUOIA ESRI Shapefile", "_PARCA_polygon.shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de PARCA
  message('        Lecture des données')
  NAMEofSHP(rep)
  if (isFALSE(SEQ)){stop("Répertoire SEQUOIA non detecté")}
  
  PARCA <- st_read(rep, options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  assign("PARCA", PARCA, envir=globalenv())
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n \n")
  
  # write function
  write <- function(nom, rep, name){
    st_write(nom, rep, name, append=FALSE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }
  
  # Correction de PARCA
  PARCA_cor <- PARCA[!st_is_empty(PARCA),,drop=FALSE]
  if (nrow(PARCA_cor)<nrow(PARCA)){
    cat("        Des polygones vides ont été détectés dans PARCA  \n")
    write(PARCA_cor, 
          repout2, 
          paste(NAME,"PARCA_polygon.shp",sep="_"))
    PARCA <- PARCA_cor
  }

  # Création du .shp "UA_polygon"
  message('        Création de UA_polygon')
  UA_polygon <- PARCA
  UA_polygon$PARCA <- as.character(paste0(UA_polygon$SECTION, ' ', UA_polygon$N_PARCA))
  UA_polygon$PARFOR <- as.character(NA)
  UA_polygon$N_PARFOR <- as.character(NA)
  UA_polygon$N_SSPARFOR <- as.character(NA)
  UA_polygon$SOL_TYPE <- as.character(NA)
  UA_polygon$PLT_TYPE <- as.character(NA)
  UA_polygon$PLT_ESS <- as.character(NA)
  UA_polygon$PLT_STR <- as.character(NA)
  UA_polygon$PLT_TSE <- as.character(NA)
  UA_polygon$AME_TYPE <- as.character(NA)
  UA_polygon$DISP_TYPE <- as.character(NA)
  UA_polygon$COMMENTS  = as.character(NA)
  UA_polygon$SURF_SIG <- NA_real_
  UA_polygon$SURF_COR <- as.numeric(UA_polygon$SURF_CA)
  new_order <-c("REG_CODE", "REG_NOM", "DEP_CODE", "DEP_NOM", "COM_CODE", "COM_NOM", "PROP", "ADRESSE", "IDU", "PREFIXE", "SECTION", "N_PARCA", "PARCA", "LIEUDIT", 
                "PARFOR", "N_PARFOR","N_SSPARFOR", "SOL_TYPE", "OCCUP_SOL", "PLT_TYPE", "PLT_ESS", "PLT_STR", "PLT_TSE", "AME_TYPE", "DISP_TYPE", "COMMENTS", "SURF_CA", "SURF_SIG", "SURF_COR")
  UA_polygon <- unique(UA_polygon[, new_order])
  write(UA_polygon, 
        repout2, 
        paste(NAME,"UA_polygon.shp",sep="_"))

  # Création du .shp "PROP_polygon"
  message('\n        Création de PROP_polygon')
  PROP_polygon <- aggregate(
    x = PARCA["SURF_CA"],
    by = list(PROP = PARCA$PROP),
    FUN = sum,
    do_union = TRUE 
  )
  write(PROP_polygon, 
        repout2, 
        paste(NAME,"PROP_polygon.shp",sep="_"))

  # Création du .shp "PROP_line"
  message('\n        Création de PROP_line')
  PROP_line <- LINEfromPOLYGON(PROP_polygon)
  write(PROP_line, 
        repout2, 
        paste(NAME,"PROP_line.shp",sep="_"))

  # Création du .shp "PROP_point"
  message('\n        Création de PROP_point')
  PROP_point <- st_centroid(PROP_polygon, of_largest_polygon=F)
  write(PROP_point, 
        repout2, 
        paste(NAME,"PROP_point.shp",sep="_"))

}
