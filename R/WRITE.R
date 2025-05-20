#' @title WRITE
#' Export d'un .shp EPSG: 2154 ~ UTF-8
#' @encoding UTF-8
#' @description 
#' La fonction \code{WRITE} exporte un sf vers un répertoire de choix avec le nom choisit par l'utilisateur avec toutes les configurations voulues.
#' @usage WRITE(sf, repout, nom)
#' @param sf Couche sf à exporter
#' @param repout Répertoire d'export
#' @param nom Nom de la couche
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'    WRITE(PARCA, repout2, paste(NAME,"PARCA.shp",sep="_"))
#' @export
#' 
#' @import sf stringr utils

# Lancement des library
# if (!require("sf")) {install.packages("sf")}
# if (!require("stringr")) {install.packages("stringr")}

WRITE <- function(sf, repout, nom){
  # Export du shapefile
  if (packageVersion("sf")<'0.9.0'){
    st_write(sf, dsn=repout, layer =nom, update=TRUE, delete_layer = TRUE,
             driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
  } else {
    st_write(sf, dsn=repout, layer =nom, append=FALSE, delete_layer = TRUE,
             driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
  }

  # Export du .prj
  options(useFancyQuotes = FALSE)
  PRJ <- file(paste0(paste(repout,str_replace(nom,".shp",""),sep="/"), ".prj"))
  writeLines(
    paste0('PROJCS[', dQuote('RGF_1993_Lambert_93'), ',GEOGCS[', dQuote('GCS_RGF_1993'),
           ',DATUM[', dQuote('D_RGF_1993'), ',SPHEROID[', dQuote('GRS_1980'),
           ',6378137.0,298.257222101]],PRIMEM[', dQuote('Greenwich'), ',0.0],UNIT[',
           dQuote('Degree'), ',0.0174532925199433]],PROJECTION[',
           dQuote('Lambert_Conformal_Conic'), '],PARAMETER[', dQuote('False_Easting'),
           ',700000.0],PARAMETER[', dQuote('False_Northing'), ',6600000.0],PARAMETER[',
           dQuote('Central_Meridian'), ',3.0],PARAMETER[', dQuote('Standard_Parallel_1'),',49.0],PARAMETER[',
           dQuote('Standard_Parallel_2'), ',44.0],PARAMETER[', dQuote('Latitude_Of_Origin'), ',46.5],UNIT[',
           dQuote('Meter'), ',1.0]]')
    , PRJ)
  close(PRJ)


  cat(paste("        Le fichier",nom,"a été exporté dans",repout),"\n")

}
