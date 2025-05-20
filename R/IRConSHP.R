#' @title IRConSHP
#' Recuperation de l'IGN (c) BD Ortho IRC (r) sur une emprise shapefile
#' @encoding UTF-8
#' @description
#' La fonction \code{IRConSHP} télécharge IGN© BD Ortho IRC® à partir d'une emprise shapefile
#' @usage IRConSHP(rep)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' \item{IRC.tif}{Fichier raster .tiff ;  IRC enregistré dans le répertoire du fichier}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'   IRConSHP(rep=F)
#' @export
#'
#' @import tcltk sf happign

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)

IRConSHP <- function(rep=F){
  options(warn=-1) # Déctivation des warnings
  message('- - - IRC sur shapefile - - -')

  message("\n        Lecture du shapefile")
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout1}
  shp <- st_read(rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès \n")

  out_name <- paste(repout, paste0(NAME,"_IRC.tif"), sep="/")

  get_irc <- function(shp, out_name, buff = 1000, res = 0.8, crs = 2154){

    shp <- shp |>
      st_union() |>
      st_transform(crs) |>
      st_buffer(buff)

    irc <- get_wms_raster(shp, layer = "ORTHOIMAGERY.ORTHOPHOTOS.IRC", crs = crs,
                           res = res, verbose = FALSE, overwrite = TRUE, filename = out_name)

    return(irc)

  }
  irc <- get_irc(shp, out_name)

  cat("        L' IRC a été exporté dans", repout, "\n")
}
