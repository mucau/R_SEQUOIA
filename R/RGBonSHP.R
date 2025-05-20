#' @title RGBonSHP
#' Recuperation de l'IGN (c) BD Ortho RGB(r) sur une emprise shapefile
#' @encoding UTF-8
#' @description
#' La fonction \code{RGBonSHP} télécharge IGN© BD Ortho RGB® à partir d'une emprise shapefile
#' @usage RGBonSHP(rep)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' \item{RGB.tif}{Fichier raster .tiff ;  RGB enregistré dans le répertoire du fichier}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'   RGBonSHP(rep=F)
#' @export
#'
#' @import tcltk sf happign

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)

RGBonSHP <- function(rep=F){
  options(warn=-1) # Déctivation des warnings
  message('- - - RGB sur shapefile - - -')

  message("\n        Lecture du shapefile")
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout1}
  SHP <- st_read(rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
  cat("        Le fichier .shp a été chargé avec succès \n")

  out_name <- paste(repout, paste0(NAME,"_RGB.tif"), sep="/")

  get_rgb <- function(shp, out_name, buff = 1000, res = 0.8, crs = 2154){

    shp <- shp |>
      st_union() |>
      st_transform(crs) |>
      st_buffer(buff)

    rgb <- get_wms_raster(shp, layer = "ORTHOIMAGERY.ORTHOPHOTOS.BDORTHO", crs = crs,
                           res = res, verbose = FALSE, overwrite = TRUE, filename = out_name)

    return(rgb)
  }
  rgb <- get_rgb(SHP, out_name)

  cat("        Le RGB a été exporté dans", repout, "\n")
}
