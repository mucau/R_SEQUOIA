#' @title MNHonSHP
#' Recuperation du modèle numérique de hauteur de l'IGN©
#' @encoding UTF-8
#' @description
#' La fonction \code{MNHonSHP} télécharge le MNT et MNS BD ALti® IGN© à partir d'une emprise shapefile
#' @usage MNHonSHP(rep)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' \item{MNH.tif}{Fichier raster .tiff ;  MNH enregistré dans le répertoire du fichier}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'   MNHonSHP(rep=F)
#' @export
#'
#' @import tcltk sf happign terra

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)
# library(terra)

MNHonSHP <- function(rep=F){
  options(warn=-1) # Déctivation des warnings
  message('- - - MNH sur shapefile - - -')

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

  out_name <- paste(repout, paste0(NAME,"_MNH.tif"), sep="/")

  get_mnh <- function(shp, out_name, buff = 1000, res = 1, crs = 2154){

    shp <- shp |>
      st_union() |>
      st_transform(crs) |>
      st_buffer(buff)

    mnt <- get_wms_raster(shp, "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
                          res = res, crs = crs, rgb = FALSE, verbose = FALSE,
                          overwrite = TRUE)
    mns <- get_wms_raster(shp, "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS",
                          res = res, crs = crs, rgb = FALSE, verbose = FALSE,
                          overwrite = TRUE)

    mnt[mnt < 0] <- 0
    mns[mns < 0] <- 0

    mnh <- mns - mnt

    mnh[mnh < 0] <- NA  # Remove negative value
    mnh[mnh > 50] <- 50 # Remove height more than 50m

    writeRaster(mnh, out_name, overwrite = TRUE)
    return(mnh)
  }
  mnh <- get_mnh(SHP, out_name)

  cat("        Le MNH a été exporté dans", repout, "\n")
}
