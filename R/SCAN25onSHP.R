#' @title SCAN25onSHP
#' Recuperation de l'IGN (c) Scan 25 (r) sur une emprise shapefile
#' @encoding UTF-8
#' @description
#' La fonction \code{SCAN25onSHP} télécharge IGN© Scan 25® à partir d'une emprise shapefile
#' @usage SCAN25onSHP(rep)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' \item{Scan25.tif}{Fichier raster .tiff ;  Scan 25 enregistré dans le répertoire du fichier}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'   SCAN25onSHP(rep=F)
#' @export
#'
#' @import tcltk sf happign terra

# Lancement des library
# library(tcltk)
# library(sf)
# library(happign)
# library(terra)

SCAN25onSHP <- function(rep=F){
  options(warn=-1) # Déctivation des warnings
  message('- - - SCAN 25 sur shapefile - - -')

  message("\n        Lecture du shapefile")
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  if(isFALSE(SEQ)){repout <- repout}else{repout <- repout1}

  SHP <- st_read(rep, options = "ENCODING=UTF-8", agr="constant", quiet=T)
  cat("        Le fichier .shp a été chargé avec succès \n")

  out_name <- paste(repout, paste0(NAME,"_Scan25.tif"), sep="/")

  get_scan <- function(shp, out_name, type = "scan25", buff = 1000, res = 0.8, crs = 2154){

    layer <- switch(type,
                    scan25 = "SCAN25TOUR_PYR-JPEG_WLD_WM",
                    scan100 = "SCAN100_PYR-JPEG_WLD_WM",
                    oaci = "SCANOACI_PYR-JPEG_WLD_WM",
                    carte_ign = "GEOGRAPHICALGRIDSYSTEMS.MAPS",
                    stop("layer should be one of 'scan25', 'scan100', 'oaci' or 'carte_ign'.",
                         call. = FALSE))

    wms_url <- sprintf("WMS:https://data.geopf.fr/private/wms-r?VERSION=1.3.0&LAYERS=%s&CRS=EPSG:4326&FORMAT=image/geotiff&apikey=ign_scan_ws",
                       layer)

    shp <- shp |>
      st_union() |>
      st_transform(crs) |>
      st_buffer(buff)

    bb <- st_bbox(shp)

    warp_options <- c(
      "-of", "GTIFF",
      "-te", bb$xmin, bb$ymin, bb$xmax, bb$ymax,
      "-te_srs", st_crs(crs)$srid,
      "-t_srs", st_crs(crs)$srid,
      "-tr", res, res,
      "-r", "bilinear",
      "-overwrite"
    )

    rast <- gdal_utils(
      util = "warp",
      source = wms_url,
      destination = out_name,
      quiet = FALSE,
      options = c(warp_options)
    ) |> suppressWarnings()

    scan <- terra::rast(out_name)

    return(scan)
  }
  scan <- get_scan(SHP, out_name)

  cat("        Le SCAN 25 a été exporté dans", repout, "\n")
}
