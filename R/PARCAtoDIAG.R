#' @title PARCAtoDIAG
#' Creation d'un ensemble de .shp a partir d'un parcellaire cadastral .shp
#' @encoding UTF-8
#' @description 
#' La fonction \code{PARCAtoDIAG} génère un ensemble de .shp (EPSG 2154) nécessaires à la réalisation d'un diagnostic forestier à partir d'un parcellaire cadastral .shp SEQUOIA (EPSG 2154).
#' @usage PARCAtoDIAG(rep)
#' @param rep CHARACTER. Adresse du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' Les fichiers .shp suivants sont produits :
#' @return
#' \item{DIAG_point}{Fichier shapefile ; placettes}
#' \item{DIAG_line}{Fichier shapefile ; limites}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   PARCAtoDIAG(rep=F)
#' @export
#' 
#' @import tcltk dplyr stringr sf

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("dplyr")) {install.packages("dplyr")}

PARCAtoDIAG <- function(rep=F) {
  message('- - - Création de DIAG_point/line à partir de PARCA_polygon - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("SEQUOIA ESRI Shapefile", "_PARCA_polygon.shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  # Lecture de PARCA
  message('        Lecture des données')
  SEQUOIA::NAMEofSHP(rep)
  if (isFALSE(SEQ)){stop("Répertoire SEQUOIA non detecté")}
  
  PARCA <- st_read(rep, options = "ENCODING=UTF-8",
                   agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  assign("PARCA", PARCA, envir=globalenv())
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n \n")
  
  gha <- data.frame(RES_ESS = character(0),
                    RES_G = integer(0),
                    PLACETTE = character(0))
  
  tse <- data.frame(TSE_ESS = character(0),
                    TSE_DM = character(0),
                    PLACETTE = character(0))
  
  reg <- data.frame(REG_ESS = character(0),
                    REG_STADE = character(0),
                    REG_ETAT = character(0),
                    PLACETTE = character(0))
  
  va <- data.frame(VA_REG = character(0),
                   VA_ESS = character(0),
                   VA_AGE_APP = integer(0),
                   VA_STADE = character(0),
                   VA_NHA = numeric(0),
                   VA_HT = numeric(0),
                   VA_TX_TROUEE = numeric(0),
                   VA_ELAG = character(0),
                   VA_VEG_CO = character(0),
                   VA_TX_DG = numeric(0),
                   VA_DGT = character(0),
                   VA_RMQ = character(0),
                   PLACETTE = character(0))
  
  # Geometric table
  create_empty_sf <- function(geom_type, ...){
    empty_sf_geom <- st_sfc()
    class(empty_sf_geom) <- c(paste0("sfc_", toupper(geom_type)), class(empty_sf_geom)[-1])
    fields <- list(..., geometry = empty_sf_geom)
    template <- st_sf(fields, crs = 2154)
    return(template)
  }
  
  placette <- create_empty_sf(
    "POINT",
    PLACETTE = character(0),
    PLTM_PARC = character(0),
    PLTM_TYPE = character(0),
    PLTM_RICH = character(0),
    PLTM_STADE = character(0),
    PLTM_PB = character(0),
    PLTM_SANT = character(0),
    PLTM_MEC = character(0),
    PLTM_HISTO = character(0),
    PLTM_AME = character(0),
    PLTM_RMQ = character(0),
    PICTURES = character(0),
    TSE_DENS = character(0),
    TSE_VOL = numeric(0),
    TSE_NATURE = character(0),
    TSE_POTEN = character(0),
    TSE_CLOISO = character(0),
    TSE_RMQ = character(0)
  )
  
  limite <- create_empty_sf(
    "LINESTRING",
    TYPE = character(0),
    RMQ = character(0)
  )
  
  picto <- create_empty_sf(
    "POINT",
    TYPE = character(0),
    NATURE = character(0),
    PICTURES = character(0)
  )
  
  
  # Save
  out_qpkg <- paste(repout3, paste0(NAME,"_DIAGNOSTIC.gpkg"), sep="/")
  layers <- list(Gha = gha, Tse = tse, Reg = reg, Va = va,
                 Placette = placette, Limite = limite, Picto = picto)
  
  # Write layer using names of list before as layer name
  mapply(st_write,
         obj = layers,
         layer = names(layers),
         MoreArgs = list(dsn = out_qpkg, append = FALSE, quiet = TRUE)) |>
    invisible()
  
}
