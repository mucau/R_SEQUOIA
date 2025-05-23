% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RGBonSHP.R
\encoding{UTF-8}
\name{RGBonSHP}
\alias{RGBonSHP}
\title{RGBonSHP
Recuperation de l'IGN (c) BD Ortho RGB(r) sur une emprise shapefile}
\usage{
RGBonSHP(rep)
}
\arguments{
\item{rep}{Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.}
}
\value{
\item{RGB.tif}{Fichier raster .tiff ;  RGB enregistré dans le répertoire du fichier}
}
\description{
La fonction \code{RGBonSHP} télécharge IGN© BD Ortho RGB® à partir d'une emprise shapefile
}
\examples{
### Fonctionnement :
  RGBonSHP(rep=F)
}
\author{
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
