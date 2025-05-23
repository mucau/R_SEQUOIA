% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MNHonSHP.R
\encoding{UTF-8}
\name{MNHonSHP}
\alias{MNHonSHP}
\title{MNHonSHP
Recuperation du modèle numérique de hauteur de l'IGN©}
\usage{
MNHonSHP(rep)
}
\arguments{
\item{rep}{Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.}
}
\value{
\item{MNH.tif}{Fichier raster .tiff ;  MNH enregistré dans le répertoire du fichier}
}
\description{
La fonction \code{MNHonSHP} télécharge le MNT et MNS BD ALti® IGN© à partir d'une emprise shapefile
}
\examples{
### Fonctionnement :
  MNHonSHP(rep=F)
}
\author{
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
