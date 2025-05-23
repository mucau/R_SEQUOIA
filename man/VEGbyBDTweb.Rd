% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VEGbyBDTweb.R
\encoding{UTF-8}
\name{VEGbyBDTweb}
\alias{VEGbyBDTweb}
\title{VEGbyBDTweb
Telechargement de vegetation depuis l'IGN(c) BD TOPO(r))}
\usage{
VEGbyBDTloc(PARCA, bdt)
}
\arguments{
\item{rep}{Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile}

\item{bdt}{Character. Répertoire de la IGN© BD TOPO®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier}
}
\value{
\item{VEG_polygon}{Objet sf ; Polygones des surfaces forestières ; TYPE='VEG'}
\item{VEG_line}{Objet sf ;Lignes contours surfaces forestières ; TYPE='VEG'}
\item{VEG_point}{Objet sf ;Points natures de culture ; TYPE='VEG'}
}
\description{
La fonction \code{VEGbyBDTloc} télécharge les données de végétations depuis IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\examples{
### Fonctionnement :
    VEGbyBDTweb(rep=F, bdt=F)
}
\author{
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
