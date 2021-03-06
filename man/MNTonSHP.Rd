% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MNTonSHP.R
\encoding{UTF-8}
\name{MNTonSHP}
\alias{MNTonSHP}
\title{MNTonSHP
Recuperation d'un modele numerique de terrain et creation des contours d'elevation}
\usage{
MNTonSHP(REP_SHP, NAME, TEMP)
}
\arguments{
\item{REP_SHP}{CHARACTER. Adresse du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.}

\item{NAME}{CHARACTER. Nom du fichier de sortie sans l'extension.Défault = \code{"TOPO_line"}}

\item{TEMP}{NUMERIC. Valeur du tempon (buffer) pour l'emprise de travail. Défault = \code{200}}
}
\value{
\item{MNT.tif}{Fichier raster .tiff ;  MNT enregistré dans le répertoire du fichier}
\item{TOPO_line}{Objet sf ; contours enregistré dans le répertoire du fichier}
}
\description{
Recuperation d'un modele numerique de terrain et creation des contours d'elevation
La fonction \code{MNTonSHP} récupère ou télécharge un modèle numérique de terrain pour une zone d'étude et génère des contours d'élévation topographiques (équidistances à 5 ou 10 mètres) enregistrés au format \code{.shp}.
Le MNT récupérée peut correspondre à celui proposer sur l'Amazon Web Service, à celui de l'IGN© BD Alti® ou à celui de l'IGN© RGE Alti 5m
}
\examples{
### Fonctionnement :
  TOPO_line <- MNTonSHP(REP_SHP = F, NAME = NULL, TEMP = 50)
}
\author{
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
