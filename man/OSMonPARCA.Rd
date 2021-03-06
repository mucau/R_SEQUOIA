% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OSMonPARCA.R
\encoding{UTF-8}
\name{OSMonPARCA}
\alias{OSMonPARCA}
\title{OSMonPARCA
Telechargement des donnees OpenStreetMap}
\usage{
OSMonPARCA(rep)
}
\arguments{
\item{rep}{CHARACTER. Adresse du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.}
}
\value{
\item{COMS_polygon}{Fichier shapefile ; Communes environnantes ; Tempon de 2000m autour du PARCA_polygon}
\item{COMS_line}{Fichier shapefile ; Contours des communes environnantes ; Tempon de 2000m autour du PARCA_polygon}
\item{COM_line}{Fichier shapefile ; Contours des communes environnantes ; Tempon de 500m autour du PARCA_polygon}
\item{COMS_point}{Fichier shapefile ; Centroid des communes environnantes ; Tempon de 2000m autour du PARCA_polygon}
\item{COM_point}{Fichier shapefile ; Centroid des communes environnantes ; Tempon de 500m autour du PARCA_polygon}
\item{INFRA_polygon}{Objet sf ; Infrastructures environnantes. Y sont intégrés TYPE='BT' : les batis, TYPE='CIM' : les cimetières, TYPE='SP' : les terrains de sports}
\item{INFRA_line}{Objet sf ; Infrastructures environnantes. Y sont intégrés TYPE='LE' : les lignes électriques, TYPE='VF' : les voies ferrées}
\item{ROAD_line}{Fichier shapefile ; Routes environnantes. Le TYPE renseigné correspond à la nomenclature de l'OSM.}
}
\description{
La fonction \code{OSMonPARCA} télécharge les données OSM autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\note{
Pour utilier le script \code{\link{ROADtoROUTE}}, le champ "TYPE" de ROAD_line doit être rectifié mannuellement pour correspondre à la nomenclature suivante = TYPE='RN' : routes nationnales & autoroutes, TYPE='RD' : routes départementales,  TYPE='RC' : routes revêtues non départementales, TYPE='RF' : les routes empierrées/forestières, TYPE='PN' : les pistes en terrain naturel
}
\examples{
### Fonctionnement :
  OSMonPARCA(rep=F)
}
\author{
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
