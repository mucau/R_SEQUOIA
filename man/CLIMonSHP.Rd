% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CLIMonSHP.R
\encoding{UTF-8}
\name{CLIMonSHP}
\alias{CLIMonSHP}
\title{CLIMonSHP
Recuperation des donnees climatiques sur l'emprise d'un .shp}
\usage{
CLIMonSHP(shp)
}
\arguments{
\item{shp}{Adresse du fichier \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.}
}
\value{
\item{METEOFRANCE_df}{Dataframe des données METEO-FRANCE}
\item{AURELHY_df}{Dataframe des données AURELHY}
\item{DRIAS_df}{Dataframe des données DRIAS}
\item{RFN_sf_polygon}{Objet sf ; polygones des régions forestières nationales identifiées sur la zone}
\item{SER_sf_polygon}{Objet sf ; polygones des sylvoécorégions identifiées sur la zone}
}
\description{
La fonction \code{CLIMonSHP} fournit les données nécéssaire à la production d'une fiche \code{.html} via le Rmakdown \code{CLIM.Rmd}. La fiche fait synthèse des données climatologiques sur une zone détude (\code{.shp} quelconque).
La synthèse fournit:
\enumerate{
  \item Les normales en se basant sur les données issues des stations METEO-FRANCE (ou des données AURELHY)
  \item Les prévisions attendues en se basant sur les données Drias.
}
}
\details{
La fonction seule produit une multitude d'objet dans l'environnement nécessaire à la fonction \code{CLIM.Rmd}.
}
\examples{
### Fonctionnement :
  CLIMonSHP(shp = F)
}
\seealso{
Pour mieux comprendre, voir les notices des fonctions suivantes:
\code{\link{METEOFRANCEonSHP}}, \code{\link{AURELHYonSHP}}, \code{\link{BHbyDRIAS}}
}
\author{
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
