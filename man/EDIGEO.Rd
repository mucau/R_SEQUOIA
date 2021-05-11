\name{EDIGEO}
\encoding{UTF-8}
\alias{EDIGEO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Chargement et export des donnees EDIGEO du cadastre.gouv.fr
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction EDIGEO charge les données EDIGEO du cadastre.gouv.fr autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) nécessaires ou utiles à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
EDIGEO(PARCA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Parmi les fichiers nécessaires :
  \item{LIEUDIT_point}{
Lieudits environnants la propriété (.shp)
}
  \item{ROAD_polygon}{
.shp Vides cadatrés environnants la propriété (routes+tronçons fluviaux)
}
Parmi les fichiers utiles :
  \item{BATICA_polygon}{
Batiments cadastré environnants la propriété (.shp)
}
\item{PARCELLES_polygon}{
Parcellaire cadastral global des communes (.shp)
}
  \item{TSURF_polygon}{
Etangs et mares cadastrées (.shp)
}
  \item{TINE_line}{
Lignes cadastrales (chemins, notes, etc.) (.shp)
}
  \item{TRONFLUV_polygon}{
Ruisseaux cadastrés (.shp)
}
  \item{COMMUNI_line}{
Tronçons routiers inscrits au cadastre (.shp)
}
  \item{SUBDFISC_polygon}{
Subdivisions fiscales (.shp)
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### Fonctionnement :
  EDIGEO(PARCA)
}
