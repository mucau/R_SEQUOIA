\name{ETALAB}
\encoding{UTF-8}
\alias{ETALAB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement des donnees ETALAB
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction ETALAB télécharge les données ETALAB autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires ou utiles à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
ETALAB(PARCA)
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
  \item{BATICA_polygon}{
.shp Batiments cadastré environnants la propriété
}
  \item{LIEUDIT_point}{
.shp Lieudits environnants la propriété
}
  \item{ROAD_polygon}{
.shp Vides cadatrés environnants la propriété (routes+tronçons fluviaux)
}
Parmi les fichiers utiles :
\item{PARCELLES_polygon}{
Parcellaire cadastral global des communes
}
  \item{TSURF_polygon}{
Etangs et mares cadastrées
}
  \item{TINE_line}{
Lignes cadastrales (chemins, notes, etc.)
}
  \item{TRONFLUV_polygon}{
Ruisseaux cadastrés
}
  \item{COMMUNI_line}{
Tronçons routiers inscrits au cadastre
}
  \item{SUBDFISC_polygon}{
Subdivisions fiscales
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
  ETALAB(PARCA)
}
