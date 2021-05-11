\name{BD_PARCA}
\encoding{UTF-8}
\alias{BD_PARCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Chargement et export des donnees de la IGN (c) BD_PARCELLAIRE (r)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction BD_PARCA charge les données de la IGN (c) BD_PARCELLAIRE (r) autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) nécessaires ou utiles à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
BD_PARCA(PARCA, rep_BDPARCA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral
}
  \item{rep_BDPARCA}{
%%     ~~Describe \code{x} here~~
Répertoire de la BD_PARCELLAIRE utilisée
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Parmi les fichiers nécessaires :
 \item{ROAD_polygon}{
.shp Vides cadatrés environnants la propriété (routes+tronçons fluviaux)
}
Parmi les fichiers utiles :
\item{PARCELLES_polygon}{
Parcellaire cadastral global des communes (.shp)
}
  \item{BATICA_polygon}{
Batiments cadastrés environnants la propriété (.shp)
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
  BD_PARCA(PARCA)
}
