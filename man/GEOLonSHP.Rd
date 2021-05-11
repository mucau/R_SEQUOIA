\name{GEOLonSHP}
\encoding{UTF-8}
\alias{GEOLonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Import de la BRGM (C) BD_Charm50 (r) sur une emprise shapefile
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction GEOLonSHP téléchargement la BRGM (C) BD_Charm50 (r) départementale détecté sur l'emprise du shapefile d'entrée, réalise une intersection des couches et calcul la surface SIG par entité géologique.

}
\usage{
GEOLonSHP(shp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{shp}{
Shapefile d'emprise (polygon). Par défault FALSE génère une boite de sélection.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
  \item{geol_polygon}{
Shapefile de la géologie sur l'emprise. Généré dans le répertoire du fichier source.
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
  GEOLonSHP(shp = F)
}
