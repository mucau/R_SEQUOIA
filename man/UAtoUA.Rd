\name{UAtoUA}
\encoding{UTF-8}
\alias{UAtoUA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Actualisation du .shp des unites d'analyse
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction UAtoUA corrige les éventuelles erreurs de surfaces cadastrales dues aux traitements cartographique sur le .shp des unités d'analyse.

Elle complète le .shp par l'ajout des colonnes de surfaces.
}
\usage{
UAtoUA(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .shp des unités d'analyse (UA_polygon)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Parmi les fichiers nécessaires :
  \item{SURF_SIG}{
Surface cartographique selon R
}
  \item{SURF_COEFF}{
Coefficient de correction à appliquer à \code{SURF_SIG} pour obtenir \code{SURF_COR}
}
  \item{SURF_COR}{
Surface cadastrale corrigée
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
Il a été constaté que R et Qgis ne donnaient pas les mêmes valeurs de \code{SURF_SIG} mais cela est sans impact sur la \code{SURF_COR}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### Fonctionnement :
  rep  <- tk_choose.files(caption = "Choisir le fichier .shp contenant les unités d'analyse")

  UAtoUA(rep)
}
