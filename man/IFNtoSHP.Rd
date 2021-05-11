\name{IFNtoSHP}
\encoding{UTF-8}
\alias{IFNtoSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recuperation des donnees .shp de l'IFN
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{IGNtoSHP} permet de télécharger les fichiers shapefiles disponibles depuis le serveur de l'IFN.
}
\usage{
IFNtoSHP(code)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code}{
%%     ~~Describe \code{x} here~~
CHARACTER. Code du fichier à télécharger depuis le serveur IFN
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{
%%  ~Describe the value returned
La fonction retourne un shapefile fonction du code.

Les codes sont les suivants :
  \item{'RFN'}{
%%     ~~Describe \code{x} here~~
Régions forestières nationales
}
  \item{'RFD'}{
%%     ~~Describe \code{x} here~~
Régions forestières départementales
}
  \item{'SER'}{
%%     ~~Describe \code{x} here~~
Sylvoécorégions
}
  \item{'SERAR'}{
%%     ~~Describe \code{x} here~~
Sylvoécorégions d'alluvions récentes
}
}
\references{
%% ~put references to the literature/web site here ~
Les données sont explicitées sur le portail de l'IFN:
\url{http://inventaire-forestier.ign.fr/carto/carto/afficherCarto/}
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
  SER_polygon <- IFNtoSHP(code = 'SER')
}
