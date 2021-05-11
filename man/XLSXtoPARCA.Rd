\name{XLSXtoPARCA}
\encoding{UTF-8}
\alias{XLSXtoPARCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation d'un parcellaire cadastral .shp a partir d'une matrice cadastrale .xlsx
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction XLSXtoPARCA génère deux parcellaires cadastrals au format .shp (EPSG 2154) à partir d'une matrice cadastral formatée au format .xlsx .

}
\usage{
XLSXtoPARCA(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .xlsx contenant la matrice cadastrale
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
  \item{PARCA_polygon}{
%%     ~~Describe \code{x} here~~
Parcellaire cadastrale .shp (EPSG 2154) pour post-traitement
}
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
Parcellaire cadastrale .shp (EPSG 2154) pour archivage
}
La fonction génère également un objet sf   "PARCA_polygon" dans l'environnement de travail.
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
  rep  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales")

  XLSXtoPARCA <- function(rep)
}
