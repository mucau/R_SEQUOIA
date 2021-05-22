\name{BDFORETonSHP}
\encoding{UTF-8}
\alias{BDFORETonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Intersection d'un shapefile avec la IGN (C) BD Foret V2 (r)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{BDFORETonSHP} intersecte un shapefile avec la  IGN (C) BD Foret V2 (r).

}
\usage{
BDFORETonSHP(shp, repBDFORET)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{shp}{
CHARACTER. Répertoire du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
   \item{repBDFORET}{
CHARACTER. Répertoire du dossier de la BD. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
}
\value{
%%  ~Describe the value returned
  \item{BDFORET_polygon}{
Shapefile de la BDFORET sur l'emprise. Généré dans le répertoire du fichier source.
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
  BDFORETonSHP(shp = F, BDFORETonSHP = F)
}
