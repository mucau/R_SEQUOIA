\name{PARCAtoUA}
\encoding{UTF-8}
\alias{PARCAtoUA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation d'un ensemble de .shp a partir d'un parcellaire cadastral .shp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction PARCAtoUA génère un ensemble de .shp (EPSG 2154) nécessaires à la réalisation d'une cartographie forestière ponctuelle à partir d'un parcellaire cadastral .shp formatée (EPSG 2154).

}
\usage{
XLSXtoPARCA(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
repertoire du parcellaire cadastral PARCA_polygon
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{UA_polygone}{
Unités d'analyse
}
  \item{PROP_polygon}{
Propriété unifiée
}
  \item{PROP_line}{
Contours de la propriété unifiée
}
  \item{PROP_point}{
Centroide de la propriété unifiée
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
  rep  <- tk_choose.files(caption = "Choisir le fichier .shp contenant le parcellaire cadastral")

  PARCAtoUA(rep)
}
