\name{UAtoTOPO}
\encoding{UTF-8}
\alias{UAtoTOPO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation d'un .shp des lignes topographiques a partir du .shp des unites d'analyse et d'un MNT
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction UAtoTOPO génère un .shp (EPSG 2154) nécessaires à la réalisation d'une cartographie forestière ponctuelle : les lignes topographies (équidistances à 10 mètres).

La fonction appelle la fonction \code{MNTonSHP}.
}
\usage{
UAtoTOPO(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .shp des unités d'analyse (UA_polygone)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Parmi les fichiers nécessaires :
  \item{TOPO_line}{
Lignes topographiques lissées (équidistance 10 mètres)
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
  rep  <- tk_choose.files(caption = "Choisir le fichier .shp contenant les unités d'analyse")

  UAtoTOPO(rep)
}
