\name{CAGEF}
\encoding{UTF-8}
\alias{CAGEF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation de donnees d'habillage "CAGEF"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction CAGEF utilise les fonctions OSMonPARCA, BDTOPOonPARCA, BDTOPO_HYDRO, BDTOPO_VEG et ETALAB/BD_PARCA/EDIGEO pour générer un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
CAGEF(rep, CODECA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
repertoire du parcellaire cadastral PARCA_polygon
}
  \item{CODECA}{
%%     ~~Describe \code{x} here~~
Code de référence des données cadastrales à utiliser 1: Etalab, 2: IGN© BD Parcellaire®, 3: Edigeo
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Cf. Autres ressources
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
  CAGEF(rep=F, CODECA=1)
}
