\name{BDTOPO_VEG}
\encoding{UTF-8}
\alias{BDTOPO_VEG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement de vegetation depuis IGN(c) BD TOPO(r))
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction BDTOPO_VEG charge les données de végétations depuis IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
BDTOPO_VEG(PARCA, repBDTOPO)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral.
}
  \item{repBDTOPO}{
%%     ~~Describe \code{x} here~~
Répertoire de la IGN© BD TOPO® départementale
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Les objets sf suivants sont produits :
  \item{VEG_polygon}{
Polygones des surfaces forestières (sf) (TYPE='VEG')
}
  \item{VEG_line}{
Lignes contours surfaces forestières(sf) (TYPE='VEG')
}
  \item{VEG_point}{
Points natures de culture (sf) (TYPE='VEG')
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
  BDTOPO_VEG(PARCA=F, repBDTOPO=F))
}
