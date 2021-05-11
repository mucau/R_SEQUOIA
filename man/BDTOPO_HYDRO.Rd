\name{BDTOPO_HYDRO}
\encoding{UTF-8}
\alias{BDTOPO_HYDRO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement des donnees IGN(c) BD TOPO(r) HYDROGRAPHIE
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction BDTOPO_HYDRO charge et exporte les données IGN© BD TOPO® HYDROGRAPHIE autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
BDTOPO_HYDRO(PARCA, repBDTOPO)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral
}
  \item{repBDTOPO}{
%%     ~~Describe \code{x} here~~
Répertoire de la IGN© BD TOPO® HYDROGRAPHIE. Deux choix possibles: départementale ou métropolitaine.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Les objets sf suivants sont produits :
  \item{HYDRO_polygon}{
Polygones hydrologique des surfaces hydrographiques permanentes (SURFO), intermittentes (SURFOi) et des réservoirs d'eau (RESO) (.sf)
}
  \item{HYDRO_line}{
Lignes hydrologiques des tronçons fluviaux permanentes (RU), intermittents (RUi) et des canalisations (CANO) (.sf)
}
  \item{HYDRO_point}{
Points hydrologiques des points d'eau (PTSO) et hydronymes (NOMO) (.sf)
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
  BDTOPO_HYDRO(PARCA, repBDTOPO)
}
