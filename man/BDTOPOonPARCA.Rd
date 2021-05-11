\name{BDTOPOonPARCA}
\encoding{UTF-8}
\alias{BDTOPOonPARCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement de vegetation depuis IGN(c) BD TOPO(r)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction BDTOPOonPARCA charge les données de végétations depuis IGN© BD TOPO® autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
BDTOPOonPARCA(rep, repBDTOPO)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
repertoire du parcellaire cadastral PARCA_polygon
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
Les fichiers suivants sont produits :
  \item{COMS_polygon}{
Communes environnantes (.shp) (Tempon de 2000m autour du PARCA_polygon)
}
  \item{COMS_line}{
Contours des communes environnantes (.shp) (Tempon de 2000m autour du PARCA_polygon)
}
  \item{COM_line}{
Contours des communes environnantes (.shp) (Tempon de 500m autour du PARCA_polygon)
}
  \item{COMS_point}{
Centroid des communes environnantes (.shp) (Tempon de 2000m autour du PARCA_polygon)
}
  \item{COM_point}{
Centroid des communes environnantes (.shp) (Tempon de 500m autour du PARCA_polygon)
}
  \item{INFRA_polygon}{
sf Infrastructures environnantes (sf dans l'environnement).Y sont intégrés les batis (TYPE='BT'), les cimetières 'CIM' (TYPE='CIM'), les terrains de sports (TYPE='SP') et les constructions surfaciques (TYPE='CSTsurf')
}
  \item{INFRA_line}{
sf Infrastructures environnantes (sf dans l'environnement). Y sont intégrés les constructions linéaires (TYPE='CSTline'), les ignes orographiques (TYPE='ORO'), les lignes électriques (TYPE='LE') et les voies ferrées (TYPE='VF').
}
  \item{ROAD_line}{
Routes environnantes (.shp). Y sont intégrés les routes nationnales & autoroutes (TYPE='RN'), départementales (TYPE='RD'), communales (TYPE='RC'), les routes empierrées/forestières (TYPE='RF') et les pistes en terrain naturel (TYPE='PN')
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
  BDTOPOonPARCA(rep=F, repBDTOPO=F))
}
