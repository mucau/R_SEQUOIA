\name{OSMonPARCA}
\encoding{UTF-8}
\alias{OSMonPARCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement des donnees OpenStreetMap
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction OSMonPARCA télécharge les données OSM autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
OSMonPARCA(rep)
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
sf Infrastructures environnantes (sf dans l'environnement).Y sont intégrés les batis (TYPE='BT'), les cimetières 'CIM' (TYPE='CIM') et les terrains de sports (TYPE='SP')
}
  \item{INFRA_line}{
sf Infrastructures environnantes (sf dans l'environnement). Y sont intégrés les lignes électriques (TYPE='LE') et les voies ferrées (TYPE='VF').
}
  \item{ROAD_line}{
Routes environnantes (.shp). Le TYPE renseigné correspond à la nomenclature de l'OSM.
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
Pour utilier le script ROADtoROUTE, le champ "TYPE"" de ROAD_line doit être rectifié mannuellement pour correspondre à la nomenclature suivante : routes nationnales & autoroutes (TYPE='RN'), départementales (TYPE='RD'),  routes revêtues non départementales (TYPE='RC'), les routes empierrées/forestières (TYPE='RF') et les pistes en terrain naturel (TYPE='PN')
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### Fonctionnement :
  OSMonPARCA(rep)
}
