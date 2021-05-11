\name{ROADtoROUTE}
\encoding{UTF-8}
\alias{ROADtoROUTE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Conversion de ROAD vers ROUTE
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction ROADtoROUTE peut génére les .shp (EPSG 2154) polygone et ligne des routes à partir des vides cadastraux (ROAD_polygon) ou des routes référencées sur l'OSM (ROAD_line)

}
\usage{
ROADtoROUTE(road)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{road}{
ROAD_polygon.shp ou ROAD_line.shp
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Le shapefile road d'entrée doit disposer d'une table attributaire complétée. Le champ "TYPE" doit correspondre à la nomenclature suivante : routes nationnales & autoroutes (TYPE='RN'), départementales (TYPE='RD'), routes revêtues non départementales (TYPE='RC'), les routes empierrées/forestières (TYPE='RF') et les pistes en terrain naturel (TYPE='PN'). Dans ROAD_polygon.shp, les codes 'CR' pour chemins ruraux et 'CC' pour chemins privés cadastrés sont acceptés en plus des codes précédents.
}
\value{
%%  ~Describe the value returned
  \item{ROUTE_line}{
Contour du polygon
}
  \item{ROUTE_polygon}{
Polygon des routes
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
  ROADtoROUTE(road = F)
}
