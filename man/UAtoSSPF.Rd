\name{UAtoSSPF}
\encoding{UTF-8}
\alias{UAtoSSPF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation d'un ensemble de .shp a partir du .shp des unites d'analyse
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction UAtoSSPF génère un ensemble de .shp (EPSG 2154) nécessaires à la réalisation d'une cartographie forestière ponctuelle à partir du .shp des unités d'analyse complété (EPSG 2154).

La fonction peut également actualiser l'étiquetage du parcellaire de gestion.
}
\usage{
UAtoSSPF(rep)
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
  \item{PF_polygon}{
Parcellaire forestier
}
  \item{PF_line}{
Contour du parcellaire forestier
}
  \item{SSPF_polygon}{
Sous-parcellaire forestier (parcellaire de gestion)
}
  \item{SSPF_line}{
Etiquette du sous-parcellaire --> actualisation
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

  UAtoSSPF(rep)
}
