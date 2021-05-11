\name{MNTonSHP}
\encoding{UTF-8}
\alias{MNTonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recuperation d'un modele numerique de terrain et creation des contours d'elevation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{MNTonSHP} récupère ou télécharge un modèle numérique de terrain pour une zone d'étude et génère des contours d'élévation topographiques (équidistances à 5 ou 10 mètres) enregistrés au format \code{.shp}.

Le MNT récupérée peut correspondre à celui proposer sur l'Amazon Web Service, à celui de l'IGN© BD Alti® ou à celui de l'IGN© RGE Alti 5m
}
\usage{
MNTonSHP(REP_SHP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{REP_SHP}{
%%     ~~Describe \code{x} here~~
CHARACTER. Répertoire du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
  \item{NAME}{
%%     ~~Describe \code{x} here~~
CHARACTER. Nom du fichier de sortie sans l'extension.Défault = \code{"TOPO_line"}
}
  \item{TEMP}{
%%     ~~Describe \code{x} here~~
NUMERIC. Valeur du tempon (buffer) pour l'emprise de travail. Défault = \code{200}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction produit un MNT et un fichier \code{.shp} utile à la création d'une cartographie forestière ponctuelle.
}
\value{
%%  ~Describe the value returned
  \item{MNT.tif}{
\code{Raster} object du MNT enregistré dans le répertoire du fichier \code{.shp}.
}
  \item{"TOPO_line"}{
\code{sf} object des contours enregistré dans le répertoire du fichier \code{.shp}. L'object est retournée par la fonction.
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

  TOPO_line <- MNTonSHP(REP_SHP = F, NAME = NULL, TEMP = 50)
}
