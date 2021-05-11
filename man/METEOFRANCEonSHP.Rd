\name{METEOFRANCEonSHP}
\encoding{UTF-8}
\alias{METEOFRANCEonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Climatologie METEOFRANCE sur une zone d'etude
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{METEOFRANCEonSHP} télécharge les données climatologiques de la station Météo-France la plus proche du centroide de la zone détude (\code{.shp} quelconque).

Les informations récupérées correspondantes aux normales et records disponibles pour cette station.
}
\usage{
METEOFRANCEonSHP(repshp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repshp}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
La fonction retourne un dataframe des données METEOFRANCE. Ces renseignements sont fournies au mois et cumulés/moyennés à l'année.

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

  METEOFRANCEonSHP(repshp = F)
}
