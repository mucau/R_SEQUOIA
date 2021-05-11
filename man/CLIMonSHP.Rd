\name{CLIMonSHP}
\encoding{UTF-8}
\alias{CLIMonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recuperation des donnees climatiques sur l'emprise d'un .shp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction CLIMonSHP fournit la synthèse des données climatologiques sur une zone détude (\code{.shp} quelconque) en se basant sur les données METEO-FRANCE, AURELHY et DIGITALIS. Les informations pour les variables spatialisées sont fournies sur une étendue de 2,5 km autour de la zone entrée.

Cette fonction fournit les données nécéssaire à la production d'une fiche \code{.html} via le Rmakdown \code{CLIM.Rmd}.
}
\usage{
CLIMonSHP(repshp, repRdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repshp}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
  \item{repRdata}{
%%     ~~Describe \code{x} here~~
Répertoire des données \code{.Rdata}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction seule produit une multitude d'objet dans l'environnement nécessaire à la fonction \code{CLIM.Rmd}.
}
\value{
%%  ~Describe the value returned
  \item{METEOFRANCE_df}{
%%     ~~Describe \code{x} here~~
Dataframe des données METEO-FRANCE
}
  \item{AURELHY_df}{
%%     ~~Describe \code{x} here~~
Dataframe des données AURELHY
}
  \item{DIGITALIS_df}{
%%     ~~Describe \code{x} here~~
Dataframe des données AURELHY
}
  \item{RFN_sf_polygon}{
%%     ~~Describe \code{x} here~~
sf object polygon des régions forestières nationales identifiées sur la zone
}
  \item{SER_sf_polygon}{
%%     ~~Describe \code{x} here~~
sf object polygon des sylvoécorégions identifiées sur la zone
}

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
  CLIMonSHP(repshp = F, repRdata = F)
}
