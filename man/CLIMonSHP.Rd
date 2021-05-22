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
La fonction \code{CLIMonSHP} fournit les données nécéssaire à la production d'une fiche \code{.html} via le Rmakdown \code{CLIM.Rmd}. La fiche fait synthèse des données climatologiques sur une zone détude (\code{.shp} quelconque).

La synthèse fournit:
  \tabular{ll}{
    \tab >>  Les normales en se basant sur les données issues des stations METEO-FRANCE (ou des données AURELHY) \cr
    \tab >>  Les prévisions attendues en se basant sur les données Drias. \cr
  }

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
Pour plus de renseignements, voir les notices des fonctions suivantes:
  \tabular{ll}{
    \tab >>  \code{METEOFRANCEonSHP} \cr
    \tab >>  \code{AURELHYonSHP} \cr
    \tab >>  \code{BHbyDRIAS} \cr
  }
}
\value{
%%  ~Describe the value returned
  \item{METEOFRANCE_df}{
Dataframe des données METEO-FRANCE
}
  \item{AURELHY_df}{
Dataframe des données AURELHY
}
  \item{DRIAS_df}{
Dataframe des données DRIAS
}
  \item{RFN_sf_polygon}{
sf object polygon des régions forestières nationales identifiées sur la zone
}
  \item{SER_sf_polygon}{
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
