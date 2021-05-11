\name{AURELHYonSHP}
\encoding{UTF-8}
\alias{AURELHYonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Climatologie AURELHY sur une zone d'etude
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction AURELHYonSHP renseigne des données climatologiques sur une zone détude (\code{.shp} quelconque).

Les données climatiques Aurelhy (© Météo-France, 2001) ont été obtenues à partir de jeux de données ponctuelles des stations Météo-France qui ont été spatialisées en utilisant des modèles statistiques élaborés à l’aide de variables topographiques.

Les informations ci-dessous correspondantes aux normales déterminés par le modèle Aurelhy sur une étendue de 2,5 km autour de la forêt pour la période 1981-2010.
}
\usage{
AURELHYonSHP(repshp, repRdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repshp}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
  \item{repRdata}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.Rdata} contenant les données AURELHY. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction réclame une archive .Rdata externe non fournie.
}
\value{
%%  ~Describe the value returned
La fonction retourne un dataframe des données AURELHY: température (min, max, moy), précipitationet nombre de jours de gel. Ces renseignements sont fournies au mois et cumulés/moyennés à l'année.

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

  AURELHYonPARCA(repshp = F, repRdata = F)
}
