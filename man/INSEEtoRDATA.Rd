\name{INSEEtoRDATA}
\encoding{UTF-8}
\alias{INSEEtoRDATA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recuperation des donnees administratives (et creation d'une archive R.data)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction INSEEtoRDATA télécharge depuis la plateforme ouverte des données publiques françaises (data.gouv) et du site de l'INSEE plusieurs fichiers de référence administratives. La fonction les sauvegarde ensuite dans une archive .Rdata si un répertoire de sauvegarde est fournit.
}
\usage{
INSEEtoRDATA(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire de sortie de l'archive .Rdata. Si \code{NULL}, les données sont justes ajoutées à l'environnement.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction produit une archive .Rdata nécessaire à la fonction \code{\link{HTMLtoXLSX}}.
}
\value{
%%  ~Describe the value returned
  \item{INSEE_POST}{
%%     ~~Describe \code{x} here~~
Base officielle des codes postaux
}
INSEE : Codes officiels géographiques au 1er janvier 2018
  \item{INSEE_REGS}{
%%     ~~Describe \code{x} here~~
Liste des régions
}
  \item{INSEE_DEPS}{
%%     ~~Describe \code{x} here~~
Liste des départements
}
  \item{INSEE_COMS}{
%%     ~~Describe \code{x} here~~
Liste des communes
}

}
\references{
%% ~put references to the literature/web site here ~
Les codes des champs de l'INSEE sont explicités sur le site de l'institut :
\url{https://www.insee.fr/fr/information/3363504}
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
  rep  <- tk_choose.files(caption = "Choisir le répertoire de sortie")

  INSEEtoRDATA(rep)
}
