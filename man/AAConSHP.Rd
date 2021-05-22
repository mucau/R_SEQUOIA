\name{AAConSHP}
\encoding{UTF-8}
\alias{AAConSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Detection des monuments historiques
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{AAConSHP} détecte la présence d'aires d'alimentation de captage dans l'environnement immédiat d'un shapefile.
}
\usage{
AAConSHP(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
CHARACTER. Répertoire du fichier \code{.shp}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction s'inspire du package \code{cadastreAnalysis}.
}
\value{
%%  ~Describe the value returned
La fonction renvoit un affichage des éléments.
}
\references{
%% ~put references to the literature/web site here ~
}

\author{
%%  ~~who you are~~
Paul CARTERON <\email{carteronpaul@gmail.com}>
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

  AAConSHP(rep=F)
}
