\name{LOADfromIGN}
\encoding{UTF-8}
\alias{LOADfromIGN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement des donnees IGN
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{LOADfromIGN} permet de télécharger rapidement les données de l'IGN (C).
  \tabular{ll}{
    \tab >>  BD Parcellaire® \cr
    \tab >>  BD TOPO® \cr
    \tab >>  BD FORET® V2 \cr
    \tab >>  RGE ALTI 5m \cr
    \tab >>  BD ALTI® V2 25M \cr
  }
La fonction demande le département consernée.
}
\usage{
LOADfromIGN(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
CHARACTER. Répertoire du dossier de téléchargement. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction télécharge les données depuis le serveur \url{http://files.opendatarchives.fr/professionnels.ign.fr/}
}
\value{
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
  LOADfromIGN(rep = F)
}
