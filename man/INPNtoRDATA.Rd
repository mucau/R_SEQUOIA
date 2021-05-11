\name{INPNtoRDATA}
\encoding{UTF-8}
\alias{INPNtoRDATA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation d'une archive .Rdata des enjeux environnementaux
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction INPNtoRDATA télécharge depuis le site de l'INPN l'ensemble des fichiers .shp des enjeux environnementaux et sauvegarde ces couches dans une archive .Rdata .

}
\usage{
INPNtoRDATA(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire de sortie de l'archive .Rdata
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction produit une archive .Rdata nécessaire à la fonction \code{\link{INPNonPARCA}}.
}
\value{
%%  ~Describe the value returned
  \item{list_INPN}{
%%     ~~Describe \code{x} here~~
Liste de fichiers cartographiques sf
}
  \item{list_INPN_NOM}{
%%     ~~Describe \code{x} here~~
Liste des noms des fichiers cartographiques sf de la liste précédente
}

}
\references{
%% ~put references to the literature/web site here ~
Vous pouvez retrouvé les données environnementales sur le site de l'Institut national pour la protection de la nature (INPN) : \url{https://inpn.mnhn.fr/accueil/index}
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

  INPNtoRDATA(rep)
}
