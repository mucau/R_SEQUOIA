\name{INPNonPARCA}
\encoding{UTF-8}
\alias{INPNonPARCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Enjeux environnementaux sur une zone d'etude
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction INPNonPARCA recherche les intersections entre une zone d' étude (.shp quelconque) et les zonnages environnementaux référencées sur le site de l'INPN.
Pour chaque enjeux, une couche .shp contenant les zones intersectant la zone d'étude est exportée.
}
\usage{
INPNonPARCA(repshp, repRdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repshp}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .shp de la zone d'étude
}
  \item{repRdata}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .Rdata contenant les données INPN
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction réclame une archive .Rdata externe produite par la fonction \code{\link{INPNtoRDATA}}.
}
\value{
%%  ~Describe the value returned
La fontion retourne un ensemble de .shp

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
  repshp  <- tk_choose.files(caption = "Choisir le fichier .shp")
  repRdata  <- tk_choose.dir(caption = "Choisir le fichier .Rdata contenant les données INPN")

  INPNonPARCA(repshp, repRdata)
}
