\name{WRITE}
\encoding{UTF-8}
\alias{WRITE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Export d'un .shp EPSG: 2154 ~ UTF-8
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction WRITE exporte un sf vers un répertoire de choix avec le nom choisit par l'utilisateur avec toutes les configurations voulues.

}
\usage{
WRITE(sf, repout, nom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sf}{
%%     ~~Describe \code{x} here~~
Couche sf à exporter
}
  \item{repout}{
%%     ~~Describe \code{x} here~~
Répertoire d'export
}
  \item{nom}{
%%     ~~Describe \code{x} here~~
Nom de la couche
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
  WRITE(PARCA, repout2, paste(NAME,"PARCA.shp",sep="_"))

}
