\name{HTMLtoXLSX}
\encoding{UTF-8}
\alias{HTMLtoXLSX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation d'un .xlsx a partir de matrices cadastrales .html
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction HTMLtoXLSX parcourt un dossier contenant des matrices cadastrales au format .html et génère un fichier .xlsx compilant l'ensemble des matrices.

}
\usage{
HTMLtoXLSX(rephtml, repRdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rephtml}{
%%     ~~Describe \code{x} here~~
Répertoire du dossier contenant les matrices cadastrales .html
}
  \item{repRdata}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .Rdata contenant les données administratives
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
L'archive .Rdata contenant les données administratives est produite par la fonction \code{\link{INSEEtoRDATA}}.
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
  rephtml  <- tk_choose.dir(caption = "Choisir le dossier des extraits de matrice cadastrale")
  repRdata  <- tk_choose.dir(caption = "Choisir le fichier .Rdata des données administratives")

  HTMLtoXLSX(rephtml, repRdata)
}
