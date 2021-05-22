\name{BHbyDRIAS}
\encoding{UTF-8}
\alias{BHbyDRIAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Traitement des donnees DRIAS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{BHbyDRIAS} traite les données récupérée sur le serveur DRIAS.

Procédure:
  \tabular{ll}{
    1) \tab Aller sur https://drias-prod.meteo.fr/okapi/accueil/okapiWebDrias/index.jsp \cr
    2) \tab Cliquer sur \emph{Simulations climatiques atmosphériques} / \emph{Métropole} / \emph{INDICATEUR DRIAS 2020} \cr
    3) \tab Sélectioner \emph{Indicateurs mensuels 'DRIAS-2020' par horizon} \cr
    4) \tab Dans \emph{Sélection du jeu de données }, sélectioner votre modèle. Je conseille \emph{ALADIN63_CNRM-CM5} / \emph{RCP8.5} \cr
    5) \tab Dans \emph{Référence temporelle}, cocher toutes les cases \cr
    6) \tab Dans \emph{Référence géographique}, chercher votre secteur \cr
    7) \tab Dans \emph{Indicateurs météorologiques}, cliquer sur "tous" \cr
    
  }

}
\usage{
BHbyDRIAS(txt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.txt} issu de DRIAS. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
  \item{DRIAS_df}{
Dataframe des données DRIAS (brut)
}
  \item{DRIAS_serie}{
Dataframe des données pour création de diagramme ombrothermique
}
  \item{DRIAS_bh}{
Dataframe des données pour création de diagramme ETP
}
  \item{Periode}{
Horizon retenu dans le code
}

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

   BHbyDRIAS(txt = F)
}
