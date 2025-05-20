#' @title BHbyDRIAS
#' Traitement des donnees DRIAS
#' @encoding UTF-8
#' @description 
#' La fonction \code{BHbyDRIAS} traite les données récupérée sur le serveur DRIAS.
#' Procédure:
#' \enumerate{
#'   \item Aller sur \url{https://drias-prod.meteo.fr/okapi/accueil/okapiWebDrias/index.jsp} 
#'   \item Cliquer sur \emph{Simulations climatiques atmosphériques} / \emph{Métropole} / \emph{INDICATEUR DRIAS 2020}
#'   \item Sélectioner \emph{Indicateurs mensuels 'DRIAS-2020' par horizon}
#'   \item Dans \emph{Sélection du jeu de données }, sélectioner votre modèle. Je conseille \emph{ALADIN63_CNRM-CM5} / \emph{RCP8.5}
#'   \item Dans \emph{Référence temporelle}, cocher toutes les cases
#'   \item Dans \emph{Référence géographique}, chercher votre secteur
#'   \item Dans \emph{Indicateurs météorologiques}, cliquer sur "tous"
#' }
#' @usage BHbyDRIAS(txt)
#' @param txt Répertoire du fichier \code{.txt} issu de DRIAS. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return Les fichiers sf suivant sont généré dans l'environnement
#' \item{DRIAS_df}{Dataframe des données brut DRIAS} 
#' \item{DRIAS_serie}{Dataframe des données pour création de diagramme ombrothermique}
#' \item{RIAS_bh}{Dataframe des données pour création de diagramme ETP}
#' \item{periode}{Horizon retenu dans le code}
#' @return Les horizons retenus sont les suivants :
#' \enumerate{
#' \item H1 Horizon proche : indicateurs calculés sur la période 2021-2050
#' \item H2 Horizon moyen : indicateurs calculés sur la période 2041-2070
#' \item H3 Horizon lointain : indicateurs calculés sur la période 2071-2100
#' }
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'    BHbyDRIAS(txt = F)
#' @export
#' 
#' @import tcltk utils sf dplyr stringr

# Lancement des library
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}

BHbyDRIAS <- function(txt=F, periode=F) {
  # Sélection du fichier
  if(isFALSE(txt)){
    txt  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .txt de donnée",
                                   filter = matrix(c("Fichier texte", ".txt"), 1, 2, byrow = TRUE))
  }
  if (!length(txt)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  # Lecture du chapeau
  header <- read.delim(file=txt, encoding="latin1")
  for (a in 1:nrow(header)){
    if (grepl("Modele", header[a,1])) {Modele <- str_replace(header[a,1], "# Modele     : ", "")}
    if (grepl("Scenario ", header[a,1])) {Scenario <- str_replace(header[a+1,1], "#     ", "")}
    if (grepl("# Format des enregistrements", header[a,1])) {colnames <- strsplit(str_replace(header[a+1,1], "# ", ""), ";")[[1]]}
  }
  colnames[length(colnames)+1]<-""
  data <- read.table(file=txt, sep=";", quote="", col.names=colnames)
  assign("DRIAS_df", data, envir=globalenv())
  
  #Sélection de la période
  if(isFALSE(periode)){
    form <- c("H1 Horizon proche [2021-2050]",
              "H2 Horizon moyen [2041-2070]", 
              "H3 Horizon lointain [2071-2100]")
    
    periode <- utils::select.list(form, multiple = F,
                                  title = "Quelle période ?",
                                  graphics = T)
    }
  if (periode==""){stop("Aucune source sélectionnée !")}
  
  Horizon <- str_sub(periode, 0, 2)
  
  serie <- data %>%
    filter(periode %in% Horizon) %>% 
    group_by(Mois) %>%
    summarize(Tm = mean (NORTAV), 
              Tn = mean(NORTNAV), 
              Tx = mean(NORTXAV), 
              P = mean(NORRR), 
              ETP = mean(NORETPC)) %>%
    mutate(Période = Horizon) %>%
    select(Période, Mois, Tn, Tx, P, ETP)
  
  assign("DRIAS_modele", Modele, envir=globalenv())
  assign("DRIAS_scenario", Scenario, envir=globalenv())
  assign("DRIAS_horizon", Horizon, envir=globalenv())
  assign("DRIAS_periode", periode, envir=globalenv())
  assign("DRIAS_serie", serie, envir=globalenv())
}
