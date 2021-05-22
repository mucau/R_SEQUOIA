# Lancement des library
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}

BHbyDRIAS <- function(txt=F) {
  if(isFALSE(txt)){
    txt  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .txt de donnée",
                                   filter = matrix(c("Fichier texte", ".txt"), 1, 2, byrow = TRUE))
  }
  if (!length(txt)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  data <- read.table(file=txt, sep=";", quote="")
  colnames(data) <- c("Point", "Latitude", "Longitude", "Contexte", "Periode", "Mois", "NORTAV", "NORTNAV", "NORTXAV", "NORSD", "NORTX35", "NORTXND", "NORTNHT", "NORTR", "NORTNFD", "NORTNND", "NORTXFD", "NORTNCWD", "NORTXHWD", "NORTRAV", "NORTXQ90", "NORTXQ10", "NORTNQ10", "NORTNQ90", "NORHDD", "NORCDD", "NORPAV", "NORRR", "NORRR1MM", "NORPN20MM", "NORPFL90", "NORPXCWD", "NORPXCDD", "NORPINT", "NORHUSAV", "ATAV", "ATNAV", "ATXAV", "ASD", "ATX35", "ATXND", "ATNHT", "ATR", "ATNFD", "ATNND", "ATXFD", "ATNCWD", "ATXHWD", "ATRAV", "ATXQ90", "ATXQ10", "ATNQ10", "ATNQ90", "AHDD", "ACDD", "APAV", "ARR", "ARR1MM", "APN20MM", "APFL90", "APXCWD", "APXCDD", "APINT", "AFFAV", "AFF3", "AHUSAV")
  
  #Sélection de la période
  form <- c("H1","H2", "H3")
  
  Res <- select.list(form, multiple = F,
                     title = "Quelle période ?",
                     graphics = T)
  if (!length(Res)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  data2 <- data %>% 
    select(Point, Latitude, Periode, Mois, NORTAV, NORTNAV, NORTXAV, NORRR) %>%
    filter(Periode %in% Res)
  
  n <- length(unique(data$Point))
  years<- length(unique(data$Periode))
  latitude = mean(data$Latitude)
  
  serie <- data2 %>% group_by(Mois) %>%
    summarize(Tm = mean (NORTAV), Tn = mean(NORTNAV), Tx = mean(NORTXAV), 
              Pl = mean(NORRR))%>%
    mutate(year = 2050, month=Mois) %>%
    select(year, month, Tn, Tx, Pl)
  
  data3 <- SEQUOIA::THORNTHWAITE(serie, latitude)%>%mutate(Annee=Res)
  
  assign("Periode", Res, envir=globalenv())
  assign("DRIAS_serie", serie, envir=globalenv())
  assign("DRIAS_df", data, envir=globalenv())
  assign("DRIAS_bh", data3, envir=globalenv())
}
