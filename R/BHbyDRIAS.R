# Lancement des library
if (!require("dplyr")) {install.packages("dplyr")}

BHbyDRIAS <- function(txt=F) {
  if(isFALSE(txt)){
    txt  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .txt de donnée",
                                   filter = matrix(c("Fichier texte", ".txt"), 1, 2, byrow = TRUE))
  }
  if(length(txt)){
    data <- read.table(file=txt, sep=";", quote="")
    if (length(data)==17) {
      colnames(data) <- c("year", "month", "day", "idPoint", "Latitude", "Longitude", "Altitude",
                          "Tn", "Tx", "Tm", "Pl", "Pn", "Ha", "Rv", "Ri", "V", "Vm")

      n <- length(unique(data$idPoint))
      years<- length(unique(data$year))
      latitude = mean(data$Latitude)

      data2 <- data %>% group_by(month) %>%
        summarize(Tn = mean(Tn), Tx = mean(Tx), Pl = sum(Pl)/(n*years), Pn = sum(Pn)/(n*years), Ha = mean(Ha),
                  Rv = mean(Rv), Ri = mean(Ri), V = mean(V), Vm = mean(Vm)) %>%
        mutate(year  = 2050,
               month = 1:12) %>%
        select(year, month, Tn, Tx, Pl, Pn, Ha, Rv, Ri, V, Vm)

      serie <- data2 %>%
        mutate(P=Pl) %>%
        select(year, month, Tn, Tx, P)

      data3 <- SEQUOIA::THORNTHWAITE(serie, latitude)

    }
    if (length(data)==14) {
      colnames(data) <- c("year", "month", "day", "idPoint", "Latitude", "Longitude", "Altitude",
                          "Tn", "Tx", "Tm", "P", "Pn", "Ha", "V")

      n <- length(unique(data$idPoint))
      years<- length(unique(data$year))
      latitude = mean(data$Latitude)

      data2 <- data %>%
        group_by(month) %>%
        summarize(Tn = mean(Tn), Tx = mean(Tx), P = sum(P)/(n*years), Pn = sum(Pn)/(n*years), Ha = mean(Ha),
                  V = mean(V)) %>%
        mutate(year  = 2050,
               month = 1:12) %>%
        select(year, month, Tn, Tx, P, Pn, Ha, V)

      serie <- data2 %>%
        select(year, month, Tn, Tx, P)

      data3 <- SEQUOIA::THORNTHWAITE(serie, latitude)
    }
    assign("DRIAS_df", serie, envir=globalenv())
    assign("DRIAS_bh", data3, envir=globalenv())
  }
}
