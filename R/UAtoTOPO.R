# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("stringr")) {install.packages("stringr")}

UAtoTOPO <- function(rep=F) {
  message('- - - Création de TOPO_line - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de UA
  message('        Lecture des données')
  UA <- st_read(rep, options = "ENCODING=UTF-8",
                agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  cat("        Le fichier UA_polygon.shp a été chargé avec succès  \n")
  assign("UA", UA, envir=globalenv())

  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_UA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

  # Création des contours
  SEQUOIA::MNTonSHP(rep, NAME)
}
