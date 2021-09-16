# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}

CAGEF <- function(rep=F, CODECA=1){
  message('- - - Création du fond vectoriel - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de PARCA
  message('        Lecture des données')
  PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  assign("PARCA", PARCA, envir=globalenv())
  cat('        Objet sf PARCA détecté \n \n')

  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_PARCA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

  # Téléchargement des données OSM
  message("        Téléchargement des limites administratives & des infrastructures")
  
  bdtopo <- askYesNo("Voulez-vous utiliser une IGN© BD TOPO® départementale ?")
  if(is.na(bdtopo)){break}

  if (isTRUE(bdtopo)) {
    repTOPO <- tk_choose.dir(default= getwd(),
                             caption = "Choisir le répertoire de l'IGN (c) BD TOPO (r)")
    assign("repTOPO", repTOPO, envir=globalenv())
    if (!length(repTOPO)){stop("Aucune sélection effectuée > Traitement annulé \n")}
    SEQUOIA:::BDTOPOonPARCA(rep, repTOPO)
  } else {
    SEQUOIA:::OSMonPARCA(rep)
  }

  # Téléchargement des données BDTOPO_HYDRO
  message(" \n        Téléchargement des données IGN© BD TOPO® Hydrologie")

  if (isTRUE(bdtopo)) {
    SEQUOIA:::BDTOPO_HYDRO(PARCA, "IGN© BD TOPO® Hydrographie Départementale")
  } else {
    SEQUOIA:::BDTOPO_HYDRO(PARCA, "IGN© BD TOPO® Hydrographie Métropole")
  }

  INFRA_polygon <- rbind(INFRA_polygon, HYDRO_polygon)
  INFRA_line <- rbind(INFRA_line, HYDRO_line)
  INFRA_point <- rbind(INFRA_point, HYDRO_point)

  # Import de la végétation
  message("        Import de la végétation depuis IGN© BD TOPO®")
  if (isTRUE(bdtopo)) {
    SEQUOIA:::BDTOPO_VEG(PARCA, repTOPO)
    INFRA_polygon <- rbind(INFRA_polygon, VEG_polygon)
    INFRA_line <- rbind(INFRA_line, VEG_line)
    INFRA_point <- rbind(INFRA_point, VEG_point)
  } else {
    cat("        Pas d'import de végétation \n \n")
  }

  # Sortie de INFRA_polygon & INFRA_line
  SEQUOIA:::WRITE(INFRA_polygon, repout2, paste(NAME,"INFRA_polygon.shp",sep="_"))
  SEQUOIA:::WRITE(INFRA_line, repout2, paste(NAME,"INFRA_line.shp",sep="_"))
  SEQUOIA:::WRITE(INFRA_point, repout2, paste(NAME, "INFRA_point.shp", sep="_"))

  # Création des données cadastrales
  if (CODECA==1){
    message("\n        Téléchargement des données ETALAB")
    SEQUOIA:::ETALAB(PARCA)
  }
  if (CODECA==2){
    message("\n        Chargement et export des données BD_PARCELLAIRE")
    if(!exists("repBDPARCA")) {repBDPARCA <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire du dossier'IGN BD PARCA'")}
    SEQUOIA:::BD_PARCA(PARCA, repBDPARCA)
  }
  if (CODECA==3){
    message("\n        Chargement et export des données EDIGEO")
    SEQUOIA:::EDIGEO(PARCA)
  }
  # Fin de programme
  message("\n        Fin de téléchargement")
}
