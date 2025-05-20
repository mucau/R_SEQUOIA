#' @title METEOFRANCEonSHP
#' Climatologie METEOFRANCE sur une zone d'etude
#' @encoding UTF-8
#' @description 
#' La fonction \code{METEOFRANCEonSHP} télécharge les données climatologiques de la station Météo-France la plus proche du centroide de la zone détude
#' Les informations récupérées correspondantes aux normales et records disponibles pour cette station.
#' @usage METEOFRANCEonSHP(repshp)
#' @param repshp CHARACTER. Adresse du fichier \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @details 
#' La fonction retourne un dataframe des données METEOFRANCE. Ces renseignements sont fournies au mois et cumulés/moyennés à l'année.
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   METEOFRANCEonSHP(repshp = F)
#' @export
#' 
#' @import tcltk dplyr stringr sf nngeo utils

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("nngeo")) {install.packages("nngeo")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("dplyr")) {install.packages("dplyr")}

METEOFRANCEonSHP <- function(repshp = F){
  if(isFALSE(repshp)) {
    repshp <- tk_choose.files(caption = "Choisir le fichier .shp",
                                                 filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
    }
  if (!length(repshp)){
    stop("Pas de fichier sélectionné >> Traitement annulé")
  }
  
  # Lecture du fichier .shp
  SHP <- st_read(repshp, options = "ENCODING=UTF-8", quiet=T, agr='constant')
  METEOFRANCE_sf_point <- SEQUOIA::METEOFRANCE_sf_point

  # Détermination de la station Météo France la plus proche
  CENTROID_point <- st_transform(st_centroid(st_combine(SHP)),2154)
  METEOFRANCE_sf_point <- st_transform(METEOFRANCE_sf_point, 2154)
  pts <- st_nn(CENTROID_point, METEOFRANCE_sf_point, sparse = TRUE, k = 1, maxdist = Inf,
                 returnDist = FALSE, progress = TRUE)[[1]][1] # renvoie la ligne de la station la plus proche
  DISTANCE <- round(as.numeric(st_distance(CENTROID_point, METEOFRANCE_sf_point[pts,])[1,1])/1000,2)

  # Récupération des données Météo France
  ID <- as.data.frame(METEOFRANCE_sf_point[pts,"ID"])[,-2] # ID de la station
  STATION  <- as.data.frame(METEOFRANCE_sf_point[pts,"STATION"])[,-2] # Nom de la station
  ALTITUDE <- as.data.frame(METEOFRANCE_sf_point[pts,"ALTITUDE"])[,-2] # Nom de la station
  COORDS <- st_coordinates(st_transform(st_centroid(st_combine(METEOFRANCE_sf_point[pts,])),4326))

  LATITUDE  <- as.numeric(COORDS[2])
  LONGITUDE <- as.numeric(COORDS[1])

  FICHE_URL <- "https://donneespubliques.meteofrance.fr/FichesClim/FICHECLIM_"
  if(ID > 9999999){ b <- as.character("") } else  { b <- "0" }
  URL <- paste0(FICHE_URL, b, ID, ".data")
  TD = tempdir() # répertoire temporaire
  TF = tempfile(tmpdir=TD, fileext=".data") # fichier temporaire
  download.file(URL, TF, method="libcurl")
  
  # Lecture des données brutes
  CSV <- read.table(TF, header=T, sep = ";", dec=".", quote="", skip=7, fill=T, na.strings = "            ", encoding="UTF-8")
  colnames(CSV) <- c("VARIABLE","Janv.", "Févr.", "Mars",  "Avril", "Mai",   "Juin",  "Juil.", "Août",  "Sept.", "Oct.",  "Nov.",  "Déc.",  "Année", "DETAIL"  )
  
  # Traitement des données brutes
  for (i in 1:nrow(CSV)){
    if (grepl("Records", as.character(CSV[i,1]))){
      CSV[i+1,15]<-as.character(CSV[i,1])
      CSV<-CSV[-i,]
    }
    if (grepl("Statistiques", as.character(CSV[i,1]))){
      CSV[i+1,15]<-as.character(CSV[i,1])
      CSV<-CSV[-i,]
    }
    if (grepl("Tn=Température minimale. Tx=Température maximale", as.character(CSV[i,1]))){
      donnee <- as.character(CSV[i-7,1])
      CSV[(i-6):(i-1),1]<-paste0(donnee, " ", CSV[(i-6):(i-1),1])
      CSV[i-7,]<-NA
      
      CSV[(i-6):(i-1),15]<-as.character(CSV[i,1])
      CSV<-CSV[-i,]
    }
    if (grepl("Rr : Hauteur quotidienne de précipitations", as.character(CSV[i,1]))){
      donnee <- as.character(CSV[i-4,1])
      CSV[(i-3):(i-1),1]<-paste0(donnee, " ", CSV[(i-3):(i-1),1])
      CSV[i-4,]<-NA
      
      CSV[(i-3):(i-1),15]<-as.character(CSV[i,1])
      CSV<-CSV[-i,]
    }
    if (grepl("Données non disponibles", as.character(CSV[i,1]))){
      CSV[i-1,15]<-as.character(CSV[i,1])
      CSV<-CSV[-i,]
    }
    if (grepl("- : donnée manquante", as.character(CSV[i,1]))){
      CSV<-CSV[-i,]
    }
    if (grepl(">= 16 m/s", as.character(CSV[i,1]))){
      CSV[i,1]<-as.character("Nombre moyen de jours avec des rafales >= 16 m/s (>= 58 km/h)")
      CSV[i+1,1]<-as.character("Nombre moyen de jours avec des rafales >= 28 m/s (>= 10 km/h)")
      CSV[i,15]<-as.character(CSV[i,1])
      CSV[i+1,15]<-as.character(CSV[i+1,1])
    }
    if (grepl("Brouillard", as.character(CSV[i,1]))){
      CSV[i,1]<-as.character("Nombre moyen de jours de brouillard")
    }
    if (grepl("Neige", as.character(CSV[i,1]))){
      CSV[i,1]<-as.character("Nombre moyen de jours de neige")
    }
    if (grepl("Orage", as.character(CSV[i,1]))){
      CSV[i,1]<-as.character("Nombre moyen de jours d'orage")
    }
    if (grepl("Grêle", as.character(CSV[i,1]))){
      CSV[i,1]<-as.character("Nombre moyen de jours de grêle")
    }
  }
  
  CSV <- CSV %>%
    filter(!is.na(Janv.))
  
  for (i in 1:nrow(CSV)){
    if (is.na(CSV[i,1])&!is.na(CSV[i,2])){
      CSV[i,1]<-CSV[i-1,1]
      CSV[i-1,1]<-NA
    }
  }
  
  CSV <- CSV %>%
    filter(!is.na(VARIABLE))%>%
    filter(!(VARIABLE == 'Données non disponibles'))
  
  # Récupération des données statitisques
  statistiques <- as.data.frame(CSV[str_detect(CSV$VARIABLE, "statistique"), ])
  
  if (nrow(statistiques)>=2){
    STAT <- paste0(paste(statistiques[,1], collapse=". "),".")
    CSV <- CSV[1:(nrow(CSV)-nrow(statistiques)),]
  } else {
    STAT <- as.character(statistiques[1,1])
    CSV <- CSV[-nrow(CSV),]
  }
  
  #CSV <- CSV %>% filter(Janv.!="")
  CSV <- CSV[-c(27,30),]
  
  # Création du tableau de sortie
  METEOFRANCE_df <- CSV %>%
    select(VARIABLE, DETAIL, Janv.:Année)

  # Sortie du tableur
  assign("MF_ID", ID, envir=globalenv())
  assign("MF_DISTANCE", DISTANCE, envir=globalenv())
  assign("MF_STATION", STATION, envir=globalenv())
  assign("MF_ALTITUDE", ALTITUDE, envir=globalenv())
  assign("MF_LATITUDE", LATITUDE, envir=globalenv())
  assign("MF_LONGITUDE", LONGITUDE, envir=globalenv())
  assign("MF_STAT", STAT, envir=globalenv())
  assign("MF_METEOFRANCE_df", METEOFRANCE_df, envir=globalenv())
  cat("Le tableur METEOFRANCE_df a été exporté \n")
  return(METEOFRANCE_df)
}
