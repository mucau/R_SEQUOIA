# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("data.table")) {install.packages("data.table")}
if (!require("dplyr")) {install.packages("dplyr")}

AURELHYonSHP <- function(shp=F, Rdata=F){
  if(isFALSE(shp)) {shp <- tk_choose.files(caption = "Choisir le fichier .shp",
                                                 filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))}
  if(isFALSE(Rdata)) {Rdata <- tk_choose.dir(caption = "Choisir le dossier contenant les données AURELHY.Rdata")}
  if (!length(shp) || !length(Rdata)){
    warning("Pas de répertoire >> Traitement annulé")
  } else {

    # Lecture des fichiers
    SHP <- st_read(shp, options = "ENCODING=UTF-8", quiet=T, agr='constant')
    SHP <- st_transform(SHP, 2154)
    load(paste(Rdata, "AURELHY.Rdata", sep="/"))


    # Création de l'emprise
    EMPRISE <- st_sf(st_as_sfc(st_bbox(st_buffer(st_combine(SHP), 2500, nQuadSegs = 30))))
    EMPRISE$AURELHY <- as.character("AURELHY")

    # Latitude
    latitude <- st_coordinates(st_transform(st_centroid(st_combine(SHP)),4326))[2]
    assign("latitude", latitude, envir=globalenv())

    # Jointure avec les données AURELHY
    AU_df <- data.frame()
    for (i in 1:length(AURELHY_list)) { # Pour chaque fichier .html de la liste
      pts <-st_sf(AURELHY_list[[i]], agr='constant')
      emp <- st_sf(st_transform(EMPRISE[,"AURELHY"],st_crs(pts)), agr='constant')
      df  <- st_intersection(pts, emp)

      if (nrow(df)<1) {
        cat(paste("Aucune correspondance entre le .shp et le fichier n°",i),"\n")
      } else {
        AU_df <- rbind(AU_df, as.data.frame(df))
        cat(paste("Correspondances entre le .shp et le fichier Aurelhy n°",i),"\n")
      }
    }

    # Sortie du tableur
    AU_df <- as.data.frame(AU_df)[,-length(AU_df)]

    # création dataP
    dataP <- transpose(AU_df[,3:14]); dataP$P <- as.numeric(1.1)
    for (a in 1:nrow(dataP)){
      dataP[a,ncol(dataP)] <- round(mean(unlist(dataP[a, 1:ncol(dataP)-1])),1)
    }
    dataP <- dataP %>%
      mutate(month=1:12) %>%
      select(month, P)

    # création dataTn
    dataTn <- transpose(AU_df[,15:26]); dataTn$Tn <- as.numeric(1.1)
    for (a in 1:nrow(dataTn)){
      dataTn[a,ncol(dataTn)] <- round(mean(unlist(dataTn[a, 1:ncol(dataTn)-1])),1)
    }
    dataTn <- dataTn %>%
      mutate(month=1:12) %>%
      select(month, Tn)

    # création dataTx
    dataTx <- transpose(AU_df[,27:38]); dataTx$Tx <- as.numeric(1.1)
    for (a in 1:nrow(dataTx)){
      dataTx[a,ncol(dataTx)] <- round(mean(unlist(dataTx[a, 1:ncol(dataTx)-1])),1)
    }
    dataTx <- dataTx %>%
      mutate(month=1:12) %>%
      select(month, Tx)

    # création dataNbjgel
    dataNbjgel <- transpose(AU_df[,39:50]); dataNbjgel$Nbjgel <- as.numeric(1.1)
    for (a in 1:nrow(dataNbjgel)){
      dataNbjgel[a,ncol(dataNbjgel)] <- round(mean(unlist(dataNbjgel[a, 1:ncol(dataNbjgel)-1])),1)
    }
    dataNbjgel <- dataNbjgel %>%
      mutate(month=1:12) %>%
      select(month, Nbjgel)

    # Création et export du tableur définitif
    AU_df <- cbind(dataP, dataTn[2], dataTx[2], dataNbjgel[2]) %>%
      mutate(Tm=Tn+Tx/2) %>%
      select(month, P, Tn, Tm, Tx, Nbjgel)
    return(AU_df)
  }
}
