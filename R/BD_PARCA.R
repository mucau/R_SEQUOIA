# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("R.utils")) {install.packages("R.utils")}
if (!require("lwgeom")) {install.packages("lwgeom")}

BD_PARCA <- function(PARCA=F, repBDPARCA=F){
  if(isFALSE(PARCA)) {
    rep<-tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)")
    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
    assign("PARCA", PARCA, envir=globalenv())

    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    assign("repout2", repout2, envir=globalenv())
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    assign("repout3", repout3, envir=globalenv())
  }
  if(isFALSE(repBDPARCA)) {repBDPARCA <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire du dossier'IGN BD PARCA'")}
  options(warn=-1)

  # Préparation des tempoms
  BUF2CONV <- function(x) { #Fonction de conversion des tempons en enveloppe
    CONVEX_ALL <- st_sf(st_sfc(crs=st_crs(x)))
    for (a in 1:nrow(x)){
      CONVEX <- st_convex_hull(x[a,])
      CONVEX_ALL <- rbind(CONVEX_ALL, CONVEX)
    }
    return(CONVEX_ALL)
  }

  T = 500
  TEMPON1 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T, 30)), 'POLYGON')))
  TEMPON2 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T-1, 30)), 'POLYGON')))
  TEMPON3 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T-2, 30)), 'POLYGON')))
  TEMPON4 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T*2, 30)), 'POLYGON')))
  TEMPON5 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, (T*2)-1, 30)), 'POLYGON')))
  TEMPON6 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T*4, 30)), 'POLYGON')))

# Creation de BATICA_polygon
  BATICA_polygon <- st_read(paste(repBDPARCA,"BATIMENT.SHP", sep="/"), options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T)
  if(nrow(BATICA_polygon)>0){
    BATICA_polygon <- st_sf(st_intersection(BATICA_polygon, TEMPON1), agr="constant")
    SEQUOIA:::WRITE(BATICA_polygon, repout3, paste(NAME,"BATICA_polygon.shp",sep="_"))}

# Creation de PARCELLES_polygon
  PARCELLES_polygon <- st_read(paste(repBDPARCA,"PARCELLE.SHP", sep="/"), options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T)
  if(nrow(PARCELLES_polygon)>0){
    PARCELLES_polygon <- st_sf(st_intersection(st_make_valid(PARCELLES_polygon), TEMPON4), agr="constant")
    SEQUOIA:::WRITE(PARCELLES_polygon, repout3, paste(NAME,"PARCELLES_polygon.shp",sep="_"))}

# Creation de ROAD_polygon
  ROAD_polygon <- st_intersection(PARCELLES_polygon, TEMPON4)
  ROAD_polygon <- st_intersection(st_difference(TEMPON4, st_union(ROAD_polygon)), TEMPON5)
  if(nrow(ROAD_polygon)>0){
    ROAD_polygon <- st_cast(ROAD_polygon, 'POLYGON')
    ROAD_polygon <- ROAD_polygon %>%
      mutate(TYPE = as.character(NA),
            NAME = as.character(NA)) %>%
      dplyr::select(TYPE, NAME)
    SEQUOIA:::WRITE(ROAD_polygon, repout2, paste(NAME,"ROAD_polygon.shp",sep="_"))}
  options(warn=1)
}
