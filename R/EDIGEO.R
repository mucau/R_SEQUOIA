# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("rvest")) {install.packages("rvest")}
if (!require("R.utils")) {install.packages("R.utils")}
if (!require("stringr")) {install.packages("stringr")}


EDIGEO <- function(PARCA, rep_EDIGEO){

# Préparation des tempoms
  BUF2CONV <- function(x) { #Fonction de conversion des tempons en enveloppe
    CONVEX_ALL <- st_sf(st_sfc(crs=st_crs(x)))
    for (a in 1:nrow(x)){
      CONVEX <- st_convex_hull(x[a,])
      CONVEX_ALL <- rbind(CONVEX_ALL, CONVEX)
    }
    return(CONVEX_ALL)
  }
  options(warn=-1)
  T = 500
  TEMPON1 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T, 30)), 'POLYGON')))
  TEMPON2 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T-1, 30)), 'POLYGON')))
  TEMPON3 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T-2, 30)), 'POLYGON')))
  TEMPON4 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T*2, 30)), 'POLYGON')))
  TEMPON5 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, (T*2)-1, 30)), 'POLYGON')))
  TEMPON6 <- BUF2CONV(st_sf(st_cast(st_union(st_buffer(PARCA, T*4, 30)), 'POLYGON')))

# Données de base
  LAYER <- c("PARCELLE_id",
             "LIEUDIT_id_LABEL",
             "BATIMENT_id",
             "TSURF_id",
             "TLINE_id",
             "TRONFLUV_id",
             "ZONCOMMUNI_id")

  NAMES <- c("PARCELLES_polygon",
             "LIEUDIT_point",
             "BATICA_polygon",
             "TSURF_polygone",
             "TLINE_line",
             "TRONFLUV_polygone",
             "ZONCOMMUNI_line")

  ## Liste des .THF
  LISTE_THF <- list.files(rep_EDIGEO, "*.THF", recursive=T)

  for (a in 1:length(LAYER)){ # Début boucle EDIGEO
    ## Création sf vierge
    SHPS_SF <- st_sf(st_sfc())

    ## Lecture des données
    for (b in 1:length(LISTE_THF)){
      LIST_SF <- as.list(st_layers(paste(rep_EDIGEO,LISTE_THF[b],sep="/")))[1]

      if (grepl(LAYER[a],LIST_SF)!=F) {
        SHP_SF <- st_read(paste(rep_EDIGEO,LISTE_THF[b],sep="/"),LAYER[a], quiet=T)
        st_crs(SHPS_SF) <- st_crs(SHP_SF) # Changement du système de projection
        SHPS_SF <- rbind(SHPS_SF,SHP_SF)
      }
    }

    if(nrow(SHPS_SF)>0){
      SHPS_SF <- st_sf(st_transform(SHPS_SF, 2154), agr="constant")

      if (a<2){
        ROAD_polygon <- st_intersection(SHPS_SF, TEMPON4)
        ROAD_polygon <- st_intersection(st_difference(TEMPON4, st_union(ROAD_polygon)), TEMPON5)
        if(nrow(ROAD_polygon)>0){
          ROAD_polygon <- ROAD_polygon %>%
            mutate(TYPE = as.character(NA),
                  NAME = as.character(NA)) %>%
            dplyr::select(TYPE, NAME)
          SEQUOIA:::WRITE(ROAD_polygon, repout2, paste(NAME,"ROAD_polygon.shp",sep="_"))
        }
        SHP <- st_intersection(SHPS_SF, TEMPON1)
        SEQUOIA:::WRITE(SHP, repout3, paste(NAME,"PARCELLES_polygon.shp",sep="_"))
      } else {
        SHP <- st_intersection(SHPS_SF, TEMPON1)
        if (a>2){
          SEQUOIA:::WRITE(SHP, repout3, paste(NAME,paste0(NAMES[a],".shp"),sep="_"))
        } else {
          SEQUOIA:::WRITE(SHP, repout2, paste(NAME,paste0(NAMES[a],".shp"),sep="_"))
        }
      }
    }
  } # Fin boucle EDIGEO
  options(warn=1)
}
