#' @title ETALAB
#' Telechargement des donnees ETALAB
#' @encoding UTF-8
#' @description 
#' La fonction \code{ETALAB} télécharge les données ETALAB autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires ou utiles à la réalisation d'une cartographie forestière ponctuelle.
#' @usage ETALAB(PARCA)
#' @param PARCA sf du parcellaire cadastrale. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{BATICA_polygon}{Fichier shapefile ; Batiments cadastré environnants la propriété}
#' \item{LIEUDIT_point}{Fichier shapefile ; Lieudits environnants la propriété}
#' \item{ROAD_polygon}{Fichier shapefile ; Vides cadatrés environnants la propriété : routes+tronçons fluviaux}
#' \item{PARCELLES_polygon}{Fichier shapefile ; Parcellaire cadastral global des communes}
#' \item{SUBDFISC_polygon}{Fichier shapefile ; Subdivisions fiscales}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   ETALAB(PARCA)
#' @export
#' 
#' @import tcltk sf dplyr stringr

# Lancement des library
# if (!require("sf")) {install.packages("sf")}
# if (!require("rvest")) {install.packages("rvest")}
# if (!require("R.utils")) {install.packages("R.utils")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("tcltk")) {install.packages("tcltk")}

ETALAB <- function(PARCA=F){
  if(isFALSE(PARCA)) {
    rep<-tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)")

    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  }
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

# Adresses de téléchargement et de destination
  DEP_URL <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/departements"
  COM_URL <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes"
  FEUILLES_URL <- "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/"

  DATA_LIST <- c("-batiments.json.gz",
                 "-communes.json.gz",
                 "-feuilles.json.gz",
                 "-lieux_dits.json.gz",
                 "-parcelles.json.gz",
                 "-prefixes_sections.json.gz",
                 "-sections.json.gz",
                 "-subdivisions_fiscales.json.gz")

# Sélection des communes concernées
  DEP_CODE <- unique(as.data.frame(PARCA["DEP_CODE"])[1])
  COMS_SF <- st_sf(st_sfc())
  for (a in 1:nrow(DEP_CODE)) {
    DEP <- str_pad(DEP_CODE[a,1], 2, "left", pad = "0")
    URL <- paste(DEP_URL, DEP, paste("cadastre-", DEP, DATA_LIST[2], sep=""), sep="/")

    TD = tempdir() # répertoire temporaire
    TF = tempfile(tmpdir=TD, fileext=".gz") # fichier temporaire
    download.file(URL, TF, method="libcurl", quiet=T) # Téléchargement du fichier URL sous le nom TF dans TD
    R.utils::gunzip(TF, remove=F) # Extraction de TF dans TD

    COM_SF <- st_read(str_replace(TF, ".gz", ""), quiet=T) # Lecture du fichier sf
    st_crs(COMS_SF) <- st_crs(COM_SF) # Changement du système de projection

    COMS_SF <- rbind(COMS_SF,COM_SF)
  }
  COMS_SF <- st_transform(COMS_SF, 2154)
  COMMUNE <- st_intersection(st_make_valid(COMS_SF), st_make_valid(TEMPON1))
  cat("        ", nrow(COMMUNE), ' communes ont été détectées'," \n")

# Configuration de base
  ID_CAD <- unique(as.data.frame(COMMUNE["id"])[1])

# Fonction de téléchargement
  ETALABCA <- function(ID_CAD, N) { # Debut fonction ETALABCA
    ## Création d'un SF vide
    SHPS_SF <- st_sf(st_sfc())

    ## Téléchargement des données
    for (a in 1:nrow(ID_CAD)) {
      DEP <- str_sub(ID_CAD[a,1],1,2)
      COM <- ID_CAD[a,1]
      URL <- paste(COM_URL, DEP, COM, sep="/")
      LISTE_URL <- list(html_attr(html_nodes(read_html(URL), "a"), "href"))

      if (grepl(paste("cadastre-", COM, DATA_LIST[N], sep=""), LISTE_URL)!=F) {
        URL <- paste(COM_URL, DEP, COM, paste("cadastre-", COM, DATA_LIST[N], sep=""), sep="/")

        TD = tempdir() # répertoire temporaire
        TF = tempfile(tmpdir=TD, fileext=".gz") # fichier temporaire
        download.file(URL, TF, method="libcurl", quiet=T) # Téléchargement du fichier URL sous le nom TF dans TD
        R.utils::gunzip(TF, remove=F) # Extraction de TF dans TD

        SHP_SF <- st_read(str_replace(TF, ".gz", ""), quiet=T) # Lecture du fichier sf
        st_crs(SHPS_SF) <- st_crs(SHP_SF) # Changement du système de projection

        SHPS_SF <- rbind(SHPS_SF,SHP_SF)
      }else {
        message('        >> Aucune données disponibles pour ', DATA_LIST[N], ' pour la commune ', COM)
      }
    }

    ## Export du SF
    if(nrow(SHPS_SF)>0){
      SHPS_SF <- st_sf(st_transform(SHPS_SF, 2154), agr="constant")
      return(SHPS_SF)
      cat(paste0("        La donnée", DATA_LIST[N], "a été téléchargée sur l emprise \n"))
    }
  } # Fin fonction ETALABCA

# Creation de BATICA_polygon
  BATICA_polygon <- ETALABCA(ID_CAD, N=1)
  if(nrow(BATICA_polygon)>0){
    BATICA_polygon <- st_sf(st_intersection(BATICA_polygon, TEMPON1), agr="constant")
    SEQUOIA:::WRITE(BATICA_polygon, repout3, paste(NAME,"BATICA_polygon.shp",sep="_"))}

# Creation de LIEUDIT_point
  LIEUDIT_point <- ETALABCA(ID_CAD, N=4)
  if(nrow(LIEUDIT_point)>0){
    LIEUDIT_point <-st_centroid( st_sf(st_intersection(LIEUDIT_point, TEMPON1), agr="constant"))
    SEQUOIA:::WRITE(LIEUDIT_point, repout2, paste(NAME,"LIEUDIT_point.shp",sep="_"))}

# Creation de SUBDFISC_polygon
  SUBDFISC_polygon <- ETALABCA(ID_CAD, N=8)
  if(nrow(SUBDFISC_polygon)>0){
    SUBDFISC_polygon <- st_sf(st_intersection(SUBDFISC_polygon, TEMPON1), agr="constant")
    SEQUOIA:::WRITE(SUBDFISC_polygon, repout3, paste(NAME,"SUBDFISC_polygon.shp",sep="_"))}

# Creation de PARCELLES_polygon
  PARCELLES_polygon <- ETALABCA(ID_CAD, N=5)
  if(nrow(PARCELLES_polygon)>0){
    SEQUOIA:::WRITE(PARCELLES_polygon, repout3, paste(NAME,"PARCELLES_polygon.shp",sep="_"))}

# Creation de ROAD_polygon
  ROAD_polygon <- st_intersection(PARCELLES_polygon, TEMPON4)
  ROAD_polygon <- st_intersection(st_difference(TEMPON4, st_union(ROAD_polygon)), TEMPON5)
  if(nrow(ROAD_polygon)>0){
    ROAD_polygon <- ROAD_polygon %>%
      mutate(TYPE = as.character(NA),
            NAME = as.character(NA)) %>%
      dplyr::select(TYPE, NAME)
    SEQUOIA:::WRITE(ROAD_polygon, repout2, paste(NAME,"ROAD_polygon.shp",sep="_"))}

# Creation de FEUILLES_polygon
  FEUILLES_polygon <- ETALABCA(ID_CAD, N=3)

# Import des données EDIGEO
  message("\n        Téléchargement des données EDIGEO")

  FEUILLE <- unique(as.data.frame(st_intersection(FEUILLES_polygon, PARCA))[1:10]) # Détection des feuilles concernnées
  cat("        ", paste0(nrow(FEUILLE), " feuilles cadastrales ont été détectées \n"))

  COM_CODE <- unique(as.data.frame(PARCA[c("DEP_CODE","COM_CODE")])[1:2])

  TD = tempdir() # répertoire temporaire
  for (b in 1:nrow(FEUILLE)) {
    ZIP_URL <- paste(paste(FEUILLES_URL,str_sub(FEUILLE[b,1],1,2),str_sub(FEUILLE[b,1],1,5),sep="/"),paste0("edigeo-",FEUILLE[b,1],".tar.bz2"),sep="/")

    TF = tempfile(tmpdir=TD, fileext=".tar.bz2") # fichier temporaire
    download.file(ZIP_URL, TF, method="libcurl", quiet=T) # Téléchargement du fichier URL sous le nom TF dans TD

    setwd(TD)
    bunzip2(TF)
    untar(str_replace(TF,".bz2",""))
    cat(paste0("        La feuille ", FEUILLE[b,1], "a été téléchargée \n"))
  }

# Compilation et export des données EDIGEO
    LAYER <- c("TSURF_id",
               "TLINE_id",
               "TRONFLUV_id",
               "ZONCOMMUNI_id")

    NAMES <- c("TSURF_polygon",
               "TLINE_line",
               "TRONFLUV_polygon",
               "ZONCOMMUNI_line")

    ## Liste des .THF
    LISTE_THF <- list.files(TD, "*.THF")

    for (a in 1:length(LAYER)){ # Début boucle EDIGEO
      ## Création sf vierge
      SHPS_SF <- st_sf(st_sfc())

      ## Lecture des données
      for (b in 1:length(LISTE_THF)){
        LIST_SF <- as.list(st_layers(paste(TD,LISTE_THF[b],sep="/")))[1]

        if (grepl(LAYER[a],LIST_SF)!=F) {
          SHP_SF <- st_read(paste(TD,LISTE_THF[b],sep="/"),LAYER[a], quiet=T)
          st_crs(SHPS_SF) <- st_crs(SHP_SF) # Changement du système de projection
          SHPS_SF <- rbind(SHPS_SF,SHP_SF)
        }
      }

      if(nrow(SHPS_SF)>0){
        SHPS_SF <- st_sf(st_transform(SHPS_SF, 2154), agr="constant")
        SHP <- st_intersection(SHPS_SF, TEMPON1)
        SEQUOIA:::WRITE(SHP, repout3, paste(NAME,paste0(NAMES[a],".shp"),sep="_"))
        }
      } # Fin boucle EDIGEO
    options(warn=1)
}
