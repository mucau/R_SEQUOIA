#' @title CADbyETALAB
#' Telechargement des donnees cadastrale a partir du PCI Etalab from cadastre.data.gouv.fr
#' @encoding UTF-8
#' @description 
#' La fonction \code{CADbyETALAB} télécharge les données ETALAB autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires ou utiles à la réalisation d'une cartographie forestière ponctuelle.
#' @usage CADbyETALAB(rep)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @return
#' \item{BATICA_polygon}{Fichier shapefile ; Batiments cadastré environnants la propriété}
#' \item{LIEUDIT_point}{Fichier shapefile ; Lieudits environnants la propriété}
#' \item{ROAD_polygon}{Fichier shapefile ; Vides cadatrés environnants la propriété : routes+tronçons fluviaux}
#' \item{PARCELLES_polygon}{Fichier shapefile ; Parcellaire cadastral global des communes}
#' \item{SUBDFISC_polygon}{Fichier shapefile ; Subdivisions fiscales}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   CADbyETALAB(rep=F)
#' @export
#' 
#' @import tcltk sf xml2

# Lancement des library
# library(tcltk)
# library(sf)
# library(xml2)

CADbyETALAB <- function(rep=F){
  # Lecture du shapefile
  message('        Lecture des données')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  NAMEofSHP(rep)
  PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)
  cat("        Le fichier .shp a été chargé avec succès  \n")
  assign("PARCA", PARCA, envir=globalenv())
  
  # Fonction de conversion des tempons en enveloppe
  BUF2CONV <- function(x, T) { 
    SHP <- st_sf(st_cast(st_union(st_buffer(x, T, 30)), 'POLYGON'))
    CONVEX_ALL <- st_sf(st_sfc(crs=st_crs(SHP)))
    for (a in 1:nrow(SHP)){
      CONVEX <- st_convex_hull(SHP[a,])
      CONVEX_ALL <- st_make_valid(rbind(CONVEX_ALL, CONVEX))
    }
    return(CONVEX_ALL)
  }
  
  # Creation des tempons
  T = 500
  TEMPON1 <- BUF2CONV(PARCA, T)
  TEMPON2 <- BUF2CONV(PARCA, T-1)
  TEMPON3 <- BUF2CONV(PARCA, T-2)
  TEMPON4 <- BUF2CONV(PARCA, T*2)
  TEMPON5 <- BUF2CONV(PARCA, T*4)
  
  # write function
  write <- function(nom, rep, name=NULL){
    if (is.null(name)){
      name <- paste0(NAME, "_", deparse(substitute(nom)), ".shp")
    } else {
      name <-  paste0(NAME, "_", name, ".shp")
    }
    st_write(nom, rep, name, append=FALSE, delete_layer=TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }
  
  # Fichiers à télécharger
  DATA_LIST <- c("-batiments.json.gz",
                 "-communes.json.gz",
                 "-feuilles.json.gz",
                 "-lieux_dits.json.gz",
                 "-parcelles.json.gz",
                 "-prefixes_sections.json.gz",
                 "-sections.json.gz",
                 "-subdivisions_fiscales.json.gz")
  
  # Sélection des communes concernées
  dep_url <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/departements"
  dep_code <- unique(as.data.frame(PARCA["DEP_CODE"])[1])
  
  COMS_SF <- st_sf(st_sfc())
  
  for (a in 1:nrow(dep_code)) {
    departement <- sprintf("%02d", as.numeric(dep_code[a,1]))
    url <- paste(dep_url, departement, paste("cadastre-", departement, DATA_LIST[2], sep=""), sep="/")
    
    TD = tempdir()
    TF = tempfile(tmpdir=TD, fileext=".gz")
    utils::download.file(url, TF, method="libcurl", quiet=T)
    R.utils::gunzip(TF, remove=F)
    
    COM_SF <- st_read(gsub(".gz", "", TF), quiet=TRUE)
    st_crs(COMS_SF) <- st_crs(COM_SF)
    
    COMS_SF <- rbind(COMS_SF, COM_SF)
  }
  COMS_SF <- st_transform(COMS_SF, 2154)
  COMMUNE <- st_intersection(st_make_valid(COMS_SF), st_make_valid(TEMPON1))
  ID_CAD <- unique(as.data.frame(COMMUNE["id"])[1])
  cat("        ", nrow(COMMUNE), ' communes ont été détectées'," \n")
  
  # Fonction de téléchargement
  ETALABCA <- function(ID_CAD, N) { # Debut fonction ETALABCA
    ## Création d'un SF vide
    SHPS_SF <- st_sf(st_sfc())
    
    ## Téléchargement des données
    for (a in 1:nrow(ID_CAD)) {
      departement <- substr(ID_CAD[a, 1], 1, 2)
      commune <- ID_CAD[a,1]
      
      dep_url <- paste("https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes", departement, sep="/")
      page <- read_html(dep_url)
      links <- xml_find_all(page, ".//a")
      urls <- xml_attr(links, "href")
      urls <- urls[!is.na(urls) & urls != ""]
      com_url <- urls[grepl(commune, urls)]
      
      if (!(length(com_url) == 0)) {
        com_url <- paste("https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes", departement, com_url, sep="/")
        page <- read_html(com_url)
        links <- xml_find_all(page, ".//a")
        urls <- xml_attr(links, "href")
        urls <- urls[!is.na(urls) & urls != ""]
        data_url <- urls[grepl(DATA_LIST[N], urls)]
        
        if (!(length(data_url) == 0)) {
          url <- paste(dep_url, commune, paste("cadastre-", commune, DATA_LIST[N], sep=""), sep="/")
          TD = tempdir()
          TF = tempfile(tmpdir=TD, fileext=".gz")
          download.file(url, TF, method="libcurl", quiet=T)
          R.utils::gunzip(TF, remove=F)
          
          SHP_SF <- st_read(gsub(".gz", "", TF), quiet=T)
          st_crs(SHPS_SF) <- st_crs(SHP_SF)
          
          SHPS_SF <- rbind(SHPS_SF,SHP_SF)
        } else {
          message('        >> Aucune données disponibles pour ', DATA_LIST[N], ' pour la commune ', commune)
        }
      } else {
        message('        >> Aucune données disponibles pour la commune ', commune)
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
    write(BATICA_polygon, repout2)
  }
  
  # Creation de LIEUDIT_point
  LIEUDIT_point <- ETALABCA(ID_CAD, N=4)
  if(nrow(LIEUDIT_point)>0){
    LIEUDIT_point <-st_centroid( st_sf(st_intersection(LIEUDIT_point, TEMPON1), agr="constant"))
    write(LIEUDIT_point, repout2)
  }
  
  # Creation de SUBDFISC_polygon
  SUBDFISC_polygon <- ETALABCA(ID_CAD, N=8)
  if(!is.null(SUBDFISC_polygon)){
    SUBDFISC_polygon <- st_sf(st_intersection(SUBDFISC_polygon, TEMPON1), agr="constant")
    write(SUBDFISC_polygon, repout2)
  }
  
  # Creation de PARCELLES_polygon
  PARCELLES_polygon <- ETALABCA(ID_CAD, N=5)
  if(nrow(PARCELLES_polygon)>0){
    write(PARCELLES_polygon, repout2)
  }
  
  # Creation de ROAD_polygon
  ROAD_polygon <- st_intersection(PARCELLES_polygon, TEMPON4)
  ROAD_polygon <- st_intersection(st_difference(TEMPON4, st_union(ROAD_polygon)), TEMPON5)
  if(nrow(ROAD_polygon)>0){
    ROAD_polygon$TYPE = as.character(NA)
    ROAD_polygon$NAME = as.character(NA)
    ROAD_polygon<-ROAD_polygon[,c("TYPE", "NAME")]
    write(ROAD_polygon, repout2)
  }
  
  # Creation de FEUILLES_polygon
  FEUILLES_polygon <- ETALABCA(ID_CAD, N=3)
  
  # Import des données EDIGEO
  message("\n        Téléchargement des données EDIGEO")
  
  FEUILLE <- unique(as.data.frame(st_intersection(FEUILLES_polygon, PARCA))[1:10]) # Détection des feuilles concernnées
  cat("        ", paste0(nrow(FEUILLE), " feuilles cadastrales ont été détectées \n"))
  
  COM_CODE <- unique(as.data.frame(PARCA[c("DEP_CODE","COM_CODE")])[1:2])
  
  ## Téléchargement des données
  FEUILLES_URL <- "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/2024-10-01/edigeo/feuilles/"
  TD = tempdir() 
  for (b in 1:nrow(FEUILLE)) {
    ZIP_URL <- paste(paste(FEUILLES_URL,
                           substr(FEUILLE[b, 1], 1, 2),
                           substr(FEUILLE[b, 1], 1, 5),sep="/"),
                     paste0("edigeo-",FEUILLE[b,1],".tar.bz2"),sep="/")
    
    TF = tempfile(tmpdir=TD, fileext=".tar.bz2")
    download.file(ZIP_URL, TF, method="libcurl", quiet=T)
    
    setwd(TD)
    R.utils::bunzip2(TF)
    utils::untar(gsub(".bz2", "", TF))
    cat(paste0("        La feuille ", FEUILLE[b,1], "a été téléchargée \n"))
  }
  
  ## Compilation et export des données EDIGEO
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
        st_crs(SHPS_SF) <- st_crs(SHP_SF)
        SHPS_SF <- rbind(SHPS_SF,SHP_SF)
      }
    }
    
    if(nrow(SHPS_SF)>0){
      SHPS_SF <- st_sf(st_transform(SHPS_SF, 2154), agr="constant")
      SHP <- st_intersection(SHPS_SF, TEMPON1)
      
      write(SHP, repout2, NAMES[a])
    }
  } # Fin boucle EDIGEO
  
}

