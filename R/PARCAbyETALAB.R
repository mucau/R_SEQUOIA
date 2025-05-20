#' @title PARCAbyETALAB
#' Creation d'un parcellaire cadastral .shp a partir du PCI Etalab from cadastre.data.gouv.fr
#' @encoding UTF-8
#' @description 
#' La fonction \code{PARCAbyETALAB} télécharge un parcellaire cadastral à partir d'une table cadastrale SEQUOIA
#' @usage PARCAbyETALAB(x)
#' @param x TIBBLE. Table cadastrale SEQUOIA. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{PARCA_etalab_sf}{Objet sf "PARCA_etalab_sf" exporté dans l'environnement de travail. Parcellaire cadastrale.}
#' \item{PARCA_etalab_errors}{Objet tibble "PARCA_etalab_errors" exporté dans l'environnement de travail. Références non trouvées.}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'    PARCAbyETALAB(x=F)
#' @export
#' 
#' @import tcltk openxlsx sf xml2

# Lancement des library
# library(tcltk)
# library(openxlsx)
# library(sf)
# library(xml2)

PARCAbyETALAB <- function(x=F){ # function
  # Lancement de la fonction
  message('        PARCA depuis ETALAB')
  
  # Lecture des données
  if(isFALSE(x)){
    x  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales",
                          filter = matrix(c("Excel", ".xlsx", "Excel 97-2003", ".xls"), 2, 2, byrow = TRUE))
    if (!length(x)){stop("Aucun fichier sélectionnée !")}
    x <- unique(read.xlsx(x))
  }
  cat(paste0("        ", nrow(x), " références cadastrales à chercher \n"))
  
  # Téléchargement des données
  message('\n        Téléchargement des données ETALAB')
  x$DEP_CODE <- sprintf("%02s", x$DEP_CODE)
  x$COM_CODE <- sprintf("%03s", x$COM_CODE)
  x$ID_CAD <- paste(x$DEP_CODE, x$COM_CODE, sep = "")
  ID_CAD <- as.data.frame(unique(x["ID_CAD"]))
  
  PARCELLES_SF <- st_sf(st_sfc())
  DEP_ERR <- list()
  
  for (a in 1:nrow(ID_CAD)) { # Boucle de téléchargement
    departement <- substr(ID_CAD[a, 1], 1, 2)
    commune <- ID_CAD[a,1]
    
    dep_url <- paste("https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes", departement, sep="/")
    page <- read_html(dep_url)
    links <- xml_find_all(page, ".//a")
    urls <- xml_attr(links, "href")
    urls <- urls[!is.na(urls) & urls != ""]
    com_url <- urls[grepl(commune, urls)]
    
    if (!(length(com_url) == 0)) {
      url <- paste(dep_url, commune, paste("cadastre-", commune, "-parcelles.json.gz", sep=""), sep="/")
      TD = tempdir()
      TF = tempfile(tmpdir=TD, fileext=".gz")
      download.file(url, TF, method="libcurl", quiet=T)
      R.utils::gunzip(TF, remove=F)
      
      PARCELLE_SF <- st_read(gsub("\\.gz$", "", TF), quiet=T)
      st_crs(PARCELLES_SF) <- st_crs(PARCELLE_SF)
      
      PARCELLES_SF <- rbind(PARCELLES_SF,PARCELLE_SF)
      cat("        La commune", commune, "a été téléchargée \n")
    }else {
      message("        La commune ", commune, " n'est pas disponible")
    } 
  } # Fin de boucle de téléchargement
  
  # Sélection des parcelles
  message('\n        Sélection des parcelles')
  if(nrow(PARCELLES_SF )>0){
    PARCELLES_SF<- st_sf(st_transform(PARCELLES_SF , 2154), agr="constant")
    PARCELLES_SF$IDU <- substring(PARCELLES_SF$id, 3, 14)
    DATA_PARCAS <- subset(PARCELLES_SF, PARCELLES_SF$IDU %in% x$IDU)
    
    DATA_PARCAS <- merge(x = DATA_PARCAS, y = x, by = "IDU")
    DATA_PARCAS$SURF_CA = DATA_PARCAS$contenance/10000
      new_order <-c("REG_CODE", "REG_NOM", "DEP_CODE", "DEP_NOM", "COM_CODE", "COM_NOM", "PROP", "ADRESSE", "IDU", "PREFIXE", "SECTION", "N_PARCA", "LIEUDIT", "OCCUP_SOL", "TX_BOISE", "REV_CA", "SURF_CA")
    ET_PARCA <- st_transform(unique(DATA_PARCAS[, new_order]), 2154)
    cat(paste0("        ", nrow(ET_PARCA), " parcelles cadastrales ont été sélectionnées \n"))
    
  } # Sélection
  
  # Export des données
  PARCA <- st_transform(ET_PARCA, 2154)
  assign("PARCA_etalab_sf", PARCA, envir=globalenv())
  
  # Export des erreurs
  idu <- unlist(as.data.frame(unique(PARCA["IDU"]))[1])
  errors <- subset(x, !(x$IDU %in% idu))
  assign("PARCA_etalab_errors", errors, envir=globalenv())
  
} # Fin fonction
  