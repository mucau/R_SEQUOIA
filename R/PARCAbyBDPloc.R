#' @title PARCAbyBDPloc
#' Creation d'un parcellaire cadastral .shp a partir d'une IGN (c) BD parcellaire (r) locale
#' @encoding UTF-8
#' @description 
#' La fonction \code{PARCAbyBDPloc} télécharge un parcellaire cadastral à partir d'une table cadastrale SEQUOIA
#' @usage PARCAbyBDPloc(x, bdp)
#' @param x TIBBLE. Table cadastrale SEQUOIA. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param bdp Character. Répertoire de l'IGN© BD_PARCELLAIRE®. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @return
#' \item{PARCA_bdploc_sf}{Objet sf "PARCA_bdploc_sf" exporté dans l'environnement de travail. Parcellaire cadastrale.}
#' \item{PARCA_bdploc_errors}{Objet tibble "PARCA_bdploc_errors" exporté dans l'environnement de travail. Références non trouvées.}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'    PARCAbyBDPloc(x=F, F)
#' @export
#' 
#' @import tcltk openxlsx sf

# Lancement des library
# library(tcltk)
# library(openxlsx)
# library(sf)

PARCAbyBDPloc <- function(x=F, bdp=F) {
  # Lancement de la fonction
  message('        PARCA depuis une IGN BD Parcellaire locale')
  
  # Lecture des données
  if(isFALSE(x)){
    x  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales",
                          filter = matrix(c("Excel", ".xlsx", "Excel 97-2003", ".xls"), 2, 2, byrow = TRUE))
    if (!length(x)){stop("Aucun fichier sélectionnée !")}
    x <- unique(read.xlsx(x))
  }
  cat(paste0("        ", nrow(x), " références cadastrales à chercher \n"))
  
  # Récupération de la bdp
  DEPS <- unique(x["DEP_CODE"])
  cat(paste0("        Départements à charger: ", unlist(as.list(DEPS[1]))), "\n")
  
  if(isFALSE(bdp)){
    bdp  <- tk_choose.dir(default= getwd(),
                          caption = "Choisir le répertoire de l'IGN© BD_PARCELLAIRE®")
  }
  if (is.na(bdp)){stop("Aucun dossier sélectionnée !")}
  
  shp_rep  <- list.files(bdp, "PARCELLE.SHP", recursive = T)
   
  # Lecture de la bdp
  message('\n        Lecture IGN© BD_PARCELLAIRE®')
  SHP <- st_read(paste(bdp, shp_rep, sep="/"), agr="constant", quiet=T, stringsAsFactors = F)
  cat("        L'IGN© BD_PARCELLAIRE® a été chargée \n")
  
  SHP$IDU = paste(SHP$CODE_DEP, SHP$CODE_COM, SHP$COM_ABS, SHP$SECTION, SHP$NUMERO, sep="")
  
  x$DEP_CODE <- sprintf("%02s", x$DEP_CODE)
  x$IDU <- paste(x$DEP_CODE, x$IDU, sep = "")
  cat("        L'IDU a été crée \n \n")
  
  # Sélection des parcelles
  message('        Sélection des parcelles')
  BD_PARCAS <- subset(SHP, SHP$IDU %in% x$IDU)
  
  BD_PARCA <- merge(x = BD_PARCAS[,"IDU"], y = x, by = "IDU", all.x=T, all.y=F)
  new_order <-c("REG_CODE", "REG_NOM", "DEP_CODE", "DEP_NOM", "COM_CODE", "COM_NOM", "PROP", "ADRESSE", "IDU", "PREFIXE", "SECTION", "N_PARCA", "LIEUDIT", "OCCUP_SOL", "TX_BOISE", "REV_CA", "SURF_CA")
  BD_PARCA <- unique(BD_PARCA[, new_order])
  cat(paste0("        ", nrow(BD_PARCA), " parcelles cadastrales ont été sélectionnées \n"))
  
  # Export des données
  PARCA <- st_transform(BD_PARCA, 2154)
  PARCA$IDU <- as.character(substr(PARCA$IDU, 3, nchar(PARCA$IDU)))
  assign("PARCA_bdploc_sf", PARCA, envir=globalenv())
    
  # Export des erreurs
  idu <- unlist(as.data.frame(unique(PARCA["IDU"]))[1])
  errors <- subset(x, !(x$IDU %in% idu))
  assign("PARCA_bdploc_errors", errors, envir=globalenv())
  
} # Fin fonction