#' @title PARCAbyBDPweb
#' Creation d'un parcellaire cadastral .shp a partir de l'etalab
#' @encoding UTF-8
#' @description 
#' La fonction \code{PARCAbyBDPweb} télécharge un parcellaire cadastral à partir d'une table cadastrale SEQUOIA
#' @usage PARCAbyBDPweb(x)
#' @param x TIBBLE. Table cadastrale SEQUOIA. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{PARCA_bdpweb_sf}{Objet sf "PARCA_bdpweb_sf" exporté dans l'environnement de travail. Parcellaire cadastrale.}
#' \item{PARCA_bdpweb_errors}{Objet tibble "PARCA_bdpweb_errors" exporté dans l'environnement de travail. Références non trouvées.}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'    PARCAbyBDPweb(x=F)
#' @export
#' 
#' @import tcltk openxlsx sf happign 

# Lancement des library
# library(tcltk)
# library(openxlsx)
# library(sf)
# library(happign)

PARCAbyBDPweb <- function(x=F){ # function
  is_empty <- function(x){identical(nrow(x),0L)}
  
  # Lancement de la fonction
  message('        PARCA depuis IGN BD Parcellaire en web service')
  
  # Lecture des données
  if(isFALSE(x)){
    x  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales",
                            filter = matrix(c("Excel", ".xlsx", "Excel 97-2003", ".xls"), 2, 2, byrow = TRUE))
    if (!length(x)){stop("Aucun fichier sélectionnée !")}
    x <- unique(read.xlsx(x))
  }
  cat(paste0("        ", nrow(x), " références cadastrales à chercher \n"))
  
  # Téléchargement des données
  message('\n        Téléchargement des données web service')
  x$DEP_CODE <- sprintf("%02s", x$DEP_CODE)
  x$COM_CODE <- sprintf("%03s", x$COM_CODE)
  x$ID_CAD <- paste(x$DEP_CODE, x$COM_CODE, sep = "")
  ID_CAD <- unlist(unique(x["ID_CAD"]))
  
  PARCELLES_SF <- st_sf(st_sfc())
  for (a in 1:length(ID_CAD)) { # téléchargement
    insee= as.character(ID_CAD[a])
    invisible(capture.output(suppressMessages(PARCELLE_SF <- get_apicarto_cadastre(insee, type="parcelle", source = "BDP"))))
    
    if (is_empty(PARCELLE_SF)){
      cat("        La commune", insee, "n'a pas été téléchargée \n")
    } else {
      st_crs(PARCELLES_SF) <- st_crs(PARCELLE_SF)
      PARCELLES_SF <- rbind(PARCELLES_SF,PARCELLE_SF)
      cat("        La commune", insee, "a été téléchargée \n")
    }
  } # end téléchargement
  
  # Sélection des parcelles
  message('\n        Sélection des parcelles')
  PARCELLES_SF <- st_sf(st_transform(PARCELLES_SF , 2154), agr="constant")
  PARCELLES_SF$IDU <- as.character(paste0(PARCELLES_SF$code_com, 
                                          PARCELLES_SF$com_abs, 
                                          PARCELLES_SF$section, 
                                          PARCELLES_SF$numero))
  BD_PARCA <- subset(PARCELLES_SF, PARCELLES_SF$IDU %in% x$IDU)
    
  BD_PARCA <- merge(x = BD_PARCA, y = x, by = "IDU")
  new_order <-c("REG_CODE", "REG_NOM", "DEP_CODE", "DEP_NOM", "COM_CODE", "COM_NOM", "PROP", "ADRESSE", "IDU", "PREFIXE", "SECTION", "N_PARCA", "LIEUDIT", "OCCUP_SOL", "TX_BOISE", "REV_CA", "SURF_CA")
  BD_PARCA <- BD_PARCA[, new_order]
  cat(paste0("        ", nrow(BD_PARCA), " parcelles cadastrales ont été sélectionnées \n"))
  
  # Export des données
  PARCA <- st_transform(BD_PARCA, 2154)
  assign("PARCA_bdpweb_sf", PARCA, envir=globalenv())
    
  # Export des erreurs
  idu <- unlist(as.data.frame(unique(PARCA["IDU"]))[1])
  errors <- subset(x, !(x$IDU %in% idu))
  assign("PARCA_bdpweb_errors", errors, envir=globalenv())
  
} # end function

