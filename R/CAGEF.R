#' @title CAGEF
#' Creation de donnees pour la realisation d'un fond vectoriel
#' @encoding UTF-8
#' @description 
#' La fonction \code{CAGEF} utilise les fonctions \code{OSMonPARCA}, \code{BDTOPOonPARCA}, \code{BDTOPO_HYDRO}, \code{BDTOPO_VEG} et \code{ETALAB}/\code{BD_PARCA}/\code{EDIGEO} pour générer un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
#' @usage CAGEF(rep, CODECA)
#' @param rep Character. Répertoire du shapefile. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du shapefile.
#' @param source_bdt Character. BD Topo utilisée. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection de la source.
#' @param source_cadastre Character. Source du casdastre utilisée. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection de la source.
#' @seealso
#' Les fonctions utilisées:
#' \code{\link{OSMonPARCA}}, \code{\link{BDTOPOonPARCA}}, \code{\link{BDTOPO_HYDRO}}, \code{\link{BDTOPO_VEG}}, \code{\link{ETALAB}}, \code{\link{BD_PARCA}}, \code{\link{EDIGEO}}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   CAGEF(rep=F, source_bdt=F, source_cadastre=F)
#' @export
#' 
#' @import tcltk sf

# Lancement des library
# library(tcltk)
# library(sf)

CAGEF <- function(rep=F, source_bdt=F, source_cadastre=F){
  options(warn=-1)
  message('- - - Création du fond vectoriel - - -')
  
  # Lecture du shapefile
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  message("\n        Choix de la source pour les données infrastructures")
  if(isFALSE(source_bdt)){
    form <- c("1 web IGN© BD TOPO®",
              "2 loc IGN© BD TOPO®")
    
    source_bdt <- select.list(form,
                        multiple = F,
                        title = "Choix de la source cadastrale",
                        graphics = T)
    }
  if (source_bdt==""){stop("Aucune source sélectionnée !")}
  
  cat(paste0("        ", source_bdt, " retenu \n"))
  
  message("\n        Téléchargement des limites administratives & des infrastructures")
  if ("1 web IGN© BD TOPO®" %in% source_bdt){
    INFRAbyBDTweb(rep)
  }
  
  if ("2 loc IGN© BD TOPO®" %in% source_bdt){
    bdt <- tk_choose.dir(default= getwd(),
                          caption = "Choisir le répertoire de l'IGN (c) BD TOPO (r)")
    if (is.na(bdt)){stop("Aucune sélection effectuée > Traitement annulé \n")}
    
    INFRAbyBDTloc(rep, bdt)
  }

  # Téléchargement des données BDTOPO_HYDRO
  message(" \n        Téléchargement des données IGN© BD TOPO® Hydrologie")
  if ("1 web IGN© BD TOPO®" %in% source_bdt){
    HYDRObyBDTweb(rep)
  }
  
  if ("2 loc IGN© BD TOPO®" %in% source_bdt){
    HYDRObyBDTloc(rep, bdt)
  }
  
  if(exists("HYDRO_polygon")){  INFRA_polygon <- unique(rbind(INFRA_polygon, HYDRO_polygon))}
  if(exists("HYDRO_line")){     INFRA_line    <- unique(rbind(INFRA_line, HYDRO_line))}
  if(exists("HYDRO_point")){    INFRA_point   <- unique(rbind(INFRA_point, HYDRO_point))}

  # Import de la végétation
  message("        Import de la végétation depuis IGN© BD TOPO®")
  if ("1 web IGN© BD TOPO®" %in% source_bdt){
    VEGbyBDTweb(rep)
  }
  
  if ("2 loc IGN© BD TOPO®" %in% source_bdt){
    VEGbyBDTloc(rep, bdt)
  }
  
  if(exists("VEG_polygon")){    INFRA_polygon <- rbind(INFRA_polygon, VEG_polygon)}
  if(exists("VEG_line")){       INFRA_line    <- rbind(INFRA_line, VEG_line)}
  if(exists("VEG_point")){      INFRA_point   <- rbind(INFRA_point, VEG_point)}

  # Export de INFRA_polygon & INFRA_line
  write <- function(nom, rep, name){
    st_write(nom, rep, name, append=FALSE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }
  
  write(INFRA_polygon, repout2, paste(NAME,"INFRA_polygon.shp",sep="_"))
  write(INFRA_line, repout2, paste(NAME,"INFRA_line.shp",sep="_"))
  write(INFRA_point, repout2, paste(NAME, "INFRA_point.shp", sep="_"))
  
  message("\n        Choix de la source pour les données cadastrales")
  if(isFALSE(source_cadastre)){
    form <- c("1 web IGN© BD Parcellaire®",
              "2 loc IGN© BD Parcellaire®",
              "3 web PCI Etalab from cadastre.data.gouv.fr")
    
    source_cadastre <- select.list(form,
                                   multiple = F,
                                   title = "Choix de la source cadastrale",
                                   graphics = T)
  }
  if (source_cadastre==""){stop("Aucune source sélectionnée !")}
  
  if ("1 web IGN© BD Parcellaire®" %in% source_cadastre) {
    CADbyBDPweb(rep)
  }
  
  if ("2 loc IGN© BD Parcellaire®" %in% source_cadastre) {
    CADbyBDPloc(rep, F)
  }
  
  if ("3 web PCI Etalab from cadastre.data.gouv.fr" %in% source_cadastre) {
    CADbyETALAB(rep)
  } 
  
  
  # Fin de programme
  message("\n        Fin de téléchargement")
  options(warn=1)
}
