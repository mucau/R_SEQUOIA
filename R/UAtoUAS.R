#' @title UAtoUAS
#' Compilation des fichiers SEQUOIA
#' @encoding UTF-8
#' @description La fonction \code{UAtoUAS} permet de compiler des shapefiles SEQUOIA d'un répertoire donné
#' @usage UAtoUAS(rep)
#' @param rep CHARACTER. Chemin du dossier contenant les dossiers SIG. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @param data CHARACTER. Type de donnée SEQUOIA (Ex: UA). Si \code{FALSE}, la fonction génère une boite de dialogue 
#' @return
#' \item{data_polygon}{Fichier shapefile ; donnée SEQUOIA compilées}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     UAtoUAS(rep=F)
#' @export
#' 
#' @import tcltk sf  

# Lancement des library
# library(tcltk)
# library(sf)
# library(openxlsx)

UAtoUAS <- function(rep=F, data=F){
  message('- - - Compilation de shapefile SEQUOIA - - -')
  # Choix du répertoire
  if(isFALSE(rep)){
    rep  <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire SEQUOIA à compiler")
  }
  if (is.na(rep)){stop("Aucun fichier sélectionnée !")}
  
  # Choix de la couche SEQUOIA
  if(isFALSE(data)){
    if (Sys.info()["sysname"]=="Windows"){
      data <- utils::winDialogString("Entrer le type de donnée SEQUOIA à compiler:", "")
    }else {
      data <- readline(prompt="Entrer le type de donnée SEQUOIA à compiler:")
    }
  } 
  if(is.null(data)|is.na(data)|data==""){stop("Aucune donnée saisie !")}
  
  # write function
  write <- function(nom, rep, name){
    st_write(nom, rep, name, append=FALSE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }

  # détection des données
  datafinded <- paste0(data, "_polygon.shp")
  liste_sfs <- list.files(rep, datafinded, recursive=T)
  cat(paste0("        ", length(liste_sfs), " fichiers détectés \n"))
  
  # compilation des données
  sfs <- st_sf(st_sfc())
  st_crs(sfs)<- 2154
  
  for (a in 1:length(liste_sfs)) {
    cat("Traitement du fichier ",a, "/", length(liste_sfs)," : ", liste_sfs[a],  "\n")
    
    ## Detection des fichiers
    file <- paste(rep, liste_sfs[a], sep="/")
    NAMEofSHP(file)
    
    ## Actualisation si UA
    if (data=="UA"){UAtoUA(file)}
    
    ## Lecture
    sf <- st_read(file, stringsAsFactors=F, options = "ENCODING=UTF-8", quiet=T)
    sf$ADRESSE = NAME
    sfs <- unique(rbind(sfs, sf))
  }
  
  # Creation du nouveau prefixe 
  if (Sys.info()["sysname"]=="Windows"){
    newname <- utils::winDialogString("Entrer le nom de la forêt (préfixe) :", "")
  }else {
    newname <- readline(prompt="Entrer le nom de la forêt (préfixe) :")
  }
  
  # Export du shapefile
  write(sfs, 
        rep, 
        paste0(newname, "_", data, "_polygon.shp"))
  
  # Export de la table
  sortie <- as.data.frame(sfs)[,-ncol(sfs)]
  repout <- paste(rep, paste0(newname,"_", data, ".xlsx"),sep="/")
  write.xlsx(sortie, repout)
  cat(paste("        Le tableur a été enregistré dans le repertoire : ", repout),"\n \n")
}







