#' @title UAtoSSPF
#' Creation d'un ensemble de .shp a partir du .shp des unites d'analyse
#' @encoding UTF-8
#' @description 
#' La fonction \code{UAtoSSPF} génère un ensemble de .shp (EPSG 2154) nécessaires à la réalisation d'une cartographie forestière ponctuelle à partir du .shp des unités d'analyse complété (EPSG 2154).
#' La fonction peut également actualiser l'étiquetage du parcellaire de gestion.
#' @usage UAtoSSPF(rep)
#' @param rep CHARACTER. Adresse du fichier \code{.shp} UA_polygon. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{PF_polygon}{Fichier shapefile ; Parcellaire forestier}
#' \item{PF_line}{Fichier shapefile ; Contour du parcellaire forestier}
#' \item{SSPF_polygon}{Fichier shapefile ; Sous-parcellaire forestier cad parcellaire des unités de gestion}
#' \item{SSPF_line}{Fichier shapefile ; Etiquette du sous-parcellaire actualisé}
#' \item{AME_polygon}{Fichier shapefile ; OPTIONEL. Traitements forestiers cad aménagements}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'     UAtoSSPF(rep=F)
#' @export
#' 
#' @import tcltk sf lwgeom

# Lancement des library
# library(tcltk)
# library(sf)

UAtoSSPF <- function(rep=F) {
  options(warn=-1) # Désactivation des warnings
  message('- - - Création de PF_polygon, PF_line, SSPF_polygon, SSPF_line - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                            filter = matrix(c("ESRI Shapefile", "_UA_polygon.shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de UA
  message('        Lecture des données')
  NAMEofSHP(rep)
  if (isFALSE(SEQ)){stop("Répertoire SEQUOIA non detecté")}
  
  UA <- st_read(rep, options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  cat("        Le fichier UA_polygon.shp a été chargé avec succès  \n \n")
  assign("UA", UA, envir=globalenv())
  
  # write function
  write <- function(nom, rep, name){
    st_write(nom, rep, name, append=FALSE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }

  # Création du .shp "PF_polygon"
  message('        Création de PF_polygon')
  PF_polygon <- aggregate(
    x = UA["SURF_COR"],
    by = list(N_PARFOR = UA$N_PARFOR),
    FUN = sum,
    do_union = TRUE 
  )
  
  write(PF_polygon, 
        repout2, 
        paste(NAME,"PF_polygon.shp",sep="_"))

  # Création du .shp "PF_line"
  message('\n        Création de PF_line')
  
  ## Creation de PF_line
  PF_line <- LINEfromPOLYGON(PF_polygon)
  
  ## Lecture de de PROP_line et creation buffer
  PROP_line <- st_read(paste(dirname(rep),paste(NAME,"PROP_line.shp",sep="_"),sep="/"), options = "ENCODING=windows-1252", quiet=T) |>
    st_transform(2154)
  PROP_line_buffer <- st_buffer(st_union(PROP_line),0.0005)
  
  ## Supression du buffer de PF_line
  PF_line <- st_difference(PF_line, PROP_line_buffer[1])

  LIST_SHP <- list.files(dirname(rep), "*.shp")
  setwd(dirname(rep))
  
  ## Export de PF_line
  if (paste(NAME,"PF_line.shp",sep="_") %in% LIST_SHP) {
    RES <- askYesNo("Voulez-vous remplacer PF_line ?")
    if (isTRUE(RES)) {
      cat("        Remplacement de PF_line","\n \n")
      write(PF_line, 
            repout2, 
            paste(NAME,"PF_line.shp",sep="_"))
    } else {
      if(is.na(RES)){break}
      cat("        Pas de remplacement de PF_line","\n \n")
    }
  } else {
    write(PF_line, 
          repout2, 
          paste(NAME,"PF_line.shp",sep="_"))
  }

  # Création du .shp "SSPF_polygon"
  message('        Création de SSPF_polygon')
  SSPF_polygon <- aggregate(
    x = UA["SURF_COR"],
    by = list(PARFOR = UA$PARFOR),
    FUN = sum,
    do_union = TRUE 
  )
  
  SSPF_polygon <- merge(x=SSPF_polygon,
                        y=unique(as.data.frame(UA[, c("PARFOR","PLT_TYPE","PLT_ESS", "PLT_STR", "PLT_TSE")])[,1:5]),
                        by="PARFOR")

  write(SSPF_polygon, 
        repout2, 
        paste(NAME,"SSPF_polygon.shp",sep="_"))

  # Création de AME_polygon
  RES2 <- askYesNo(type = "yesno","Voulez-vous créer AME_polygon ?")
  if (isTRUE(RES2)) {
    message('\n        Création de AME_polygon')
    
    ## Creation des champs
    UA$AME_CODE <- sub("[\\. ].*", "", UA$AME_TYPE)
    UA$BLOC <- as.character(paste0(UA$PARFOR,"_",UA$AME_CODE))
    
    ## Creation du AME_polygon
    AME_polygon <- aggregate(
      x = UA["SURF_COR"],
      by = list(BLOC = UA$BLOC),
      FUN = sum,
      do_union = TRUE) |>
      transform(PARFOR = sub("([A-Z]?\\d{2}\\.\\d{2})_(.*)", "\\1", BLOC),
                AME_CODE = sub("([A-Z]?\\d{2}\\.\\d{2})_(.*)", "\\2", BLOC)) |>
      merge(unique(as.data.frame(UA[, c("PARFOR", "PLT_TYPE")])[,1:2]),
            by="PARFOR",
            all.y=F) |>
      merge(unique(as.data.frame(UA[, c("AME_CODE", "AME_TYPE")])[,1:2]),
            by="AME_CODE",
            all.y=F) |>
      select(c("BLOC", "PARFOR", "PLT_TYPE", "AME_CODE", "AME_TYPE", "SURF_COR"))

    write(AME_polygon, 
          repout2, 
          paste(NAME,"AME_polygon.shp",sep="_"))
    
  }
  
  options(warn=1)
  
}
