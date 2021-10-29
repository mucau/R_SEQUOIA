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
#' @import tcltk dplyr stringr sf lwgeom

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("lwgeom")) {install.packages("lwgeom")}

UAtoSSPF <- function(rep=F) {
  options(warn=-1) # Désactivation des warnings
  message('- - - Création de PF_polygon, PF_line, SSPF_polygon, SSPF_line - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de UA
  message('        Lecture des données')
  SHP <- st_read(rep, options = "ENCODING=UTF-8",
                agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  cat("        Le fichier UA_polygon.shp a été chargé avec succès  \n \n")
  assign("UA", SHP, envir=globalenv())

  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_UA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

  # Création du .shp "PF_polygon"
  message('        Création de PF_polygon')
  PF_POLY <- aggregate(x = SHP[, "SURF_COR"], by = list(SHP$N_PARFOR),
                       FUN = sum, na.rm = TRUE)
  names(PF_POLY) <- c("N_PARFOR","SURF_COR","geometry")

  SEQUOIA:::WRITE(PF_POLY, repout2, paste(NAME,"PF_polygon.shp",sep="_"))

  # Création du .shp "PF_line"
  message('\n        Création de PF_line')
  PF_PTS <- st_combine(st_cast(PF_POLY[1],'MULTIPOINT', warn = F)) # Récupère les noeuds de polygones

  PF_LINE <- st_cast(PF_POLY[1],'MULTILINESTRING') # Récupère les contours de polygones
  PF_LINE <- lwgeom::st_split(PF_LINE, PF_PTS) # Segmentation : Decoupe les contours de polygones par les noeuds
  PF_LINE <- st_collection_extract(PF_LINE,"LINESTRING") # Sélectionne les objets de classe LINESTRING
  PF_LINE$LENGTH <- st_length(PF_LINE) # Ajouter un champ longueur
  PF_LINE <- PF_LINE[!duplicated(PF_LINE$LENGTH),] # Supprime les doublons sur le champs longueur
  st_crs(PF_LINE) <- "+init=epsg:2154"

  PROP_LINE <- st_read(paste(dirname(rep),paste(NAME,"PROP_line.shp",sep="_"),sep="/"),
                       options = "ENCODING=windows-1252", quiet=T)
  st_crs(PROP_LINE) <- "+init=epsg:2154"
  PROP_POLY <- st_buffer(st_union(PROP_LINE),0.0005)
  PROP_PLOY_LINE <- st_sf(st_cast(PROP_POLY,'MULTILINESTRING'))

  st_agr(PF_LINE)="constant"
  PF_LINE <- st_difference(PF_LINE, PROP_POLY[1])

  LIST_SHP <- list.files(dirname(rep), "*.shp")
  setwd(dirname(rep))

  if (paste(NAME,"PF_line.shp",sep="_") %in% LIST_SHP) {
    RES <- askYesNo("Voulez-vous remplacer PF_line ?")
    if (isTRUE(RES)) {
      cat("        Remplacement de PF_line","\n \n")
      SEQUOIA:::WRITE(PF_LINE, repout2, paste(NAME,"PF_line.shp",sep="_"))
    } else {
      if(is.na(RES)){break}
      cat("        Pas de remplacement de PF_line","\n \n")
    }
  } else {
    SEQUOIA:::WRITE(PF_LINE, repout2, paste(NAME,"PF_line.shp",sep="_"))
  }

  # Création du .shp "SSPF_polygon"
  message('        Création de SSPF_polygon')
  SSPF_POLY <- aggregate(x = SHP[, "SURF_COR"], by = list(SHP$PARFOR),
                         FUN = sum, na.rm = TRUE)
  names(SSPF_POLY) <- c("PARFOR","SURF_COR","geometry")
  SSPF_POLY <- merge(x=SSPF_POLY,
                     y=unique(as.data.frame(SHP[, c("PARFOR","PLT_TYPE","PLT_ESS", "PLT_STR", "PLT_TSE")])[,1:5]),
                     by="PARFOR")

  SEQUOIA:::WRITE(SSPF_POLY, repout2, paste(NAME,"SSPF_polygon.shp",sep="_"))

  # Actualisation de SSPF_line
  if (paste(NAME,"SSPF_line.shp",sep="_") %in% LIST_SHP) {
    RES1 <- askYesNo("Voulez-vous actualisez SSPF_line ?")
    if (isTRUE(RES1)) {
      message('\n        Actualisation de SSPF_line')

      SSPF_LINE <- st_read(paste(dirname(rep),paste(NAME,"SSPF_line.shp",sep="_"),sep="/"),
                           options = "ENCODING=UTF-8", quiet=T)
      SSPF_LINE <- SSPF_LINE %>% dplyr::select(PARFOR, LblField:LblAShow)
      SSPF_TEMPO <- merge(x = SSPF_LINE,
                          y = as.data.frame(SSPF_POLY)[,1:6],
                          by = "PARFOR") %>%
        dplyr::select(PARFOR, PLT_TYPE, PLT_ESS, PLT_STR, PLT_TSE, SURF_COR, LblField:LblAShow)

      SEQUOIA:::WRITE(SSPF_TEMPO, repout2, paste(NAME,"SSPF_line.shp",sep="_"))
    } else {
      if(is.na(RES1)){break}
    }
  }

  # Créatio de AME_polygon
  RES2 <- askYesNo(type = "yesno","Voulez-vous créer AME_polygon ?")
  if (isTRUE(RES2)) {
    message('\n        Création de AME_polygon')
    AME_list <- unique(as.data.frame(SHP["AME_TYPE"])[1])%>%
      mutate(AME_CODE = as.character(NA))
    for (i in 1:nrow(AME_list)){
      AME_list[i, "AME_CODE"] <- LETTERS[i]
    }
    SHP <- merge(SHP, AME_list, by="AME_TYPE") %>%
      mutate(BLOC = as.character(paste0(PARFOR,".",AME_CODE)))

    AME_polygon <- SHP %>%
      group_by(BLOC, PARFOR, PLT_TYPE, AME_CODE, AME_TYPE) %>%
      summarise(sum(SURF_COR))
    names(AME_polygon) <- c("BLOC","PARFOR", "PLT_TYPE", "AME_CODE", "AME_TYPE", "SURF_COR","geometry")

    SEQUOIA:::WRITE(AME_polygon, repout2, paste(NAME,"AME_polygon.shp",sep="_"))
  } else {
    if(is.na(RES2)){break}
  }
  options(warn=1)
}
