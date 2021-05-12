### Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}

INPNonSHP <- function(shp = F, repRdata = F, NAME=NULL){
  message("- - - Zonnage environnementaux - - -")
  if(isFALSE(shp)){
    shp  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if(isFALSE(repRdata)){
    repRdata <- tcltk::tk_choose.dir(caption = "Choisir le dossier contenant les données INPN.Rdata")
  }

  if (!length(shp) | !length(repRdata)){
    stop("Pas de fichiers sélectionné >> Traitement annulé")} 
  
  # Import des données .shp
  message('        Lecture des données')
  SHP <- st_read(shp, options = "ENCODING=UTF-8", quiet=T, stringsAsFactors = F)  # Lecture du shapefile
  SHP <- st_sf(st_combine(SHP))
  cat("        Le fichier .shp a été chargé avec succès \n")
  if (grepl("PARCA", REP_SHP )){
    NAME <- str_sub(REP_SHP,
                    str_locate_all(REP_SHP,'/')[[1]][nrow(str_locate_all(REP_SHP,'/')[[1]]),1]+1,
                    str_locate(REP_SHP,'_PARCA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())
  }
  if (grepl("UA", REP_SHP )){
    NAME <- str_sub(REP_SHP,
                    str_locate_all(REP_SHP,'/')[[1]][nrow(str_locate_all(REP_SHP,'/')[[1]]),1]+1,
                    str_locate(REP_SHP,'_UA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())
  }
  if(is.null(NAME)){
    NAME <- winDialogString("Entrer le nom du fichier de sortie (optionnel) : ", "")
  }
  if(!length(NAME)) {NAME <- ""} else {NAME <- paste0(NAME,"_")}
  
  # Import des données .Rdata
  load(paste(repRdata,"INPN.Rdata",sep="/"))
  cat("        L'archive .Rdata a été chargé avec succès \n \n")
  
  # Intersection
  message('        Détection des intersections')
  for (a in 1:length(list_INPN)) {
    INPN_POLY <- st_sf(list_INPN[[a]], stringsAsFactors = F)
    SHP <- st_transform(SHP,st_crs(list_INPN[[a]]))
    SHP["INPN"] <- "INPN"
    
    INPN_POLY   <- st_join(list_INPN[[a]],SHP["INPN"])
    
    INPN_POLY   <- INPN_POLY %>% filter(!is.na(INPN))
    
    NOM <- list_INPN_NOM[[a]]
    
    if (nrow(INPN_POLY)>=1) {
      SEQUOIA:::WRITE(INPN_POLY, dirname(shp), str_replace(paste0(NAME, NOM),".shp",""))
    } else {
      cat(paste("        Aucune correspondance entre le .shp et ", NOM), "\n")
    }
  }
}


