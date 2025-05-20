#' @title UAtoUA
#' Actualisation du .shp des unites d'analyse
#' @encoding UTF-8
#' @description La fonction \code{UAtoUA} calcule la surface cadastrale des unités d'analyse.
#' @usage UAtoUA(rep)
#' @param rep CHARACTER. Adresse du fichier \code{.shp} UA_polygon. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param actu BOOLEAN. Si TRUE, la fonction propose l'actualisation du répertoire si l'ancienne arborescence SEQUOIA est détectée
#' @return
#' \item{UA_polygon}{Fichier shapefile ; unité d'analyse complété}
#' @details La fonction ajoute les champs suivants : \code{SURF_SIG} Surface cartographique selon R, \code{SURF_COEFF} coefficient de correction et \code{SURF_COR} Surface cadastrale corrigée
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     UAtoUA(rep=F)
#' @export
#' 
#' @import tcltk sf qgisprocess

# Lancement des library
# library(tcltk)
# library(sf)
# library(qgisprocess)

UAtoUA <- function(rep=F) {
  message('- - - Actualisation de UA_polygon - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                            filter = matrix(c("SEQUOIA ESRI Shapefile", "_UA_polygon.shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de UA
  message('        Lecture des données')
  NAMEofSHP(rep)
  if (isFALSE(SEQ)){stop("Répertoire SEQUOIA non detecté")}
  
  UA <- st_read(rep, options = "ENCODING=UTF-8",
                   agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  assign("UA", UA, envir=globalenv())
  cat("        Le fichier UA_polygon.shp a été chargé avec succès  \n")

  # Lecture de PARCA
  LIST_SHP <- list.files(dirname(rep), "*.shp")
  setwd(dirname(rep))
  repPARCA <- gsub("_UA_", "_PARCA_", rep)

  PARCA <- st_read(repPARCA, stringsAsFactors=F, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n")

  # write function
  write <- function(nom, rep, name){
    st_write(nom, rep, name, append=FALSE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }
  
  # Correction de PARCA
  PARCA_cor <- PARCA[!st_is_empty(PARCA),,drop=FALSE]
  if (nrow(PARCA_cor)<nrow(PARCA)){
    cat("        Des polygones vides ont été détectés dans PARCA  \n")
    write(PARCA_cor, 
          repout2, 
          paste(NAME,"PARCA_polygon.shp",sep="_"))
    PARCA <- PARCA_cor
  }
  
  
  # Actualisation de R_SEQUOIA > 2
  ## Vérifiez si la colonne 'PLT_COMS' existe
  if ('PLT_COMS' %in% colnames(UA)) {
    UA$PLT_ESS <- as.character(UA$PLT_COMS)
    UA$PLT_STR <- ifelse(UA$REV_CA == 'NR', NA, as.character(UA$REV_CA))
    UA$PLT_TSE <- as.character(NA)
    UA$PARCA <- paste0(UA$SECTION, ' ', UA$N_PARCA)
  }
  
  ## Ajouter la colonne 'AME_TYPE' si elle n'existe pas
  if (!('AME_TYPE' %in% colnames(UA))) {
    UA$AME_TYPE <- as.character(NA)
  }
  
  ## Vérifiez si la colonne 'GEOL_TYPE' existe et renommez-la en 'SOL_TYPE'
  if ('GEOL_TYPE' %in% colnames(UA)) {
    UA$SOL_TYPE <- as.character(UA$GEOL_TYPE)
  }
  
  ## Ajouter la colonne 'DISP_TYPE' si elle n'existe pas
  if (!('DISP_TYPE' %in% colnames(UA))) {
    UA$DISP_TYPE <- as.character(NA)
  }
  
  ## Ajouter la colonne 'COMMENTS' si elle n'existe pas
  if (!('COMMENTS' %in% colnames(UA))) {
    UA$COMMENTS <- as.character(NA)
  }
  
  ## Refonte UA
  new_order <-c("REG_CODE", "REG_NOM", "DEP_CODE", "DEP_NOM", "COM_CODE", "COM_NOM", 
                "PROP", "ADRESSE",
                "IDU", "PREFIXE", "SECTION", "N_PARCA", "PARCA", "LIEUDIT", 
                "PARFOR", "N_PARFOR","N_SSPARFOR", "SOL_TYPE", "OCCUP_SOL", "PLT_TYPE", "PLT_ESS", "PLT_STR", "PLT_TSE", "AME_TYPE", "DISP_TYPE", "COMMENTS", "SURF_CA", "SURF_SIG", "SURF_COR")
  UA <- unique(UA[, new_order])
  cat("        Les champs de UA_polygon.shp ont été actualisé  \n")

  
  # Actualisation du champs PARFOR
  message('\n        Actualisation du champ PARFOR')
  
  ## Convertir les colonnes en caractères
  UA$N_PARFOR <- as.character(UA$N_PARFOR)
  UA$N_SSPARFOR <- as.character(UA$N_SSPARFOR)
  
  ## Fonction pour ajouter des zéros à gauche jusqu'à une longueur donnée
  pad_left <- function(x, width, pad_char = '0') {
    x <- as.character(x)
    result <- sapply(x, function(val) {
      n_pad <- pmax(0, width - nchar(val))
      if (n_pad > 0) {
        padding <- paste(rep(pad_char, n_pad), collapse = "")
      } else {
        padding <- ""
      }
      return(paste0(padding, val))
    })
    return(result)
  }
  
  ## Mettre à jour N_PARFOR
  UA$N_PARFOR <- ifelse(is.na(UA$N_PARFOR),
                        '00',
                        ifelse(grepl("^[A-Za-z]+$", UA$N_PARFOR), 
                               UA$N_PARFOR,
                               pad_left(UA$N_PARFOR, 2)))
  
  ## Mettre à jour N_SSPARFOR
  UA$N_SSPARFOR <- ifelse(is.na(UA$N_SSPARFOR),
                          '00',
                          ifelse(grepl("^[A-Za-z]+$", UA$N_SSPARFOR),
                                 UA$N_SSPARFOR,
                                 pad_left(UA$N_SSPARFOR, 2)))
  
  ## Créer la colonne PARFOR
  UA$PARFOR <- paste(UA$N_PARFOR, UA$N_SSPARFOR, sep = '.')
  
  ## Créer la colonne PARCA
  UA$PARCA <- paste(UA$SECTION, UA$N_PARCA, sep = ' ')
  
  cat("        L'identifiant PARFOR a été généré avec succès  \n \n")

  
  # Détection d'erreurs de saisies
  message("        Détection d'erreurs de saisies")
  SSPF <- unique(as.data.frame(UA[, "PARFOR"])[,-2])
  CODE <- 0

  for (a in 1:length(SSPF)) {
    ## Filtrer les lignes où PARFOR correspond à SSPF[a]
    SSPF_UA <- as.data.frame(UA[UA$PARFOR %in% SSPF[a], ])
    
    ## Supprimer la dernière colonne
    SSPF_UA <- SSPF_UA[, -ncol(SSPF_UA)]
    
    ## Obtenir les valeurs uniques pour chaque colonne
    OCCUP_SOL <- unique(SSPF_UA[, "OCCUP_SOL", drop = FALSE])
    if (nrow(OCCUP_SOL) > 1) {
      message("        Plusieurs OCCUP_SOL pour la parcelle ", SSPF[a])
      CODE <- CODE + 1
    }
    
    PLT_TYPE <- unique(SSPF_UA[, "PLT_TYPE", drop = FALSE])
    if (nrow(PLT_TYPE) > 1) {
      message("        Plusieurs PLT_TYPE pour la parcelle ", SSPF[a])
      CODE <- CODE + 1
    }
    
    PLT_ESS <- unique(SSPF_UA[, "PLT_ESS", drop = FALSE])
    if (nrow(PLT_ESS) > 1) {
      message("        Plusieurs PLT_ESS pour la parcelle ", SSPF[a])
      CODE <- CODE + 1
    }
    
    PLT_STR <- unique(SSPF_UA[, "PLT_STR", drop = FALSE])
    if (nrow(PLT_STR) > 1) {
      message("        Plusieurs PLT_STR pour la parcelle ", SSPF[a])
      CODE <- CODE + 1
    }
    
    PLT_TSE <- unique(SSPF_UA[, "PLT_TSE", drop = FALSE])
    if (nrow(PLT_TSE) > 1) {
      message("        Plusieurs PLT_TSE pour la parcelle ", SSPF[a])
      CODE <- CODE + 1
    }
  }

  if (CODE>0){ # Boucle erreur
    message("        |!| ", CODE, " anomalies détectées dans la table > Traitement annulé > Veuillez corriger |!|")
    assign("Erreurs", "Erreurs", envir=globalenv())
    write(UA, 
          repout2, 
          paste(NAME,"UA_polygon.shp",sep="_"))

    stop("Anomalies détectées > Corrigez la table \n")
  }else{
    assign("Erreurs", "OK", envir=globalenv())
    date <- gsub(":", ".", format(Sys.time(), "%Y.%m.%d-%X"))
    write(UA, 
          repout2, 
          paste(NAME,"UA_polygon.shp",sep="_"))
    write(UA, 
          repout2, 
          paste(NAME, paste0("UA_", date, "_polygon.shp"),sep="_"))
  }
  
  # Correction des erreurs topologiques
  message("\n        Correction des erreurs topologiques")
  providers <- qgis_providers()
  if("GRASS" %in% providers$provider_title) {
    invisible(
      capture.output(
        suppressMessages(
          v_clean <- qgis_run_algorithm("grass:v.clean",
                                        input = rep,
                                        type = "area",
                                        tool = "snap",
                                        GRASS_SNAP_TOLERANCE_PARAMETER = 0.05,
                                        GRASS_MIN_AREA_PARAMETER = 0.1))))
    
    cleaned <- v_clean[[".args"]][["output"]]
    
    UA <- st_read(cleaned, agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)[,-1]
    cat("        Les erreurs topologiques ont été corrigées \n \n")
  } else {
    cat("        grassprovider introuvable \n")
    message("        Les erreurs topologiques n'ont pas été corrigées \n \n")
  }

  # Correction des SURF_CA
  message("        Correction du champ SURF_CA")
  PARCA$SURFACE <- PARCA$SURF_CA
  IDU <- as.data.frame(PARCA[c("IDU","SURFACE")])[,-3] # Réapitulatif des parcelles cadastrales
  SHP <- merge(UA, IDU, by = "IDU", all=T)
  SHP$SURF_CA <- SHP$SURFACE

  UA <- SHP[ , -which(names(SHP) %in% c("SURFACE"))] # Suppression du champ SURFACE
  cat("        Les surfaces cadastrales ont été vérifiées  \n \n")

  # Création d'un id par UA IDUA
  message("        Calcul du champ SURF_COR")
  UA$IDUA <- seq.int(nrow(UA))

  UA_COR <- st_sf(st_sfc())
  UA_OUT <- subset(UA, is.na(IDU)) |>
    transform(SURF_CA=0)

  for (a in 1:nrow(IDU)) {
    IDUCA <- IDU[a,1]
    cat("        Traitement de la parcelle cadastrale",paste0(IDUCA),"|", a, "/", nrow(IDU),"\n")

    UA_TAB <- subset(UA, UA$IDU %in% IDUCA)
    
    ## Traitement des géométries vides
    if (nrow(UA_TAB)<=1) {
      for (b in 1:nrow(UA_TAB)){
        if (st_is_empty(UA_TAB[b,])){
          stop("Aucune géométrie détectée pour la parcelle ", IDUCA,". Veuillez vérifier et corriger l'UA \n")
        }
      }
    }
    UA_TAB <- subset(UA_TAB, !st_is_empty(UA_TAB))

    ## Calcul des SURF_CA
    UA_TAB$SURF_CA <- UA_TAB$SURF_CA*10000 # Calcul de la surface cadastrale

    ## Calcul des SURF_SIG
    UA_TAB$SURF_SIG <- round(as.numeric(st_area(UA_TAB))) # Calcul de la surface cartographique

    ## Détermination du coeff de correction
    TAB <- as.data.frame(UA_TAB)
    TAB <- aggregate(cbind(SURF_SIG, SURF_CA) ~ IDU, data = UA_TAB, 
                     FUN = function(x) c(SIG = sum(x), CA = mean(x)))
    TAB <- data.frame(IDU = TAB$IDU, 
                      SIG = TAB$SURF_SIG[, "SIG"], 
                      CA = TAB$SURF_CA[, "CA"])
    TAB$SURF_COEFF <- round(TAB$CA / TAB$SIG, 10)

    ## Calcul des SURF_COR
    if ("SURF_COEFF" %in% names(UA_TAB)) {UA_TAB <- UA_TAB[ , -which(names(UA_TAB) %in% c("SURF_COEFF"))]}
    UA_TAB <- merge(UA_TAB, TAB[c("IDU","SURF_COEFF")], by = "IDU", all=T)
    UA_TAB$SURF_COEFF <- as.numeric(UA_TAB$SURF_COEFF)
    UA_TAB$SURF_COR <- round(UA_TAB$SURF_SIG*UA_TAB$SURF_COEFF,0)
    UA_TAB$SURF_COR <- round(UA_TAB$SURF_COR,0)
    
    ecart = (sum(UA_TAB$SURF_COR)-sum(UA_TAB$SURF_SIG))/sum(UA_TAB$SURF_COR)*100
    
    ## Détermination de la difference restante
    SURF_CA <- IDU[a,2]*10000
    DIFFERENCE <- SURF_CA-sum(UA_TAB$SURF_COR)

    ## Correction de la surface
    if(abs(DIFFERENCE)>0){
      MAX <- max(UA_TAB$SURF_COR)
      ROW <- grep(MAX, UA_TAB$SURF_COR)
      VALUE = as.data.frame(UA_TAB[ROW, "SURF_COR"])[1,1]
      UA_TAB[ROW, "SURF_COR"]=VALUE + DIFFERENCE
    }

    ## Simplification de UA_TAB
    UA_TAB <- UA_TAB[ , -which(names(UA_TAB) %in% c("IDUA","SURF_COEFF"))]

    ## Création de UA_COR
    UA_TAB <- st_transform(UA_TAB, 2154)
    st_crs(UA_COR) <- st_crs(UA_TAB)
    UA_COR <- rbind(UA_COR, UA_TAB)

    if(abs(as.integer(DIFFERENCE))>=5 || ecart>15){
      message("            ",
              "SURF_CA: ", unique(UA_TAB$SURF_CA),"m² ",
              "SURF_SIG: ", sum(UA_TAB$SURF_SIG),"m² ",
              "Coeff: ", round(unique(TAB$SURF_COEFF),2), " ",
              "Ecart: ", ecart,"m² \n")
    }
  }
  
  UA_OUT <- UA_OUT[ , -which(names(UA_OUT) %in% c("IDUA"))] |>
    transform(SURF_COR=0)
  UA_COR <- rbind(UA_COR, UA_OUT)

  # Actualisation de UA
  cat("        UA_polygon a été actualisé \n \n")
  UA_COR <- st_transform(UA_COR, 2154)
  UA_COR$SURF_CA  = UA_COR$SURF_CA/10000
  UA_COR$SURF_SIG = UA_COR$SURF_SIG/10000
  UA_COR$SURF_COR = UA_COR$SURF_COR/10000
  UA_COR <- unique(UA_COR[, new_order])

  # Actualisation du .shp "UA_polygon"
  message("        Export de UA_polygon")
  write(UA_COR, 
        repout2, 
        paste(NAME,"UA_polygon.shp",sep="_"))
  
  # Export de la table attributaire
  sortie <- as.data.frame(UA_COR)[,1:29]
  repOut <- paste(dirname(dirname(dirname(rep))), paste0(NAME, "_PSG_", date, ".xlsx"), sep="/")
  write.xlsx(sortie, repOut, overwrite = T)
  cat(paste("        Le tableur UA a été enregistré dans le repertoire : ", repOut),"\n \n")

  options(warn=1)
  
}
