#' @title UAtoUA
#' Actualisation du .shp des unites d'analyse
#' @encoding UTF-8
#' @description La fonction \code{UAtoUA} calcule la surface cadastrale des unités d'analyse.
#' @usage UAtoUA(rep)
#' @param rep CHARACTER. Adresse du fichier \code{.shp} UA_polygon. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' \item{UA_polygon}{Fichier shapefile ; unité d'analyse complété}
#' @details La fonction ajoute les champs suivants : \code{SURF_SIG} Surface cartographique selon R, \code{SURF_COEFF} coefficient de correction et \code{SURF_COR} Surface cadastrale corrigée
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     UAtoUA(rep=F)
#' @export
#' 
#' @import tcltk stringr sf dplyr stringr

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("data.table")) {install.packages("data.table")}

UAtoUA <- function(rep=F) {
  message('- - - Actualisation de UA_polygon - - -')
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}

  # Lecture de UA
  message('        Lecture des données')
  UA <- st_read(rep, options = "ENCODING=UTF-8",
                   agr = "constant", crs=2154, quiet=T, stringsAsFactors = FALSE)
  cat("        Le fichier UA_polygon.shp a été chargé avec succès  \n")
  assign("UA", UA, envir=globalenv())

  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_UA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

  # Lecture de PARCA
  LIST_SHP <- list.files(dirname(rep), "*.shp")
  setwd(dirname(rep))
  repPARCA <- str_replace(rep,"_UA_","_PARCA_")

  PARCA <- st_read(repPARCA, stringsAsFactors=F, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("        Le fichier PARCA_polygon.shp a été chargé avec succès  \n")

  # Correction de PARCA
  PARCA_cor <- PARCA[!st_is_empty(PARCA),,drop=FALSE]
  if (nrow(PARCA_cor)<nrow(PARCA)){
    cat("        Des polygones vides ont été détectés dans PARCA  \n")
    SEQUOIA:::WRITE(PARCA_cor, repout2, paste(NAME,"PARCA_polygon.shp",sep="_"))
    PARCA <- PARCA_cor
  }
  # Actualisation de R_SEQUOIA > 2
  if ('PLT_COMS' %in% colnames(UA)) {
    UA <- UA %>%
      mutate(PLT_ESS    = as.character(PLT_COMS),
             PLT_STR    = case_when(REV_CA=='NR'~ as.character(NA), REV_CA!='NR'~ as.character(REV_CA)),
             PLT_TSE    = as.character(NA),
             PARCA = as.character(paste0(SECTION, ' ', N_PARCA)))
    if (!('AME_TYPE' %in% colnames(UA))) {
      UA <- UA %>% mutate(AME_TYPE = as.character(NA))}
    UA <- UA %>%
      dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE,
                    IDU, PREFIXE, SECTION, N_PARCA, PARCA, LIEUDIT,
                    PARFOR, N_PARFOR, N_SSPARFOR, GEOL_TYPE, OCCUP_SOL,
                    PLT_TYPE, PLT_ESS, PLT_STR, PLT_TSE, AME_TYPE,
                    SURF_CA, SURF_SIG, SURF_COR, geometry)
    cat("        Les champs de UA_polygon.shp ont été actualisé  \n")
  }

  # Actualisation du champs PARFOR
  message('\n        Actualisation du champ PARFOR')
  UA <- unique(UA) %>%
    mutate(N_PARFOR   = as.character(N_PARFOR),
           N_SSPARFOR = as.character(N_SSPARFOR),
           N_PARFOR   = case_when(grepl("^[A-Za-z]+$", N_PARFOR, perl = T)~ N_PARFOR,
                                  !grepl("^[A-Za-z]+$", N_PARFOR, perl = T)~ str_pad(N_PARFOR,2,"left",'0')),
           N_SSPARFOR   = case_when(grepl("^[A-Za-z]+$", N_SSPARFOR, perl = T)~ N_SSPARFOR,
                                    !grepl("^[A-Za-z]+$", N_SSPARFOR, perl = T)~ str_pad(N_SSPARFOR,2,"left",'0')),
           PARFOR = paste0(N_PARFOR,'.',N_SSPARFOR),
           PARCA  = as.character(paste0(SECTION, ' ', N_PARCA)))
  cat("        L'identifiant PARFOR a été généré avec succès  \n \n")

  # Détection d'erreurs de saisies
  message("        Détection d'erreurs de saisies")
  SSPF <- unique(as.data.frame(UA[, "PARFOR"])[,-2])
  CODE=0
  for (a in 1:length(SSPF)){
    SSPF_UA <- UA %>% filter(PARFOR %in% SSPF[a])
    SSPF_UA <- as.data.frame(SSPF_UA)[,-ncol(SSPF_UA)]

    OCCUP_SOL <- as.data.frame(unique(SSPF_UA[, "OCCUP_SOL"]))
    if(nrow(OCCUP_SOL)>1){message("        Plusieurs OCCUP_SOL pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    PLT_TYPE <- as.data.frame(unique(SSPF_UA[, "PLT_TYPE"]))
    if(nrow(PLT_TYPE)>1){message("        Plusieurs PLT_TYPE pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    PLT_ESS <- as.data.frame(unique(SSPF_UA[, "PLT_ESS"]))
    if(nrow(PLT_ESS)>1){message("        Plusieurs PLT_ESS pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    PLT_STR <- as.data.frame(unique(SSPF_UA[, "PLT_STR"]))
    if(nrow(PLT_STR)>1){message("        Plusieurs PLT_STR pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    PLT_TSE <- as.data.frame(unique(SSPF_UA[, "PLT_TSE"]))
    if(nrow(PLT_TSE)>1){message("        Plusieurs PLT_TSE pour la parcelle ", SSPF[a])
      CODE=CODE+1}

  }


  if (CODE>0){ # Boucle erreur
    message("        |!| ",
            CODE, " anomalies détectées dans la table > Traitement annulé > Veuillez corriger |!|")
    assign("Erreurs", "Erreurs", envir=globalenv())

    SEQUOIA:::WRITE(UA, repout2, paste(NAME,"UA_polygon.shp",sep="_"))

    stop("Anomalies détectées > Corrigez la table \n")
  }else{
    assign("Erreurs", "OK", envir=globalenv())
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
  UA_OUT <- UA %>%
    filter(is.na(IDU))

  for (a in 1:nrow(IDU)) {
    IDUCA <- IDU[a,1]
    cat("        Traitement de la parcelle cadastrale",paste0(IDUCA),"|", a, "/", nrow(IDU),"\n")

    UA_TAB <- UA %>%
      filter(UA$IDU %in% IDUCA)

    # Calcul des SURF_CA
    UA_TAB$SURF_CA <- UA_TAB$SURF_CA*10000 # Calcul de la surface cadastrale

    # Calcul des SURF_SIG
    UA_TAB$SURF_SIG <- round(as.numeric(st_area(UA_TAB))) # Calcul de la surface cartographique

    # Détermination du coeff de correction
    TAB <- as.data.frame(UA_TAB) %>%
      group_by(IDU) %>% # Récapitulatif
      summarise(SIG = sum(SURF_SIG),
                CA  = mean(SURF_CA)) %>%
      mutate(SURF_COEFF = round((CA/SIG),10))

    # Calcul des SURF_COR
    if ("SURF_COEFF" %in% names(UA_TAB)) {UA_TAB <- UA_TAB[ , -which(names(UA_TAB) %in% c("SURF_COEFF"))]}
    UA_TAB <- merge(UA_TAB, TAB[c("IDU","SURF_COEFF")], by = "IDU", all=T) %>%
      mutate(SURF_COEFF = as.numeric(SURF_COEFF),
             SURF_COR = round(SURF_SIG*SURF_COEFF,0),
             SURF_COR = round(SURF_COR,0))
    ecart = (UA_TAB$SURF_COR-UA_TAB$SURF_SIG)/UA_TAB$SURF_COR*100
    
    # Détermination de la difference restante
    SURF_CA <- IDU[a,2]*10000
    DIFFERENCE <- SURF_CA-sum(UA_TAB$SURF_COR)

    # Correction de la surface
    if(abs(DIFFERENCE)>0){
      MAX <- max(UA_TAB$SURF_COR)
      ROW <- grep(MAX, UA_TAB$SURF_COR)
      VALUE = as.data.frame(UA_TAB[ROW, "SURF_COR"])[1,1]
      UA_TAB[ROW, "SURF_COR"]=VALUE + DIFFERENCE
    }

    # Simplification de UA_TAB
    UA_TAB <- UA_TAB[ , -which(names(UA_TAB) %in% c("IDUA","SURF_COEFF"))]

    # Création de UA_COR
    UA_TAB <- st_transform(UA_TAB, 2154)
    st_crs(UA_COR) <- st_crs(UA_TAB)
    UA_COR <- rbind(UA_COR, UA_TAB)

    if(abs(as.integer(DIFFERENCE))>=5 || ecart>15){
      message("        ",
              "SURF_CA: ", unique(UA_TAB$SURF_CA),"m²",
              "SURF_SIG: ", sum(UA_TAB$SURF_SIG),"m²",
              "Coeff: ", unique(TAB$SURF_COEFF),
              "Ecart: ", DIFFERENCE,"m² \n \n")
    }
  }

  UA_COR <- rbind(UA_COR, UA_OUT[ , -which(names(UA_OUT) %in% c("IDUA"))])

  # Actualisation de UA
  cat("        UA_polygon a été actualisé \n \n")
  UA_COR <- st_transform(UA_COR, 2154)
  UA_COR <- UA_COR %>%
    mutate(SURF_CA  = SURF_CA/10000,
           SURF_SIG = SURF_SIG/10000,
           SURF_COR = SURF_COR/10000) %>%
    dplyr::select(REG_CODE:ADRESSE,IDU,PREFIXE:N_PARCA, PARCA, LIEUDIT,PARFOR,N_PARFOR,N_SSPARFOR,GEOL_TYPE, OCCUP_SOL,PLT_TYPE, PLT_ESS, PLT_STR, PLT_TSE, AME_TYPE,SURF_CA,SURF_SIG,SURF_COR:geometry)

  # Actualisation du .shp "UA_polygon"
  message("        Export de UA_polygon")
  NAME = str_sub(rep,str_locate(rep,"PSG/")[1,2]+1,str_locate(rep,"_UA")[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

  SEQUOIA:::WRITE(UA_COR, repout2, paste(NAME,"UA_polygon.shp",sep="_"))

  sortie <- as.data.frame(UA_COR)[,1:27]
  repOut <- paste(dirname(dirname(dirname(rep))), paste0(NAME, "_PSG",".xlsx"), sep="/")
  openxlsx::write.xlsx(sortie, repOut)
  cat(paste("        Le tableur UA a été enregistré dans le repertoire : ", repOut),"\n \n")

  options(warn=1)
}
