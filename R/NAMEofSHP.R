#' @title NAMEofSHP
#' Detection du préfixe & Actualisation des repertoires SEQUOIA
#' @encoding UTF-8
#' @description
#' La fonction \code{NAMEofSHP} permet de récupérer le préfixe d'un shapefile SEQUOIA et d'actualiser les répertoires SEQUOIA
#' @usage NAMEofSHP(shp=F)
#' @param shp CHARACTER. Chemion du fichier shapefile d'emprise. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier
#' @param actu BOOLEAN. Si TRUE, la fonction propose l'actualisation du répertoire si l'ancienne arborescence SEQUOIA est détectée
#' @details La fonction exporte dans l'environnement le préfixe et les répertoires SEQUOIA. A défaut, elle demande un nom.
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     NAMEofSHP(rep = F)
#' @export
#' 
#' @import tcltk

# Lancement des library
# library(tcltk)

NAMEofSHP <- function(shp=F, actu=F, change=F){ # Function
  # Sélection du shapefile
  if(isFALSE(shp)){
    shp  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  # Récupération du préfixe
  SEQUOIA_liste_prefixe <- c("_AME_","_COM_", "_COMS_", "_GEOL_", "_INFRA_", "_LIEUDIT_", 
                             "_PARCA_", "_PARCA-ARCHIVE_", "_PF_", "_PROP_", "_ROAD_", 
                             "_ROUTE_", "_SSPF_", "_SSPFinv_", "_TOPO_", "_UA_", "_RELEVES_")
  assign("SEQUOIA_liste_prefixe", SEQUOIA_liste_prefixe, envir=globalenv())
  
  NAME <- NA
  for (a in 1:length(SEQUOIA_liste_prefixe)) {
    if(grepl(SEQUOIA_liste_prefixe[a], shp)) {
      last_slash_pos <- max(gregexpr('/', shp)[[1]])
      prefix_pos <- regexpr(SEQUOIA_liste_prefixe[a], shp)
      NAME <- substr(shp, last_slash_pos + 1, prefix_pos - 1)
    }
  }
  if(!is.na(NAME)){cat(paste0("        Préfixe SEQUOIA détecté: ", NAME, "\n"))}
  
  # Affectation d'un préfixe
  if(is.na(NAME)){message("        Préfixe SEQUOIA non détecté. \n")}
  if(is.na(NAME)){
    if (Sys.info()["sysname"]=="Windows"){
      NAME <- utils::winDialogString("Entrer le nom de la forêt (préfixe) :", "")
    }else {
      NAME <- readline(prompt="Entrer le nom de la forêt (préfixe) :")
    }
  }
  assign("NAME", NAME, envir=globalenv())
  
  # Changement de préfixe
  if (isTRUE(change)){
    change <- askYesNo("Voulez-vous changer le préfixe SEQUOIA ?", FALSE, getOption("askYesNo", gettext(c("Oui", "Non", "Annuler"))))
    if (is.na(change)){change<-TRUE}
  }
  
  if (isTRUE(change)){
    if (Sys.info()["sysname"]=="Windows"){
      newNAME <- utils::winDialogString(message=paste0("Remplacer ", NAME, " par :"), "")
    }else {
      newNAME <- readline(prompt=paste0("Remplacer ", NAME, " par :"))
    }
    assign("newNAME", newNAME, envir=globalenv())
  }
  
  # Détection de l'arborescence old SEQUOIA
  SEQ <- FALSE
  old_folders <- c("0 SORTIE", "1 RASTER", "2 PSG", "3 TEMPO")
  for (a in 1:length(old_folders)) {
    if(grepl(paste0("SIG/", old_folders[a]), shp)) {
      SEQ <- 'old'
    }
  }
  
  # Détection de l'arborescence new SEQUOIA
  new_folders <- c("0_OUTPUT", "1_RASTER", "2_VECTOR", "3_OTHER")
  for (a in 1:length(new_folders)) {
    if(grepl(paste0("SIG/", new_folders[a]), shp)) {
      SEQ <- 'new'
    }
  }
  assign("SEQ", SEQ, envir=globalenv())
  
  # Actualisation de l'arborescence old SEQUOIA
  if (SEQ=='old') { # actu
    if (isTRUE(actu)){
      actu <- askYesNo("Voulez-vous actualiser l'arborescence SEQUOIA ?", FALSE, getOption("askYesNo", gettext(c("Oui", "Non", "Annuler"))))
      if (is.na(actu)){actu<-FALSE}
    }
    
    if (isTRUE(actu)){ # actu
      cat("        Actualisation des répertoires \n")
      
      ## Détection arborescence SIG
      rep_SIG <- substr(shp, 1, regexpr('SIG', shp) + attr(regexpr('SIG', shp), "match.length") - 1)
      folders<-list.files(rep_SIG)
      
      ## Création nouvelle arboressence
      for (a in 1:length(new_folders)){
        if (!dir.exists(paste(rep_SIG ,new_folders[a],sep="/"))){
          dir.create(paste(rep_SIG, new_folders[a],sep="/"))
        }
      }
      
      ## Déplacement des anciens fichiers
      for (b in 1:length(folders)){
        files<-list.files(paste(rep_SIG, folders[b], sep="/"))
        if (length(files)>=1){
          for (c in 1:length(files)){
            if (b<4) {n<-b}
            if (b==4){n<-3}
            if (b>4){n<-4}
            file.copy(paste(rep_SIG, folders[b], files[c], sep="/"), 
                      paste(rep_SIG, new_folders[n], sep="/"))
          }
        }
        unlink(paste(rep_SIG, folders[b], sep="/"), recursive = TRUE)
      }
    }
  }  # end actu
  
  # Changement du préfixe SEQUOIA
  if (isTRUE(change)){
    cat("        Changement de préfixe SEQUOIA\n")
    
    ## Détection arborescence SIG
    rep_SIG <- substr(shp, 1, regexpr('SIG', shp) + attr(regexpr('SIG', shp), "match.length") - 1)
    files <- list.files(path= rep_SIG, pattern = NAME, recursive =T)
    
    ## Changement de préfixe
    for (b in 1:length(files)){
      file <- files[b]
      newfile <- gsub(NAME, newNAME, file)
      file <- paste(rep_SIG, file, sep="/")
      newfile <- paste(rep_SIG, newfile, sep="/")
      file.rename(file, newfile)
    }
  }
  
  # Définition des répertoires de travail
  if (isFALSE(SEQ)) {
    repout <- dirname(shp)
    message("        Répertoires SEQUOIA non détectés. \n")
    
    assign("repout", repout, envir=globalenv())
  } else {
    if (SEQ =='new') {
      repout0 <- paste(dirname(dirname(shp)),new_folders[1],sep="/")
      repout1 <- paste(dirname(dirname(shp)),new_folders[2],sep="/")
      repout2 <- paste(dirname(dirname(shp)),new_folders[3],sep="/")
      repout3 <- paste(dirname(dirname(shp)),new_folders[4],sep="/")
      cat("        Répertoires SEQUOIA actualisés détectés. \n")
    }
    
    if (SEQ =='old') {
      if (actu) {
        repout0 <- paste(dirname(dirname(shp)),new_folders[1],sep="/")
        repout1 <- paste(dirname(dirname(shp)),new_folders[2],sep="/")
        repout2 <- paste(dirname(dirname(shp)),new_folders[3],sep="/")
        repout3 <- paste(dirname(dirname(shp)),new_folders[3],sep="/")
        cat("        Répertoires SEQUOIA actualisés détectés. \n")
      } else {
        repout0 <- paste(dirname(dirname(shp)),old_folders[1],sep="/")
        repout1 <- paste(dirname(dirname(shp)),old_folders[2],sep="/")
        repout2 <- paste(dirname(dirname(shp)),old_folders[3],sep="/")
        repout3 <- paste(dirname(dirname(shp)),old_folders[4],sep="/")
        cat("        Répertoires SEQUOIA non actualisés détectés. \n")
      }
    }
    assign("repout0", repout0, envir=globalenv())
    assign("repout1", repout1, envir=globalenv())
    assign("repout2", repout2, envir=globalenv())
    assign("repout3", repout3, envir=globalenv())
  }
  
}

