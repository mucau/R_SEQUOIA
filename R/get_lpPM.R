#' @title askme_lpPM
#' Recuperation des relevé des locaux et parcelles des personnes morales dans un département
#' @encoding UTF-8
#' @description
#' La fonction \code{askme_lpPM} permet d'obtenir un relevé complet d'une personne morale pour un département.
#' La fonction demande le département consernée et une dénomination du propriétaire.
#' 
#' La fonction \code{download_lpPM} permet de télécharger en amont les données 2021. Elle renvoit le répertoire de dossier contenant les txt.
#' 
#' @usage askme_lpPM(rep=F, dep=F, prop=NULL)
#' @param rep CHARACTER. Chemin du dossier contenant les txt. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier
#' @param dep CHARACTER. Numéro du département Si \code{FALSE}, la fonction génère une boite une boite de saisie.
#' @param prop CHARACTER. Dénomintation du propriétaire. Si \code{NULL}, la fonction génère une boite de saisie.
#' 
#' @details
#' La fonction \code{download_lpPM} télécharge les données depuis le serveur \url{https://data.economie.gouv.fr/explore/dataset/fichiers-des-locaux-et-des-parcelles-des-personnes-morales/information/}
#' 
#' @author 
#' Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples
#' ### Fonctionnement :
#'     monrépertoire <- download_lpPM(T)
#'     askme_lpPM(rep = monrépertoire, dep=F, prop=NULL)
#'     
#' ### ou :
#'     askme_lpPM(rep = F, dep="53", prop="mon propriétaire")
#'     
#' @export download_lpPM
#' @export askme_lpPM
#' 
#' @import tcltk openxlsx archive
# 
# library(tcltk)
# library(openxlsx)
# library(archive)

download_lpPM <- function(run=FALSE){
  options(timeout=600)
  message("Téléchargement des données \n")
  urls <- c("https://data.economie.gouv.fr/api/datasets/1.0/fichiers-des-locaux-et-des-parcelles-des-personnes-morales/attachments/fichier_des_parcelles_situation_2023_dept_01_a_61_zip/",
            "https://data.economie.gouv.fr/api/datasets/1.0/fichiers-des-locaux-et-des-parcelles-des-personnes-morales/attachments/fichier_des_parcelles_situation_2023_dept_62_a_976_zip/")
  
  for (a in 1:2){
    url <- urls[a]
    destfile <- tempfile(, fileext = ".zip")
    cat(paste0("Téléchargements des données : ", a, "/2", "\n"))
    download.file(url, destfile, mode= "wb", quiet=F)
    cat(paste0("Extraction des données : ", a, "/2", "\n"))
    archive::archive_extract(destfile, dirname(destfile))
    cat("Extraction du fichier terminée", "\n \n")
  }
  
  message(paste0("Les données sont dans ", dirname(destfile), "\n \n"))
  assign("repPPM",dirname(destfile),envir=globalenv())
  return(dirname(destfile))
}

askme_lpPM <- function(rep=F, dep=F, field=F, value=F) {
  # Choix du rep
  if(isFALSE(rep)) {
    rep  <- tk_choose.dir(default= dirname(dirname(tempfile())),
                          caption = "Choisir le dossier contenant les fichiers txt")
  }
  if (!length(rep)){stop("Aucun dossier sélectionnée !")}
  
  # Sélection des txt
  liste_txt <- list.files(rep, "*.txt", recursive=T)
  
  #Sélection du département
  message("Sélection du département")
  if(isFALSE(dep)) {
    if (Sys.info()["sysname"]=="Windows"){
      dep <- winDialogString("Entrer le n° de département: ", "")
    }else {
      dep <- readline(prompt="Entrer le n° de département: ")
    }
  }
  if(is.null(dep)||dep==""){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  dep <- paste0(sprintf("%02s", dep), "0")
  
  # Lecture du txt
  colnames <- c("Département",
                "CodeDirection",
                "CodeCommune",
                "NomCommune",
                "Préfixe",
                "Section",
                "Numplan",
                "Numdevoirie",
                "Indicederépétition",
                "CodevoieMAJIC",
                "Codevoierivoli",
                "Naturevoie",
                "Nomvoie",
                "Contenance_par",
                "SUF",
                "Natureculture",
                "Contenance_suf",
                "Codedroit",
                "NumMAJIC",
                "NumSIREN",
                "Groupepersonne",
                "Formejuridique",
                "Formejuridiqueabrégée",
                "Dénomination")
  
  liste_txt_dep <- list.files(rep, paste0("PM_23_NB_",dep), recursive=T)
  
  ppm <- as.data.frame(matrix(data=as.character(NA), nrow=0, ncol=24))
  colnames(ppm) <- colnames
  
  for (b in 1:length(liste_txt_dep)){
    txt <- paste(rep, liste_txt_dep[b], sep="/")
    data <- utils::read.table(file=txt, header=T, sep=";", quote="\"'", col.names = colnames, colClasses="character")
    
    data$Contenance_par <- as.numeric(data$Contenance_par)
    data$Contenance_suf <- as.numeric(data$Contenance_suf)
    
    ppm <- rbind(ppm, data)
  }
  cat("Les données du département ",dep, " ont été récupérées \n\n")
  
  #Sélection de la donnée
  message("Sélection des données")
  if(isFALSE(field)){
    form <- c("Dénomination du propriétaire",
              "Commune complète")
    
    field <- select.list(form,
                         multiple = F,
                         title = "Choix de la source de donnée",
                         graphics = T)
  }
  if (field==""){stop("Aucune source sélectionnée !")}
  
  if ("Dénomination du propriétaire" %in% field){
    field <- "Dénomination"
  }
  if ("Commune complète" %in% field){
    field <- "CodeCommune"
  }
  
  # Boucle propriétaire
  repeat{
    
    # Choix de la valeur
    if(isFALSE(value)){
      if (Sys.info()["sysname"]=="Windows"){
        value <- winDialogString("Entrer la valeur cherchée:", "")
      }else {
        value <- readline(prompt="Entrer la valeur cherchée:")
      }
    }
    if(is.null(value)||value=="") {stop("Aucun nom saisi > Traitement annulé \n")}
    
    # Remplacer filter et grepl
    field_value <- ppm[grepl(value, ppm[[field]]), ]
    
    if(nrow(field_value)>0){
      cat(paste0(nrow(field_value), " références trouvées \n \n"))
      break
    } else {
      value <- NULL
      cat("Aucune référence trouvée")
    }
  }
  
  # Export des données
  message("Export des données")
  repout<- getwd()
  openxlsx::write.xlsx(field_value, paste(repout, paste0(value, "_relevé.xlsx"), sep="/"), overwrite = T)
  cat(paste("Le relevé du propriétaire a été enregistré dans le repertoire : ", repout),"\n")
  
  # Export de la matrice
  matrice <- field_value
  matrice$REG_CODE <- NA
  matrice$REG_NOM <- NA
  matrice$DEP_CODE <- matrice$Département
  matrice$DEP_NOM <- NA
  matrice$COM_CODE <- matrice$CodeCommune
  matrice$COM_NOM <- matrice$NomCommune
  matrice$PROP <- matrice$Dénomination
  matrice$ADRESSE <- NA
  matrice$PREFIXE <- ifelse(matrice$Préfixe == '   ', '000', gsub(" ", 0, sprintf("%03s",matrice$Préfixe)))
  matrice$SECTION <- gsub(" ", 0, sprintf("%02s",matrice$Section))
  matrice$N_PARCA <- gsub(" ", 0, sprintf("%04s",matrice$Numplan))
  matrice$LIEUDIT <- matrice$Nomvoie
  matrice$OCCUP_SOL <- as.character("BOISEE")
  matrice$TX_BOISE <- as.character("1")
  matrice$REV_CA <- as.character("NR")
  matrice$SURF_CA <- as.numeric(matrice$Contenance_par) / 10000
  matrice$IDU <- paste0(matrice$COM_CODE, matrice$PREFIXE, matrice$SECTION, matrice$N_PARCA)
  
  matrice <- merge(matrice, SEQUOIA::ADE_3.1_DEPARTEMENT, by.x = "DEP_CODE", by.y = "INSEE_DEP", all.x = T, all.y = F)
  matrice$REG_CODE <- matrice$INSEE_REG
  matrice$DEP_NOM <- matrice$NOM_M
  
  matrice <- matrice[, c("REG_CODE", "REG_NOM", "DEP_CODE", "DEP_NOM", "COM_CODE", "COM_NOM", 
                         "PROP", "ADRESSE", "IDU", "PREFIXE", "SECTION", "N_PARCA", 
                         "LIEUDIT", "OCCUP_SOL", "TX_BOISE", "REV_CA", "SURF_CA")]
  
  matrice <- unique(matrice)
  openxlsx::write.xlsx(matrice, paste(repout, paste0(value, "_matrice.xlsx"), sep="/"), overwrite = T)
  cat(paste("La matrice SEQUOIA du propriétaire a été enregistrée dans le repertoire : ", repout),"\n \n")
  
  rm(ppm, data)
  gc()
}



