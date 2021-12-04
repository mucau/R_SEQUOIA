#' @title HTMLtoXLSX
#' Creation d'un .xlsx a partir de matrices cadastrales .html
#' @encoding UTF-8
#' @description 
#' La fonction \code{HTMLtoXLSX} parcourt un dossier contenant des matrices cadastrales au format .html et génère un fichier .xlsx compilant l'ensemble des matrices.
#' @usage HTMLtoXLSX(rephtml, repRdata)
#' @param rephtml Répertoire du dossier contenant les matrices cadastrales .html  Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
#' @param repRdata Répertoire du fichier .Rdata contenant les données administratives Si \code{FALSE}, la fonction \code{\link{INSEEtoRDATA}} est lancée.
#' @return 
#' \item{_matrice.xlsx}{Tableur Excel. Matrice cadastrale}
#' \item{_subdi.xlsx}{Tableur Excel. Matrice cadastrale avec subdivision}
#' \item{XLSX}{Objet dataframe. Matrice cadastrale}
#' \item{rep.xlsx}{Character. Répertoire de sortie}
#' @seealso
#' L'archive .Rdata contenant les données administratives est produite par la fonction \code{\link{INSEEtoRDATA}}.
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   HTMLtoXLSX(rephtml=F, repRdata=F)
#' @export
#' 
#' @import tcltk dplyr stringr utils
#' @importFrom XML readHTMLTable
#' @importFrom RCurl getURL
#' @importFrom openxlsx write.xlsx 
#' @importFrom rlist list.clean
#' @importFrom stats xtabs 
#' @importFrom stats aggregate 

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("XML")) {install.packages("XML")}
# if (!require("tidyverse")) {install.packages("tidyverse")}
# if (!require("RCurl")) {install.packages("RCurl")}
# if (!require("rlist")) {install.packages("rlist")}
# if (!require("openxlsx")) {install.packages("openxlsx")}

HTMLtoXLSX <- function(rephtml=F,repRdata=F) {

# Import des données .html
  if(isFALSE(rephtml)){
    rephtml <- tk_choose.dir(default= getwd(),
                             caption = "Choisir le dossier contenant les matrices")
    }
  if (!length(rephtml)){stop("Pas de dossier sélectionnés > Traitement annulé","\n")}

  if(isFALSE(repRdata)) {
    SEQUOIA:::INSEEtoRDATA()
  } else {
    load(paste0(repRdata, "/INSEE.Rdata"))
  }

  LISTE_HTML <- list.files(rephtml, "*.html") # Création d'une liste de fichier .html

  if(length(LISTE_HTML > 0)){ # Si la liste contient au moins un fichiers
    cat(paste0("Le dossier choisi contient ",length(LISTE_HTML)," fichiers."),"\n")

    sortie <- data.frame() # Création d'un dataframe vierge de sortie
    sortie2 <- data.frame() # Création d'un dataframe vierge de sortie
    
    # Boucle de traitement
    for (i in LISTE_HTML) { # Pour chaque fichier .html de la liste
      urldata <- RCurl::getURL(paste("file:/",rephtml,i, sep="/")) # Chargement du fichier .html

      ## Récupération des données générales
      ### Lecture des listes d'en-tête
      data <- XML::readHTMLTable(urldata, header=F, stringsAsFactors = F) # Chargement des listes d'en-têtes
      data <- rlist::list.clean(data, fun = is.null, recursive = F)         # Suppression des listes vides

      ### Traitement sur la commune
      Dep_Code   <- str_sub(data[[1]][1, 4],1,2)
      Commune    <- data[[1]][1, 6]  # Récupération de cellule commune
      Com_Nom  <- str_sub(Commune,5) # Récupération du nom de la commune
      Com_Code <- as.numeric(str_sub(Commune,1,3)) # Récupération du code la commune
      Com_Code <- str_pad(Com_Code, 3, "left", pad = "0")

      ### Récupération des adresses et statuts du/des propriétaires
      COORD_list <- rlist::list.clean(data[[2]][, 1], fun = is.na, recursive = T) # Sélection de la liste contenant les adresses + statuts propriétaires

      ### Traitement sur l'adresse du/des propriétaires
      ADRESSE_list <- COORD_list
      n <- 1 # Ligne de traitement dans la liste
      for (k in 1:length(ADRESSE_list)){         # Ballayage de la liste
        if((grepl("[0-9]",ADRESSE_list[[n]])!=T)||(nchar(ADRESSE_list[[n]])<4)){ # Détection de l'absence de chiffre
          ADRESSE_list[[n]] <- NULL              # Suppression de ligne sans adresse
          if(n>length(ADRESSE_list)){n=length(ADRESSE_list)}
        } else {n=n+1}
      }

      Coord <- paste(ADRESSE_list,collapse = " & ")      # Concaténation des adresses dans une même variable
      Coord <- str_replace_all(Coord, "Â", "")           # Supression des caractères de Â
      Coord <- str_replace_all(Coord, "[:blank:]+", " ") # Suppression des caractères vides

      ### Traitement sur le statut
      STATUT_list <- COORD_list
      n <- 1 # Ligne de traitement dans la liste
      for (k in 1:length(STATUT_list)){         # Ballayage de la liste
        if((grepl("Indivision",STATUT_list[[n]])!=T)&(grepl("nu",STATUT_list[[n]])!=T)&(grepl("usu",STATUT_list[[n]])!=T)){ # Détection de l'absence de chiffre
          STATUT_list[[n]] <- NULL              # Suppression de ligne sans adresse
          if(n>length(STATUT_list)){n=length(STATUT_list)}
        } else {n=n+1}
      }

      ### Traitement sur l'adresse du/des propriétaires
      PROP_list <- rlist::list.clean(data[[2]][, 3], fun = is.na, recursive = T)  # Sélection de la liste contenant les propriétaires

      if(length(ADRESSE_list)>1){ # Quand la liste contient plusieurs noms
        n <- 1 # Ligne de traitement dans la liste
        Nom <- ""
        for (k in 1:length(PROP_list)){         # Ballayage de la liste
          if(grepl("/",PROP_list[[n]])!=T){ # Détection de l'absence de chiffre
            PROP_list[[n]] <- NULL              # Suppression de ligne sans adresse
            if(n>length(PROP_list)){n=length(PROP_list)}
          } else {n=n+1}
        }

        Statut <- list() # Création d'une liste vierge
        for (k in 1:length(PROP_list)) {
          Statut <- c(Statut,paste(PROP_list[k],"(",str_replace(STATUT_list[k],"Ã©","é"),")"))} # Agrégation des noms avec leur statuts

        Nom <- paste(Statut,collapse = " & ") # Concaténation des noms+statut dans une même variable
      } else {  # Quand la liste contient un seul nom
        Nom <- paste(PROP_list[[1]],collapse = " & ") # Sélection du nom
      }

      ## Récupération des données cadastrales
      ### Lecture des listes du corps
      data <- XML::readHTMLTable(urldata, stringsAsFactors = F)   # Chargement des listes concernées
      data <- rlist::list.clean(data, fun = is.null, recursive = F) # Suppression des listes vides

      df <- data.frame() # Création d'un tableau de données

      ### Sélection des colonnes nommées ci-dessous
      for (j in 1:length(data)){       # Détecte le nombre de liste
        t1 <- as.data.frame(data[[j]])
        #### Détecte si la liste contient des données foncières
        if("LIVRE FONCIER" %in% unlist(t1)){
          t1 <- t1 %>%   # Inscription dans un dataframe
            dplyr::select(2,3,5,10, 11,14,15) %>%  # Sélection des colonnes inétressantes
            filter(row_number() > 2)      # Sélection des lignes hors des entêtes
          names(t1) <- c("SECTION","N_PARCA","LIEUDIT","SUB","NATURE","SURFACE","REV_CAD") # Changement des noms des champs

          t1 <- t1 %>% # Suppression des lignes dont la contenance contient des tags .html
            filter(!is.na(SURFACE)) %>%
            filter(SECTION != "SECTION")

          df <- rbind(df, t1) # Mise du dataframe dans le précédent
        }
      }
      df <- df%>%
        filter(!str_detect(SURFACE, "Â"))
      
      ### Inscription des données de la parcelle à ses subdivisions
      for (k in 1:nrow(df)){
        if (is.na(as.numeric(df[k,2]))){
          df[k, 1:3] <- df[k-1, 1:3]
          df[k, 7] <-str_replace(df[k, 7],",",".")
        } else {
          df[k, 4] <-str_replace(df[k, 4],"Â","")
          df[k, 7] <-str_replace(df[k, 7],"Â","")
        }
      }

      ### Inscription des données générales
      df <- df %>%
        filter(!str_detect(NATURE, "Â")) %>% # Conservation des seules subdivisions fiscales
        mutate(DEP_CODE = Dep_Code,         # Création du champs DEP_CODE
               COM_CODE = Com_Code,         # Création du champs COM_CODE
               COM_NOM  = Com_Nom,          # Création du champs COM_NOM
               PROP     = Nom,              # Création du champs PROP
               ADRESSE  = Coord) %>%        # Création du champs ADRESSE
        dplyr::select(DEP_CODE, COM_CODE,COM_NOM,PROP,ADRESSE,SECTION:LIEUDIT, SUB, NATURE,REV_CAD,SURFACE) # Mise en ordre des champs

      ### Création de la dataframes de données des subdivisions fiscales
      tab <- data.frame()   # Création d'un tableau de données vierge
      tab <- rbind(tab, df) # Combinaison des données récoltées

      ### Si la matrice ne contient qu'une parcelle
      if (nrow(tab)<2){ # /!\ Génération d'une ligne fictive pour conservation du type numeric pour la variable surface
        tab[2,]<-"999"  # /!\ Conversion en unknow et traitement suivant impossible sinon
      }

      ## Traitement de la table (avec subdivision) ---
      ### Mise au format des variables SURFACES, REVCAD, N_PARCA + Création des variables PREFIXE et GROUPE
      tab <- tab %>%
        mutate(SURFACE   = as.numeric(paste(str_replace_all(SURFACE, " ", "")))/10000, # Conversion de SURF_CA en données numériques exprimées en Ha
               REV_CAD   = as.numeric(gsub("[,]",".", REV_CAD)),     # Conversion de REV_CAD en données numériques
               N_PARCA   = str_pad(N_PARCA, 4, "left", pad = "0"),   # Complément de N_PARCA avec "0"
               PREFIXE   = "000",                                    # Création du champ PREFIXE
               GROUPE    = "") %>%
        dplyr::select(DEP_CODE:ADRESSE,PREFIXE,SECTION:LIEUDIT,GROUPE, SUB, NATURE:SURFACE) # Mise en ordre des champs

      ### Calcul des variables PREFIXE et SECTION
      for (k in 1:nrow(tab)){                            # Boucle sur les lignes
        if (grepl("[0-9]",tab$SECTION[k])==T){                 # Lorsqu'un préfixe est détecté, sépration du préfixe et des champs
          tab$PREFIXE[k] = str_split_fixed(tab$SECTION[k]," ",2)[1]  # Déplacement du préfixe dans le champ dédié
          tab$SECTION[k] = str_split_fixed(tab$SECTION[k]," ",2)[2]  # Déplacement de la section dans le camps dédié
        }
        if (str_sub(tab$NATURE[k],1,1)=="B"){                # Lorsque la subdivision fiscale est de type boisée
          tab$GROUPE[k] = "BOISEE" } else { tab$GROUPE[k]="NON_BOISEE" # Inscription de cetype dans GROUPE
          }
      }

      ### Mise au format des variables SECTION + Création de la variable d'identifiant unique
      tab <- tab %>%
        mutate(SECTION = str_pad(SECTION, 2, "left", pad = "0"),         # Complément de SECTION avec "0"
               IDU = paste(COM_CODE,PREFIXE,SECTION,N_PARCA,sep="")) %>% # Création de l'identifiant unique
        dplyr::select(DEP_CODE:ADRESSE,IDU,PREFIXE:SURFACE)           # Mise en ordre des champs

      # /!\ tab contient l'ensemble des données des subdivisions cadastrales. Les parcelles n'y sont pas présentes

      ## Création de la matrice finale (sans subdivisions)
      ### Création d'un tableau récapitualtif des SURFACE par GROUPE par IDU
      tab2 <- as.data.frame.matrix(stats::xtabs(SURFACE ~ IDU + GROUPE, tab)) # dataframe SURFACE par GROUPE par IDU

      ### Ajout de la colonne manquante en cas d'absence de données
      if(ncol(tab2)<2){
        if(grepl("NON",names(tab2))) {  # Si la table ne contient qu'une colonne au lieu des deux attendus
          tab2$BOISEE<- as.numeric(0.0) # Création de la variable BOISEE
        } else {
          tab2$NON_BOISEE<- as.numeric(0.0)} # Création de la variable NON_BOISEE
      }

      ### Ajout de l'IDU + Calcul de la surface cadastrale + Création d'un champs d'occupation du sol
      tab2$IDU <- row.names(tab2) # Création du champ IDU dans la nouvelle table
      tab2$SURF_CA <- as.numeric(tab2$BOISEE+tab2$NON_BOISEE) # Détermination de la surface cadastrale totale SURF_CA
      tab2$OCCUP_SOL <- ""        # Création d'un champ équivalent à GROUPE

      ### Détermination de la nature de la propriété
      Seuil <- 0.5 # Proportion de surface au delà de laquelle la parcelle est considérée comme forestière
      for (k in 1:nrow(tab2)) {
        if ((as.numeric(tab2$BOISEE[k])/tab2$SURF_CA[k])>=Seuil){ # Lorsque la proportion de surface boisée dépasse 50% de la surface cadastrale totale
          tab2[k,5] = "BOISEE" # la parcelle est jugée boisée
          tab2$TX_BOISE[k] <- (as.numeric(tab2$BOISEE[k])/tab2$SURF_CA[k])
        } else {
          tab2[k,5] = "NON BOISEE"
          tab2$TX_BOISE[k] <- (as.numeric(tab2$BOISEE[k])/tab2$SURF_CA[k])} # ou non boisée sinon
      }
      tab2 <- tab2[,-c(1:2)] # Suppression des champs GROUPE

      ### Création d'un tableau récapitualtif du REV_CAD par IDU
      tab3 <- stats::aggregate(data=tab[,c("IDU","REV_CAD")], .~IDU, FUN=sum)
      names(tab3) <- c("IDU","REV_CA")

      ### Création du tableur de sortie
      matrice <- merge(x = tab, y = tab2, by = "IDU", all.y = TRUE) # Jointure des tables 1 et 2
      matrice <- merge(x = matrice, y = tab3, by = "IDU", all.y = TRUE) # Jointure des tables 1/2 et 3
      matrice <- matrice[,-c(11,12,13,14,15)] # Suppression des champs liés au subdivisions : NATURE, GROUPE, SURFACE, REV_CAD

      names(INSEE_DEPS)<-c("REG_CODE","DEP_CODE","CHEFLIEU","TNCC","DEP_NOM","DEP_NOMI")
      INSEE_DEPS <- INSEE_DEPS %>%
        mutate(DEP_CODE = str_pad(DEP_CODE, 2, "left", pad = "0"))
      matrice <- merge(x = matrice, y=INSEE_DEPS[,c(1,2,5)], by.x = "DEP_CODE",by.y= , all.y = FALSE)
      names(INSEE_REGS)<-c("REG_CODE","CHEFLIEU","TNCC","REG_NOM","REG_NOMI")
      matrice <- merge(x = matrice, y=INSEE_REGS[,c(1,4)], by="REG_CODE",all.y = FALSE)

      matrice <- unique(matrice) %>% # Suppression des doublons
        filter(COM_CODE!="999") %>%  # Suppression de la ligne crée dans le cas d'une matrice à une parcelle
        dplyr::select(
          REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU,PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs

      # /!\  tab contient l'ensemble des données des parcelles cadastrales. Les subdivisions n'y sont plus présentes
      # /!\  si on souhaite exporté tab et non matrice --> remplacement de la ligne suivante par la seconde:

      if (is.null(nrow(sortie))){sortie=matrice}else{sortie = rbind(sortie, matrice)} # Concatene dans sortie l'ensemble des matrices
      if (is.null(nrow(sortie2))){sortie2=tab}else{sortie2 = rbind(sortie2, tab)}
      cat(paste("Traitement terminé du fichier", i),"\n")
    }

    ## Sortie définitive d'un .xlsx
    if (Sys.info()["sysname"]=="Windows"){
      NAME <- utils::winDialogString("Entrer le nom du fichier de sortie:", "")
    }else {
      NAME <- readline(prompt="Entrer le nom du fichier de sortie:")
    }
    
    repOut <- paste(rephtml, paste0(NAME,"_matrice.xlsx"), sep="/")
    cat(paste("Liste parcelles cadastrales enregistree dans le repertoire : ", repOut),"\n")
    #Encoding(sortie) <- "latin1"
    openxlsx::write.xlsx(sortie, repOut, overwrite = T)
    repOut <- paste(rephtml, paste0(NAME,"_subdi.xlsx"), sep="/")
    openxlsx::write.xlsx(sortie2, repOut, overwrite = T)
    cat(paste("Liste parcelles cadastrales enregistree dans le repertoire : ", repOut),"\n")

    assign("XLSX",sortie,envir=globalenv())
    assign("rep.xlsx",repOut,envir=globalenv())
  }
}

