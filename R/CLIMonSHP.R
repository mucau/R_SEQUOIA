# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("dplyr")) {install.packages("dplyr")}

CLIMonSHP <- function(shp = F){
  message("- - - Création d'une fiche Climatologique - - -")
  if(isFALSE(shp)){
    shp  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .shp",
                                   filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Pas de shapefile sélectionné")}

  # Bloc Aurelhy
  AURELHY <- winDialog(type = "yesno", "Voulez-vous intégrer les données AURELHY ?")
  if(AURELHY == "YES"){
    if(exists("repRdata")) {Rdata <- repRdata} else {
      Rdata <- tcltk::tk_choose.dir(caption = "Choisir le dossier contenant les données AURELHY.Rdata")
      if(is.na(Rdata)){stop("Pas de dossier sélectionné")}
    }
  } else {Rdata<-""}

  # Bloc DRIAS
  DRIAS <- winDialog(type = "yesno", "Voulez-vous intégrer les données DRIAS ?")
  if(DRIAS == "YES"){
    txt  <- tcltk::tk_choose.files(default = "~", caption = "Selectionner le fichier .txt de donnée",
                                   filter = matrix(c("Fichier texte", ".txt"), 1, 2, byrow = TRUE))
    if (!length(txt)){stop("Pas de txt sélectionné")}
  } else {txt<-""}

  # Répertoire
  dsn <- setwd("~/")
  ask <- winDialog(type = "yesno", paste0("Le fichier sera exporté dans ", dsn, "\nC'est bon pour vous ?"))
  if(ask == "NO") {
    dsn <- tcltk::tk_choose.dir(default = dirname(shp), caption = "Choisir le dossier de destination")
    if(is.na(dsn)){dsn <- setwd("~/")}
  }

  # Nom de fichier
  name <- winDialogString("Entrer le nom du fichier de sortie: ", "")
  if(!length(name)){name <- paste0("Fiche_du_", format(Sys.time(), '%y.%m.%d'))}

  # Affectation du wd
  setwd("~/")
  save(shp, Rdata, txt, name, file = "PREPA-FICHE.Rdata")

  # Lancement du Rmarkdown
  library(SEQUOIA)
  rep <- path.package("SEQUOIA", quiet = FALSE)
  rmd <- paste(rep, "CLIM.Rmd", sep='/')
  rmarkdown::render(rmd, prettydoc::html_pretty(theme = "cayman"),
                    paste0(name,"_FICHE-CLIM.html"),
                    dsn, encoding = "UTF-8")

  # Export des tableurs de données
  if (AURELHY=="NO" & DRIAS=="NO"){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp)
  }
  if (AURELHY!="NO" & DRIAS=="NO"){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "AURELHY_ombro"=AURELHY_ombro,
                   "AURELHY_etp"=AURELHY_etp)
  }
  if (AURELHY=="NO" & DRIAS!="NO"){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "DRIAS_ombro"=DRIAS_ombro,
                   "DRIAS_etp"=DRIAS_etp)
  }
  if (AURELHY!="NO" & DRIAS!="NO"){
    export <- list("METEOFRANCE_ombro" = METEOFRANCE_ombro,
                   "METEOFRANCE_etp" = METEOFRANCE_etp,
                   "AURELHY_ombro"=AURELHY_ombro,
                   "AURELHY_etp"=AURELHY_etp,
                   "DRIAS_ombro"=DRIAS_ombro,
                   "DRIAS_etp"=DRIAS_etp)
  }
  openxlsx::write.xlsx(export, paste(dsn, paste0(name, "_climat.xlsx"),sep="/"))
}
