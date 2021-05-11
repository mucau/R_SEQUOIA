# Lancement des library
if (!require("foreign")) {install.packages("foreign")}

INSEEtoRDATA <- function(rep=NULL){

# Adresses de téléchargement et de destination
  list_URL <- list()
  list_URL[[1]] <- "https://www.data.gouv.fr/en/datasets/r/554590ab-ae62-40ac-8353-ee75162c05ee"
  list_URL[[2]] <- "https://www.insee.fr/fr/statistiques/fichier/3363419/reg2018-dbf.zip"
  list_URL[[3]] <- "https://www.insee.fr/fr/statistiques/fichier/3363419/depts2018-dbf.zip"
  list_URL[[4]] <- "https://www.insee.fr/fr/statistiques/fichier/3363419/comsimp2018-dbf.zip"

# Téléchargement des données postales
  TF = tempfile(tmpdir=tempdir(), fileext=".csv") # fichier temporaire
  download.file(list_URL[[1]], TF, method="libcurl") # Téléchargement du fichier URL sous le nom TF dans TD

  INSEE_POST <- read.csv(TF, header = TRUE, sep = ";")

# Téléchargement des données INSEE
  for (a in 2:length(list_URL)) {
    TD = tempdir()
    URL <- list_URL[[a]]
    TF = tempfile(tmpdir=TD, fileext=".zip") # fichier temporaire
    download.file(URL, TF, method="libcurl") # Téléchargement du fichier URL sous le nom TF dans TD
    unzip(zipfile = TF, exdir=TD) # Extraction de TF dans TD
  }

  list_dbf <- list.files(TD, "*.dbf$")

  INSEE_REGS <- foreign::read.dbf(paste(TD, list_dbf[grep("reg",list_dbf)], sep="/"))
  INSEE_DEPS <- foreign::read.dbf(paste(TD, list_dbf[grep("dep",list_dbf)], sep="/"))
  INSEE_COMS <- foreign::read.dbf(paste(TD, list_dbf[grep("com",list_dbf)], sep="/"))

  # Ajout des données à l'environnement
  assign("INSEE_POST", INSEE_POST,envir=globalenv())
  assign("INSEE_REGS", INSEE_REGS,envir=globalenv())
  assign("INSEE_DEPS", INSEE_DEPS,envir=globalenv())
  assign("INSEE_COMS", INSEE_COMS,envir=globalenv())

  # Enrigistrement des fichiers sous un .Rdata
  if(!is.null(rep)){
    save(INSEE_POST, INSEE_REGS, INSEE_DEPS, INSEE_COMS, file=paste(rep,"INSEE.Rdata",sep="/"))
    cat("Les fichiers téléchargés ont été sauvegardés dans ", rep, "\n")
  }
}



