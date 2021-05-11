# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}

IFNtoSHP <- function(code) {
  if(code=="RFN") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/rn250_l93_shp-2.zip"
  }
  if(code=="RFD") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/rf250_l93_shp-2.zip"
  }
  if(code=="SER") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/ser_l93.zip"
  }
  if(code=="SERAR") {
    URL <- "https://inventaire-forestier.ign.fr/IMG/zip/ser_ar_l93.zip"
  }
  if(is_empty(code)) {
    warning("Aucun code entré >> Traitement annulé.")
  } else {
    TF <- tempfile(fileext = ".zip")
    download.file(URL, TF, method="libcurl")
    OUT <- unzip(TF, exdir = tempdir())
    A <- grep(".shp$", OUT)
    SHP <- st_read(OUT[A], quiet=T)
    SHP <- st_transform(SHP, crs = 2154)
    return(SHP)
  }
}

# Not run
# RFN <- IGNtoSHP('RFN')
