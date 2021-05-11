# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("dplyr")) {install.packages("dplyr")}

SSPFinUA <- function(rep=F){
  if(isFALSE(rep)) {
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(rep)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  UA <- st_read(rep, stringsAsFactors=F, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  NAME <- str_sub(rep,
                  str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_UA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())
  
  
  UARV<-UA[0,colnames(UA)]
  PF <- unique(UA$N_PARFOR)
  
  
  for (a in 1:length(PF)){
    print(paste0('Parcelle ', PF[a]))
    UAPF <- UA %>%
      filter(N_PARFOR==PF[a])
    PLTS <- unique(UAPF$PLT_TYPE)
    print(paste0(length(PLTS), ' peuplements'))
    r=1
    
    for (b in 1:length(PLTS)){
      UAPLTS <- UAPF %>%
        filter(PLT_TYPE==PLTS[b])
      ESS <- unique(UAPLTS$PLT_ESS)
      
      if (length(ESS)>1) {
        for (c in 1:length(ESS)){
          UAESS <- UAPLTS %>%
            filter(PLT_ESS==ESS[c])
          UAESS <- UAESS %>%
            mutate(N_SSPARFOR=r)
          r=r+1
          UARV <- rbind(UARV, UAESS)
        }
      }else {
        UAPLTS <- UAPLTS %>%
          mutate(N_SSPARFOR=r)
        r=r+1
        UARV <- rbind(UARV, UAPLTS)
      } #end if
    }
  }
  if(nrow(UARV)==nrow(UA)){SEQUOIA:::WRITE(UARV, getwd(), paste(NAME,"PROP_point.shp"))}
}
