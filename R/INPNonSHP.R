#' @title INPNonSHP
#' Detection des enjeux environnementaux sur une zone d'etude
#' @encoding UTF-8
#' @description 
#' La fonction \code{INPNonSHP} recherche les intersections entre une zone d' étude (.shp quelconque) et les zonnages environnementaux.
#' Pour chaque enjeux, une couche .shp contenant les zones intersectant la zone d'étude est exportée.
#' @usage INPNonSHP(shp)
#' @param shp Adresse du \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @param NAME Préfixe des fichers shapefile générés.
#' @details 
#' La fonction peut réclamer une archive .Rdata externe produite par la fonction \code{\link{INPNtoRDATA}}.
#' @return
#' La fontion retourne un ensemble de .shp dans le dossier source.
#' @references Vous pouvez retrouvé les données environnementales sur le site de l'Institut national pour la protection de la nature (INPN) : \url{https://inpn.mnhn.fr/accueil/index}
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   INPNonSHP(shp=F)
#' @export
#' 
#' @import tcltk

# Lancement des library
# library(tcltk)

INPNonSHP <- function(shp=F, source_inpn=F){
  # Lecture du shapefile
  if(isFALSE(shp)) {
    shp  <- tk_choose.files(caption = "Choisir le fichier .shp",
                            filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if (!length(shp)){stop("Aucune sélection effectuée > Traitement annulé \n")}
  
  # Source de données
  message('\n        Choix de la source') 
  if(isFALSE(source_inpn)){
    form <- c("1 web IGN©",
              "2 loc INPN© rdata")
    
    source_inpn <- select.list(form,
                               multiple = F,
                               title = "Choix de la source cadastrale",
                               graphics = T)
  }
  if (source_inpn==""){stop("Aucune source sélectionnée !")}
  cat(paste0("        ", source_inpn, " retenu \n\n"))
  
  if ("1 web IGN©" %in% source_inpn){
    INPNbyIGNweb(shp)
  }
  
  if ("2 loc INPN© rdata" %in% source_inpn){
    if(exists("repRdata")){
      INPNbyRdata(shp, repRdata)
    } else {
      INPNbyRdata(shp, F)
    }
  }
}


