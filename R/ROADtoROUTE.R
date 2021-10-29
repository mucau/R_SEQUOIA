#' @title ROADtoROUTE
#' Conversion de ROAD vers ROUTE
#' @encoding UTF-8
#' @description 
#' La fonction \code{ROADtoROUTE} génére les .shp (EPSG 2154) polygone et ligne à partir de ROAD_polygon (vides cadastraux) ou de ROAD_line (routes référencées dans l'OSM ou la BDTOPO)
#' @usage ROADtoROUTE(road)
#' @param road CHARACTER. Adresse du fichier \code{.shp} ROAD_polygon.shp ou ROAD_line.shp. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
#' @return
#' Les fichiers .shp suivants sont produits :
#' \item{ROUTE_polygon}{Fichier shapefile ; fonds routes}
#' \item{ROUTE_line}{Fichier shapefile ; contours routes}
#' @details Le shapefile road d'entrée doit disposer d'une table attributaire complétée. Le champ "TYPE" doit correspondre à la nomenclature suivante =
#' TYPE='RN' : routes nationnales & autoroutes, TYPE='RD' : routes départementales,  TYPE='RC' : routes revêtues non départementales, TYPE='RF' : les routes empierrées/forestières, TYPE='PN' : les pistes en terrain naturel
#' Dans ROAD_polygon.shp, les codes TYPE='CR' pour chemins ruraux et TYPE='CC' pour chemins privés cadastrés sont acceptés en plus des codes précédents.
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   ROADtoROUTE(road = F)
#' @export
#' 
#' @import tcltk dplyr stringr sf

# Lancement des library
# if (!require("tcltk")) {install.packages("tcltk")}
# if (!require("sf")) {install.packages("sf")}
# if (!require("dplyr")) {install.packages("dplyr")}
# if (!require("stringr")) {install.packages("stringr")}
# if (!require("lwgeom")) {install.packages("lwgeom")}

ROADtoROUTE <- function(road=F){
  options(warn=-1) #désactivation des warnings

  message('- - - Conversion de ROAD vers ROUTE - - -')
  if(isFALSE(road)) {
    road<-tk_choose.files(caption = "Choisir le fichier.shp ROAD_ line ou polygon",
                          filter = matrix(c("ESRI Shapefile", ".shp"), 1, 2, byrow = TRUE))
  }
  if(!length(road)){stop("        Aucun fichier sélectionné >> Traitement annulé")}

  # Import de la source de donnée
  message('        Lecture des données')
  ROAD <- st_read(road, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  assign("ROAD", ROAD, envir=globalenv())

  rep <- road
  NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,
                  str_locate(rep,'_ROAD')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())
  cat('        Le shapefile a été chargé avec succès \n \n')

  # Chargement 1. Cadastre
  if (grepl('polygon', road)){ # Boucle 1. Cadastre
    message('        Conversion de ROAD_polygon')
    # Lecture du fichier
    ROAD_polygon <- ROAD
    cat('        ROAD_polygon devient ROUTE_polygon \n')

    # Conversion de ROAD_polygon vers ROUTE_line
    ROUTE_line <- st_cast(ROAD_polygon, 'MULTILINESTRING') # Linéarisation du polygon
    cat('        lignes: linéarisation effectuée \n')
    ROUTE_line <- st_difference(ROUTE_line, st_buffer(st_union(ROAD_polygon), -0.0001)) # Suppression des recouvrements
    cat('        lignes: débordements supprimés \n')

    # Pré-découpage des bordures
    CONV_polygon <- st_simplify(st_convex_hull(ROAD_polygon), preserveTopology = T, dTolerance = 2)
    CONV_point  <- st_cast(st_combine(CONV_polygon),'MULTIPOINT', warn = F)
    ROUTE_line <- st_split(st_cast(ROUTE_line,'MULTILINESTRING'), CONV_point)
    ROUTE_line <- st_collection_extract(ROUTE_line,'LINESTRING')

    # Nettoyage de la table
    ROUTE_line$LENGTH <- as.numeric(st_length(ROUTE_line)) # Ajouter un champ longueur

    d=0.1
    #units(d) = "m"
    ROUTE_line<- ROUTE_line%>%
      filter(!(ROUTE_line$LENGTH < d))
    cat('        lignes: découpe réalisée \n')
    cat('        ROUTE_line a été crée \n \n')

    SEQUOIA:::WRITE(ROUTE_line, repout2, paste(NAME,"ROUTE_line.shp",sep="_"))
    SEQUOIA:::WRITE(ROAD_polygon, repout2, paste(NAME,"ROUTE_polygon.shp",sep="_"))
  } # Fin Boucle 1. Cadastre

  # Chargement 2. OSM
  if (grepl('line', road)){ # Boucle 2. OSM
    message('        Conversion de ROAD_line')
    # Lecture du fichier
    ROAD_line <- ROAD

    # Fonctions
    getline <- function (type, B){
      line <- st_zm(ROAD_line %>% filter(TYPE %in% type), drop = TRUE, what = "ZM")
      polygon <- st_buffer(line, B, 5, endCapStyle='ROUND')
      polygon <- st_simplify(polygon, preserveTopology = TRUE, dTolerance = 0.1)
      line <- st_cast(polygon, 'MULTILINESTRING')
    }

    getpolygon <- function(type, B){
      line <- st_zm(ROAD_line %>% filter(TYPE %in% type), drop = TRUE, what = "ZM")
      #line <- st_union(line)
      polygon <- st_buffer(line, B, 5, endCapStyle='ROUND')
      polygon <- st_simplify(polygon, preserveTopology = TRUE, dTolerance = 0.1)
      polygon <- st_sf(st_union(polygon)) %>%
        mutate(TYPE=type)
      return(polygon)
    }

    # Entrée des paramètres
    cat('        Entrée des paramètres \n \n')
    if (Sys.info()["sysname"]=="Windows"){
      B1 <- as.numeric(winDialogString("Tempon RN+RD:",default = ""))
      B2 <- as.numeric(winDialogString("Tempon RC+RF:",default = ""))
    }else {
      B1 <- as.numeric(readline(prompt="Tempon RN+RD:"))
      B2 <- as.numeric(readline(prompt="Tempon RC+RF:"))
    }

    # Création des buffers
    RN_buffer <- getpolygon(type="RN", B=B1+1)[, ]
    RD_buffer <- getpolygon(type="RD", B=B1)[, ]
    RC_buffer <- getpolygon(type="RC", B=B2)[, ]
    RF_buffer <- getpolygon(type="RF", B=B2)[, ]
    ROUTE_buffer1 <- rbind(RN_buffer, RD_buffer, RC_buffer, RF_buffer)
    cat('        polygones: buffers crées \n')

    # Supression des débordements
    decoupe <- function(x, y){
      if(isTRUE(nrow(x)>0) & isTRUE(nrow(y)>0)){
        polygon <- st_difference(x, y) %>% dplyr::select(TYPE)
      } else {polygon <- x}
      return(polygon)
    }
    RD_buffer <- decoupe(RD_buffer, RN_buffer)
    RC_buffer <- decoupe(decoupe(RC_buffer, RN_buffer), RD_buffer)
    RF_buffer <- decoupe(decoupe(decoupe(RF_buffer, RN_buffer), RD_buffer), RC_buffer)
    ROUTE_buffer <- st_cast(rbind(RN_buffer, RD_buffer, RC_buffer, RF_buffer), "MULTIPOLYGON") %>% st_cast("POLYGON")
    cat('        polygones: débordements supprimés \n')

    # Création du buffer de découpe des extrémités
    BUFFER_line <- ROAD_line %>% filter(TYPE %in% c("RN", "RD", "RC", "RF"))
    BUFFER_line <- BUFFER_line %>% mutate(id=1, length=as.numeric(st_length(BUFFER_line)))
    BUFFER_line <- aggregate(x = BUFFER_line[, "length"], by = list(BUFFER_line$id),
                             FUN = sum, na.rm = TRUE)

    BUFFER_polygon1 <- st_buffer(BUFFER_line, B1+4, nQuadSegs = 5, endCapStyle = 'FLAT',
                                 joinStyle = "ROUND") %>% mutate(buffer=1) %>% select(buffer)
    BUFFER_polygon2 <- st_buffer(BUFFER_line, B1+2, nQuadSegs = 5, endCapStyle = 'ROUND',
                                 joinStyle = "ROUND") %>% mutate(buffer=2) %>% select(buffer)
    BUFFER_polygon3 <- st_buffer(BUFFER_line, B1+2, nQuadSegs = 5, endCapStyle = 'FLAT',
                                 joinStyle = "ROUND") %>% mutate(buffer=1) %>% select(buffer)
    BUFFER_polygon <- st_sym_difference(BUFFER_polygon1, BUFFER_polygon2) %>% st_cast("POLYGON")
    BUFFER_polygon <- st_join(BUFFER_polygon, st_buffer(BUFFER_line, 2)%>%mutate(join=1)%>%select(join)) %>%
      filter(join==1) %>% mutate(area=as.numeric(st_area(.))) %>% filter(area<=50) %>% select(buffer)

    BUFFER_polygon <- st_make_valid(rbind(BUFFER_polygon, BUFFER_polygon3))
    BUFFER_polygon <- st_make_valid(st_union(BUFFER_polygon))
    BUFFER_polygon <- st_make_valid(st_buffer(BUFFER_polygon, 0.05))

    # Création de ROUTE_polygon
    ROUTE_polygon <- st_make_valid(st_intersection(st_make_valid(ROUTE_buffer),
                                     st_make_valid(BUFFER_polygon))) %>% dplyr::select(TYPE)
    # SEQUOIA:::WRITE(ROUTE_buffer, repout2, paste(NAME,"ROUTE_buffer.shp",sep="_"))
    # SEQUOIA:::WRITE(BUFFER_polygon, repout2, paste(NAME,"BUFFER_polygon.shp",sep="_"))

    cat('        polygones: découpe réalisée \n')
    cat('        ROUTE_polygon a été crée \n \n')

    # Création de ROUTE_line
    RN_line <- getline(type="RN", B1+1)[, ]
    RD_line <- getline(type="RD", B1)[, ]
    RC_line <- getline(type="RC", B2)[, ]
    RF_line <- getline(type="RF", B2)[, ]
    PN_line <- ROAD_line %>% filter(TYPE %in% "PN")
    ROUTE_line <- rbind(RN_line, RD_line, RC_line, RF_line)
    cat('        lignes: buffers crées \n')

    PN_line <- st_difference(PN_line, st_buffer(st_union(ROUTE_buffer1), -0.01))
    ROUTE_line <- st_difference(ROUTE_line, st_buffer(st_union(ROUTE_buffer1), -0.001))
    cat('        lignes: débordements supprimés \n')

    ROUTE_line <- st_intersection(ROUTE_line, st_buffer(st_union(ROUTE_polygon), 0.001))
    ROUTE_line <- rbind(ROUTE_line, PN_line)
    point   <- st_collection_extract(st_intersection(ROUTE_line, st_cast(ROUTE_line, "MULTILINESTRING", group_or_split = FALSE)), "POINT")
    
    ROUTE_line <- st_difference(ROUTE_line, st_union(st_buffer(point, 0.00001)))
    ROUTE_line <- st_cast(ROUTE_line, 'MULTILINESTRING') %>% st_cast('LINESTRING')
  
    ROUTE_line <- ROUTE_line %>% mutate(length = as.numeric(st_length(ROUTE_line)), DECA=as.character(NA)) %>%
      filter(length>=0.05) 
    
    ROUTE_line <- st_snap(ROUTE_line, point, 0.0001)
    
    ROUTE_line <- ROUTE_line %>%
      group_by(TYPE, NATURE, NAME) %>% 
      summarise_each(funs(mean)) %>% 
      dplyr::select(TYPE, NATURE, NAME, DECA)
    
    cat('        lignes: découpe réalisée \n')
    cat('        ROUTE_line a été crée \n \n')

    # Sorties des fichiers
    message('        Export des fichiers')
    SEQUOIA:::WRITE(point, repout2, paste(NAME,"ROUTE_point.shp",sep="_"))
    SEQUOIA:::WRITE(ROUTE_polygon, repout2, paste(NAME,"ROUTE_polygon.shp",sep="_"))
    SEQUOIA:::WRITE(ROUTE_line, repout2, paste(NAME,"ROUTE_line.shp",sep="_"))
    SEQUOIA:::WRITE(test4, repout2, paste(NAME,"ROUTE-test_line.shp",sep="_"))
    
  } # Fin Boucle 2. OSM

  # Suppression débordement INFRA_line
  repINFRA_line <- str_replace(road,"ROAD","INFRA")

  repINFRA_line <- str_replace(repINFRA_line,"polygon","line")
  INFRA_line <- st_read(repINFRA_line, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  INFRA_line <- st_difference(INFRA_line, st_make_valid(st_buffer(st_combine(ROUTE_polygon),2)))
  SEQUOIA:::WRITE(INFRA_line, repout2, paste(NAME,"INFRA_line.shp",sep="_")) # Export du shapefile

  repINFRA_line <- str_replace(repINFRA_line,"line","polygon")
  INFRA_polygon <- st_read(repINFRA_line, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  INFRA_polygon <- st_difference(INFRA_polygon, st_make_valid(st_buffer(st_combine(ROUTE_polygon),2)))
  SEQUOIA:::WRITE(INFRA_polygon, repout2, paste(NAME,"INFRA_polygon.shp",sep="_")) # Export du shapefile

  options(warn=1) # Activation des warnings
} # Fin fonction
