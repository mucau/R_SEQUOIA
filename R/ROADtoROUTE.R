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
#' @note 
#' Les fichiers ROAD_line et ROAD_polygon sont produits par \code{\link{CAGEF}}.
#' @author Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
#' @examples 
#' ### Fonctionnement :
#'   ROADtoROUTE(road = F)
#' @export
#' 
#' @import tcltk sf lwgeom

# Lancement des library
# library(tcltk)
# library(sf)
# library(lwgeom)

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
  NAMEofSHP(road)
  if (isFALSE(SEQ)){stop("Répertoire SEQUOIA non detecté")}
  
  ROAD <- st_read(road, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  assign("ROAD", ROAD, envir=globalenv())
  cat('        Le shapefile a été chargé avec succès \n \n')
  
  # write function
  write <- function(nom, rep, name=NULL){
    if (is.null(name)){
      name <- paste0(NAME, "_", deparse(substitute(nom)), ".shp")
    } else {
      name <-  paste0(NAME, "_", name, ".shp")
    }
    st_write(nom, rep, name, append=FALSE, delete_layer=TRUE, driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
    cat(paste("        Le fichier", name, "a été exporté dans", rep),"\n")
  }

  # Chargement 1. ROAD_polygon
  if (grepl('polygon', road)){ # Boucle 1. Cadastre
    message('        Conversion de ROAD_polygon')
    # Lecture du fichier
    ROUTE_polygon <- ROAD
    cat('        ROAD_polygon devient ROUTE_polygon \n')

    # Conversion de ROUTE_polygon vers ROUTE_line
    ROUTE_line <- st_cast(ROUTE_polygon, 'MULTILINESTRING') # Linéarisation du polygon
    cat('        lignes: linéarisation effectuée \n')
    ROUTE_line <- st_difference(ROUTE_line, st_buffer(st_union(ROUTE_polygon), -0.0001)) # Suppression des recouvrements
    cat('        lignes: débordements supprimés \n')

    # Pré-découpage des bordures
    CONV_polygon <- st_simplify(st_convex_hull(ROUTE_polygon), preserveTopology = TRUE, dTolerance = 2)
    CONV_point  <- st_cast(st_combine(CONV_polygon),'MULTIPOINT', warn = F)
    ROUTE_line <- st_split(st_cast(ROUTE_line,'MULTILINESTRING'), CONV_point)
    ROUTE_line <- st_collection_extract(ROUTE_line,'LINESTRING')
    
    # Import 'PN' depuis IGN BDTopo
    road2 <- gsub("polygon", "line", road)
    ROAD2 <- subset(st_read(road2, options = "ENCODING=UTF-8", quiet=T), TYPE=='PN')
    if (nrow(ROAD2)>0){
      ROAD2<-st_difference(ROAD2, st_union(ROUTE_polygon))
      ROAD2$LENGTH <- as.numeric(st_length(ROAD2)) # Ajouter un champ longueur
    }
    # Nettoyage de la table
    ROUTE_line$LENGTH <- as.numeric(st_length(ROUTE_line)) # Ajouter un champ longueur
    d=0.1
    ROUTE_line <- subset(ROUTE_line, LENGTH>=d)
    ROUTE_line$DECA = as.character(NA)
    ROUTE_line$NATURE = as.character(NA)
    ROUTE_line$NOM = as.character(NA)
    ROUTE_line <- ROUTE_line[, c("TYPE", "NATURE", "NOM", "LENGTH", "DECA")]
    
    # Import 'PN' depuis IGN BDTopo
    if (nrow(ROAD2)>0){
      ROUTE_line <- rbind(ROUTE_line, ROAD2)
    }
    
    cat('        lignes: découpe réalisée \n')
    cat('        ROUTE_line a été crée \n \n')

    write(ROUTE_line, repout2)
    write(ROUTE_polygon, repout2)
  } # Fin Boucle 1. ROAD_polygon

  # Chargement 2. ROAD_line
  if (grepl('line', road)){ # Boucle 2. OSM
    message('        Conversion de ROAD_line')
    
    # Lecture du fichier
    ROAD_line <- ROAD

    # Entrée des paramètres
    cat('        Entrée des paramètres \n \n')
    if (Sys.info()["sysname"]=="Windows"){
      B1 <- as.numeric(winDialogString("Tempon RN+RD:",default = ""))
      B2 <- as.numeric(winDialogString("Tempon RC+RF:",default = ""))
    }else {
      B1 <- as.numeric(readline(prompt="Tempon RN+RD:"))
      B2 <- as.numeric(readline(prompt="Tempon RC+RF:"))
    }
    
    # Creation de ROUTE_polygon
    get_buffer <- function(x, type, B){
      lines <- x |>
        subset(TYPE == type)
      
      if (nrow(lines)>0) {
        lines$NOM <- ifelse(is.na(lines$NOM), "NA_TEMP", lines$NOM)
        lines_aggregated <- aggregate(lines, by = list(TYPE = lines$TYPE, NOM = lines$NOM), FUN = mean, do_union = TRUE)
        lines_aggregated$NOM[lines_aggregated$NOM == "NA_TEMP"] <- NA
        
        lines_buffered <- st_buffer(lines_aggregated, B, 5, joinStyle="ROUND", endCapStyle='ROUND')|>
          st_simplify(preserveTopology = TRUE, dTolerance = 0.1)
        
        polygon <- lines_buffered |> st_cast("MULTIPOLYGON")
        polygon_geom <- st_geometry(polygon)
        polygon_merged <- st_union(polygon_geom)
        polygon_sf <- st_sf(geometry = polygon_merged)|>
          transform(TYPE = type,
                    NATURE = as.character(NA),
                    NOM = as.character(NA),
                    DECA = as.character(NA))
        
        lines_sf <- lines_buffered |> st_cast("MULTILINESTRING") |>
          subset(select=c("TYPE", "NATURE", "NOM", "DECA"))
        
        buffer <- list(polygon_sf, lines_sf)
        return(buffer)
      }
    }

    ## Création des buffers
    RN_buffer <- get_buffer(ROAD_line, "RN", B1+1)[[1]]
    RD_buffer <- get_buffer(ROAD_line, "RD", B1)[[1]]
    RC_buffer <- get_buffer(ROAD_line, "RC", B2)[[1]]
    RF_buffer <- get_buffer(ROAD_line, "RF", B2)[[1]]
    all_buffer <- rbind(RN_buffer, RD_buffer, RC_buffer, RF_buffer)
    cat('        polygones: buffers crées \n')

    ## Supression des débordements
    decoupe <- function(x, y){
      if(isTRUE(nrow(x)>0) & isTRUE(nrow(y)>0)){
        polygon <- st_difference(x, y) 
        polygon <- polygon[,"TYPE"]
      } else {
        polygon <- x
        polygon <- polygon[,"TYPE"]
      }
      return(polygon)
    }
    
    RD_buffer <- decoupe(RD_buffer, RN_buffer)
    RC_buffer <- decoupe(decoupe(RC_buffer, RN_buffer), RD_buffer)
    RF_buffer <- decoupe(decoupe(decoupe(RF_buffer, RN_buffer), RD_buffer), RC_buffer)
    ROUTE_buffer <- st_cast(rbind(RN_buffer[,"TYPE"], RD_buffer, RC_buffer, RF_buffer), "MULTIPOLYGON") 
    ROUTE_buffer <- st_cast(ROUTE_buffer, "POLYGON")
    ROUTE_buffer <- st_make_valid(ROUTE_buffer)
    cat('        polygones: débordements supprimés \n')

    ## Suppression des des extrémités
    get_extern_buffer <- function(x){
      # suppresion des PN
      lines <- x |> subset(TYPE != 'PN')
      
      #récupération des noeuds en doublons
      points <- lines |>  st_cast("POINT")
      coords <- st_coordinates(points)
      dup_indices <- which(duplicated(coords) | duplicated(coords, fromLast = TRUE))
      points_doublons <- points[dup_indices, ]
      
      #combine + buffer
      lines_combined <- st_sf(st_combine(lines))
      points_combined <- st_sf(st_combine(points_doublons))
      lines_buffered <- st_sf(st_buffer(lines_combined, B1 + 5, nQuadSegs = 5, endCapStyle = 'FLAT', joinStyle = "ROUND"))
      points_buffered <- st_sf(st_buffer(points_doublons, B1 + 5, nQuadSegs = 5, joinStyle = "ROUND"))
      
      #recupération des geometry
      poly1_geom <- st_geometry(lines_buffered)
      poly2_geom <- st_geometry(points_buffered)
      merged_poly <- st_union(c(poly1_geom, poly2_geom))
      merged_sf <- st_sf(geometry = merged_poly)
      
      return(merged_sf)
    }
    
    BUFFER_polygon <- st_make_valid(get_extern_buffer(ROAD_line))
    ROUTE_polygon <- st_make_valid(st_intersection(ROUTE_buffer, BUFFER_polygon))
    cat('        polygones: extrémités supprimées \n')
    cat('        ROUTE_polygon a été crée \n \n')
    
    # Création de ROUTE_line
    ## Création des bordures
    RN_line <- get_buffer(ROAD_line, "RN", B1+1)[[2]]
    RD_line <- get_buffer(ROAD_line, "RD", B1)[[2]]
    RC_line <- get_buffer(ROAD_line, "RC", B2)[[2]]
    RF_line <- get_buffer(ROAD_line, "RF", B2)[[2]]
    
    PN_line <- ROAD_line[ROAD_line$TYPE %in% "PN", ]
    ROUTE_line <- rbind(RN_line, RD_line, RC_line, RF_line)
    cat('        lignes: buffers créés \n')

    ## Suppression des débordements
    poly3_geom <- st_geometry(all_buffer)
    merged_poly <- st_union(poly3_geom)
    merged_sf <- st_sf(geometry = merged_poly)
    
    PN_line <- st_difference(PN_line, merged_sf)
    ROUTE_line <- st_difference(ROUTE_line, st_buffer(merged_sf,-0.001))
    cat('        lignes: débordements supprimés \n')
    
    ## Suppression des des extrémités
    ROUTE_line <- st_intersection(ROUTE_line, BUFFER_polygon)
    ROUTE_line <- rbind(ROUTE_line, PN_line)
    cat('        lignes: extrémités supprimées \n')
    cat('        ROUTE_line a été crée \n \n')

    # Export des fichiers
    message('        Export des fichiers')
    write(ROUTE_polygon, repout2)
    write(ROUTE_line, repout2)
    
  } # Fin Boucle 2. ROAD_line

  # Suppression débordement INFRA_line
  repINFRA_line <- paste(dirname(road), paste0(NAME,"_INFRA_line.shp"), sep="/")
  INFRA_line <- st_read(repINFRA_line, options = "ENCODING=UTF-8", quiet=T)
  INFRA_line <- st_difference(INFRA_line, st_make_valid(st_buffer(st_combine(ROUTE_polygon),2)))
  write(INFRA_line, repout2)

  repINFRA_polygon <- paste(dirname(road), paste0(NAME,"_INFRA_polygon.shp"), sep="/")
  INFRA_polygon <- st_read(repINFRA_polygon, options = "ENCODING=UTF-8", quiet=T)
  INFRA_polygon <- st_difference(INFRA_polygon, st_make_valid(st_buffer(st_combine(ROUTE_polygon),2)))
  write(INFRA_polygon, repout2)

  options(warn=1) # Activation des warnings
} # Fin fonction
