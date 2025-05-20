# SEQUOIA
Si vous tombez là, il y a de fortes chances que ce soit moi qui vous y est envoyé ! Pour les autres, bien le bonjour !

Sequoia, c'est un mic-mac d'idées saugrenues pour accéler la cartograhie forestière.
C'est une boite à idées et outils qui fait appel à de l'Excel, du R, et du Qgis.

# R_SEQUOIA
Le package `R_SEQUOIA` fournit des outils de trois ordres:
- Des outils de téléchargement de données ;
- Des outils d'aides à la création d'une cartographie forestière ponctuelle : véritable processus de production d'une carte, de la matrice à la carte des peuplements ;
- Des outils tout azimut à découvrir.

## Avant de jouer
Je vous invite à télécharger et extraire les données IGN BD TOPO HYDRO métropole à l'adresse suivante: https://drive.google.com/file/d/17ln_TH416HzwNKS9m-HTFcmZ0E7B-d8s/view?usp=sharing . 

L'idée serait d'extraire les données dans un répertoire du type : `GIS DataBase/IGN BD TOPO HYDRO"`


## Comment on joue ?
Pour télécharger, c'est simple, vous tapez: 

```
if (!require("devtools")) {install.packages("devtools")}
devtools::install_github("paul-carteron/cadastreAnalysis")
devtools::install_github("mucau/R_Sequoia")
```

Pour utiliser, c'est simple aussi:

```
# Répertoire de travail
repBDTOPO <- "votrerépertoire/IGN BD TOPO HYDRO"
repRdata  <- "votrerépertoire/RDATA"

# Lancement de SEQUOIA
library(SEQUOIA)
SEQUOIA::SEQUOIA(T)
```

Vous trouverez plein d'informations dans la rubrique d'aide et si besoin, vous me contacter.

Gros bisous :)
