# Culture matérielle, société et identité en Gaules et Germanies au Haut-Empire

Ce dépôt contient les scripts R utilisés pour les analyses quantitatives de
la thèse appliquées à un corpus de mobilier archéologique (instrumentum) issu 
de contextes urbains, ruraux et funéraires en Gaule et Germanies.

Ce document explique comment consulter et exécuter les scripts, **même sans
connaissance préalable de R**.

## Structure du dépôt

Le dépôt est organisé en trois grands contextes archéologiques, chacun
structuré de la même façon :

```
culture_materielle_HN/
├── contextes_urbains/
│   ├── data/         → corpus_urbain.csv
│   ├── scripts/      → AFC + CAH + k-means, pour chaque intervalle chronologique
│   └── resultats/    → fichiers générés à l'exécution
│
├── contextes_ruraux/
│   ├── data/         → corpus_rural_v2.csv
│   ├── scripts/      → AFC + CAH + k-means (zone Rhône), SVM/arbre de décision/
│   │                   forêt aléatoire/KNN (Bourgogne et Rhône)
│   └── resultats/
│
├── contextes_funeraires/
│   ├── genre_age/
│   │   ├── data/       → contextes_funeraires_v6.csv
│   │   ├── scripts/    → préparation des données (par zone géographique et
│   │   │                 par variable : sexe, âge) et AFC associées
│   │   └── resultats/
│   ├── analyse_reseaux/
│   │   ├── data/       → contextes_funeraires_v6.csv
│   │   ├── scripts/    → préparation générale + réseaux de cooccurrence
│   │   │                 d'objets (par sexe, âge, statut)
│   │   └── resultats/
│   ├── machine_learning/
│   │   ├── data/       → contextes_funeraires_v6.csv (copie, voir remarque
│   │   │                 plus bas)
│   │   ├── scripts/    → SVM, arbre de décision, forêt aléatoire, KNN,
│   │   │                 prédisant le sexe ou la classe d'âge du défunt
│   │   └── resultats/
│   └── statut_social/
│       ├── data/       → statut_social.csv, cah_gaule_nord.csv,
│       │                 cah_gaule_sud.csv, cah_gaule_centre-est.csv
│       ├── scripts/    → CAH sur les marqueurs de statut social, puis
│       │                 réseaux de cooccurrence par groupe CAH
│       └── resultats/
│
└── README.md         → ce document
```

Chaque dossier `scripts/` contient des scripts commentés, organisés en
sections numérotées expliquant chaque étape de l'analyse. Certains scripts
dépendent d'un script de préparation des données (`preparation_donnees_*.R`)
via un appel `source()` en début de fichier : ce script doit se trouver dans
le même dossier `scripts/` pour que l'exécution fonctionne.

## Consulter le code sans l'exécuter

Il n'est pas nécessaire d'installer quoi que ce soit pour lire le contenu des
scripts : il suffit de cliquer sur un fichier `.R` directement sur GitHub,
dans le dossier `scripts/` du contexte qui vous intéresse.

## Exécuter le code

Si vous souhaitez lancer les scripts vous-même, voici la marche à suivre.

### 1. Installer R et RStudio (une seule fois)

- Installer R : https://cran.r-project.org/
- Installer RStudio (interface recommandée) : https://posit.co/download/rstudio-desktop/

### 2. Télécharger le dépôt

Sur la page GitHub du projet, cliquer sur le bouton vert **Code**, puis
**Download ZIP**. Décompresser le fichier téléchargé dans un dossier de votre
choix sur votre ordinateur.

### 3. Ouvrir RStudio et définir le dossier de travail

C'est l'étape la plus importante : R doit savoir dans quel dossier chercher
les données et où écrire les résultats. Les scripts utilisent des chemins
relatifs (`data/...`, `resultats/...`) : le dossier de travail doit donc être
défini sur le **sous-dossier qui contient directement `data/`, `scripts/` et
`resultats/`** — c'est-à-dire `contextes_urbains` ou `contextes_ruraux`, mais
pour le funéraire, un cran plus profond : `contextes_funeraires/genre_age`,
`contextes_funeraires/analyse_reseaux`, `contextes_funeraires/machine_learning`
ou `contextes_funeraires/statut_social` selon le script à exécuter.

Dans RStudio :

1. Ouvrir le menu **Session**
2. Choisir **Set Working Directory** > **Choose Directory...**
3. Sélectionner le sous-dossier correspondant (par exemple
   `culture_materielle_HN-main/contextes_urbains`, ou
   `culture_materielle_HN-main/contextes_funeraires/genre_age`)

**Cas particulier du dossier `machine_learning`** : ses scripts utilisent
aussi des données préparées par des scripts situés dans `genre_age/` (via un
appel `source("../genre_age/scripts/...")`). Le fichier
`contextes_funeraires_v6.csv` doit donc se trouver à la fois dans
`genre_age/data/` et dans `machine_learning/data/` pour que ces scripts
fonctionnent.

### 4. Installer les packages nécessaires (une seule fois par ordinateur)

Selon les scripts que vous comptez exécuter, copier-coller la ou les lignes
correspondantes dans la console RStudio (en bas de l'écran) et appuyer sur
Entrée.

**Pour les scripts d'AFC / CAH / k-means** (contextes urbains et ruraux) :
```r
install.packages(c("FactoMineR", "factoextra", "dplyr", "ggplot2", "tidyr",
                    "cluster", "corrplot", "explor", "dendextend",
                    "pheatmap", "mclust", "lsr"))
```

**Pour les scripts de classification** (SVM, arbre de décision, forêt
aléatoire, KNN — contextes ruraux et funéraires) :
```r
install.packages(c("caret", "e1071", "rpart.plot", "randomForest",
                    "pROC", "ROCR", "MLmetrics", "tibble", "stringr",
                    "reshape2", "PRROC"))
```

**Pour les scripts de préparation et d'analyse des contextes funéraires**
(réseaux de cooccurrence, statut social) :
```r
install.packages(c("dplyr", "tidyr", "igraph", "ggraph", "ggplot2",
                    "pheatmap", "corrplot", "cluster", "reshape2",
                    "tidyverse"))
```

Cette étape peut prendre quelques minutes selon le nombre de packages ; elle
n'est nécessaire qu'une seule fois par ordinateur (sauf mise à jour de R).

### 5. Ouvrir et exécuter un script

1. Dans le panneau **Files** (en bas à droite de RStudio), naviguer jusqu'au
   dossier `scripts/` du contexte souhaité et cliquer sur le script à ouvrir
2. Cliquer sur le bouton **Source** en haut à droite de l'éditeur pour
   exécuter l'ensemble du script

Les résultats (graphiques, tableaux) s'affichent dans RStudio, et les
fichiers exportés apparaissent automatiquement dans le dossier `resultats/`
du contexte concerné.

## Contenu par contexte

### Contextes urbains

Analyse du mobilier de sites urbains sur trois intervalles chronologiques
(-60/1, 1/100, 100/300) : AFC, CAH, k-means, avec analyse de sensibilité
excluant les sites isolés.

### Contextes ruraux

Analyse du mobilier d'établissements ruraux (relais routiers, etc.) dans la
moyenne vallée du Rhône : AFC, CAH, k-means, analyse d'un site supplémentaire
atypique (VAM). Comparaison de plusieurs algorithmes de classification
supervisée (SVM, arbre de décision, forêt aléatoire, KNN) sur les zones
Bourgogne et Rhône.

### Contextes funéraires

Quatre sous-dossiers, chacun avec sa propre structure `data/`/`scripts/`/`resultats/` :

- **`genre_age/`** : préparation des données par zone géographique (Gaule du
  nord, du sud) et par variable étudiée (sexe, âge du défunt), suivie
  d'analyses factorielles des correspondances par intervalle chronologique.
- **`analyse_reseaux/`** : préparation générale des données (toutes zones
  confondues) puis réseaux de cooccurrence d'objets, croisés avec le sexe et
  l'âge du défunt.
- **`machine_learning/`** : modèles de classification supervisée (SVM, arbre
  de décision, forêt aléatoire, KNN) prédisant le sexe ou la classe d'âge du
  défunt à partir du mobilier funéraire, pour la Gaule du nord et du sud.
- **`statut_social/`** : classification du statut social par CAH sur des
  marqueurs de richesse funéraire (amphore, monnaie, armement...), croisée
  avec le sexe, l'âge et la chronologie, puis réseaux de cooccurrence des
  marqueurs de statut au sein de chaque groupe.

## Remarque importante

Si le dossier de travail (étape 3) n'est pas correctement défini avant de
lancer un script, celui-ci ne trouvera pas les fichiers de données et
affichera une erreur du type `cannot open file`. Il suffit de reprendre
l'étape 3, en veillant à bien sélectionner le sous-dossier du contexte
concerné (et non la racine du dépôt).

*les commentaires des scripts ont été harmonisé par Claude.ai pour le dépôt sur Github. Seuls ces commentaires ont été corrigés par ia générative : l'entièreté des scripts ont été écrit par l'autrice.
