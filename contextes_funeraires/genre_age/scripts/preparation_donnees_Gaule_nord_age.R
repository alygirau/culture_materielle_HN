####
# Préparation des données - contextes funéraires
# Zone géographique : Gaule du nord
# Analyse : croisement mobilier / âge du défunt
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(dplyr)   # Manipulation de données (mutate, filter, case_when...)


# ============================================================
# 1. CHARGEMENT DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
d <- read.csv2("data/contextes_funeraires_v6.csv", header = TRUE, encoding = "utf8", row.names = "ID")


# ============================================================
# 2. CONVERSION DES VARIABLES EN FACTEURS
# ============================================================
d$obj_nom             <- as.factor(d$obj_nom)
d$nouvelle_categorie  <- as.factor(d$nouvelle_categorie)
d$materiau_nom        <- as.factor(d$materiau_nom)
d$depot_nom           <- as.factor(d$depot_nom)
d$typeSep_nom         <- as.factor(d$typeSep_nom)
d$defunt_genre        <- as.factor(d$defunt_genre)
d$defunt_classe_age   <- as.factor(d$defunt_classe_age)
d$site_abbreviation   <- as.factor(d$site_abbreviation)
d$province_nom        <- as.factor(d$province_nom)
d$fonction_nom        <- as.factor(d$fonction_nom)
d$zone_geo            <- as.factor(d$zone_geo)


# ============================================================
# 3. FILTRAGE DES MODALITÉS NON EXPLOITABLES
# ============================================================
# Catégories d'objets trop vagues ou non pertinentes, sépultures sans sexe
# étudié, et classes d'âge non exploitables (ossements absents, non étudié,
# indéterminé, non renseigné) sont exclues. On retire également les lignes
# où l'âge précis du défunt (defunt_age) est manquant ou vide, cette variable
# étant nécessaire pour une analyse centrée sur l'âge.
categorie_to_remove <- c("indéterminé", "divers", "divers personnel", 
                         "divers économique", "divers domestique", 
                         "huisserie  charpente", 
                         "urne", "agriculture, élevage", "vase indéterminé", 
                         "scorie", "rython", "bouchon", "récipient indéterminé")

sexe_to_remove <- c("non étudié")

age_to_remove <- c("pas d'ossements", "non étudié", "indéterminé", "non renseigné")

d_filtered <- d %>%
  filter(!nouvelle_categorie %in% categorie_to_remove & !defunt_genre %in% sexe_to_remove & !defunt_classe_age %in% age_to_remove) %>%
  filter(!is.na(defunt_age) & defunt_age != "")


# ============================================================
# 4. REGROUPEMENT DES CATÉGORIES D'OBJETS
# ============================================================
# Mêmes regroupements que dans le script équivalent pour l'analyse par sexe
# (voir ce script pour le détail des fusions ligne à ligne) : parure/vêtement,
# équipement de consommation alimentaire, récipient de stockage, armement,
# artisanat, soins du corps, équipement culturel.

# --- Parure, vêtement ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "accessoire de coiffure" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "bague clé" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "petite fibule" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "fibule de taille indéterminée" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "grande fibule" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "fibule moyenne" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "parure annulaire bague et anneau" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "parure de cou" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "pendentif" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "accessoire de costume" ~ as.character("vêtement"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "parure annulaire bague a intaille" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "parure de bras" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "parure de tête" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "perle" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "divers personnel" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "clou de chaussure" ~ as.character("vêtement"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "parure indéterminée" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "épingle" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))

# --- Équipement de consommation alimentaire ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "récipient culinaire" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "récipient de boisson" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "ustensile de cuisine" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "ustensile de table ou de cuisine" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "ustensile pour la boisson" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "couteau" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "tranchet" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "récipient à boire" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "rython" ~ as.character("équipement de consommation alimentaire"),TRUE ~ as.character(nouvelle_categorie)))

# --- Récipient de stockage ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "bouteille" ~ as.character("récipient de stockage"),TRUE ~ as.character(nouvelle_categorie)))

# --- Armement ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "guerre" ~ as.character("armement"),TRUE ~ as.character(nouvelle_categorie)))

# --- Artisanat ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "outil" ~ as.character("artisanat"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "travail du textile" ~ as.character("artisanat"),TRUE ~ as.character(nouvelle_categorie)))

# --- Soins du corps ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "instrument de préparation cosmetique ou pharmaceutique" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "instrument de toilette" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "ustensile de toilette" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "cosmetique récipient" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "petite boite ou pyxide" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "récipient de cosmétique" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "strigile" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "instrument chirurgical" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "balasamaire" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "miroir" ~ as.character("soins du corps"),TRUE ~ as.character(nouvelle_categorie)))

# --- Équipement culturel ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "clochette" ~ as.character("équipement culturel"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "statuette" ~ as.character("équipement culturel"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "élément de jeu" ~ as.character("équipement culturel"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "miniature" ~ as.character("équipement culturel"),TRUE ~ as.character(nouvelle_categorie)))


# ============================================================
# 5. FUSION DES CLASSES D'ÂGE (regroupement grossier optionnel, désactivé)
# ============================================================
# Ce bloc, désactivé par défaut, fusionne toutes les classes d'âge en 3
# grandes catégories (adulte / enfant / bébé). Il est laissé en commentaire
# pour être réactivé si une analyse par grandes classes d'âge est souhaitée
# à la place de l'analyse par classes d'âge détaillées utilisée plus bas.

# # Fusion des adultes (à commenter pour l'afc)
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte ?" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "jeune adulte ou adulte mature" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte mature" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "jeune adulte" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "jeune sénile" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte sénile" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte mature ou sénile" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "juvenis ou adulte" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
#
# #Fusion des enfants
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I ou II" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infans II" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I ?" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infantile ?" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I ou infans II" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infantile" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "juvenis ou jeune adulte" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "juvenis" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "infans II ou juvenis" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "périnatal" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "prématuré" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "périnatal ou infans I" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
#
# # Fusion des bébés
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "périnatal" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "prématuré" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_classe_age = case_when(defunt_classe_age == "périnatal ou infans I" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))


# ============================================================
# 6. NETTOYAGE DES CLASSES D'ÂGE DÉTAILLÉES (actif)
# ============================================================
# Contrairement au bloc précédent, ce nettoyage-ci est actif : il ne fusionne
# pas en grandes catégories, mais harmonise des libellés de classes d'âge
# proches ou redondants (ex. fusion des mentions "ou" ambiguës vers la classe
# la plus probable), sans perdre la granularité des classes d'âge détaillées.
#
# NOTE : pour une analyse en AFC sur
# les classes d'âge, garder ce bloc actif ; pour une analyse sur l'âge réel,
# le commenter et activer à la place le bloc de la
# section 7 ci-dessous (actuellement désactivé).
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(
    defunt_classe_age == "jeune adulte ou adulte mature" ~ "jeune adulte",
    defunt_classe_age == "infans I ou infans II" ~ "infans I",
    defunt_classe_age == "juvenis ou adulte" ~ "juvenis",
    defunt_classe_age == "juvenis ou jeune adulte" ~ "juvenis",
    defunt_classe_age == "infans II ou juvenis" ~ "infans II",
    defunt_classe_age == "jeune adulte ou adulte mature" ~ "jeune adulte",
    defunt_classe_age == "périnatal ou infans I" ~ "périnatal",
    defunt_classe_age == "adulte mature ou sénile" ~ "adulte mature",
    defunt_classe_age == "indans I ou II" ~ "infans I",
    defunt_classe_age == "prématuré" ~ "périnatal",
    defunt_classe_age == "infantile ?" ~ "infans I ou II",
    defunt_classe_age == "infantile" ~ "infans I ou II",
    TRUE ~ defunt_classe_age  # Conserve la valeur originale dans les autres cas
  ))


# ============================================================
# 7. REGROUPEMENT DE L'ÂGE RÉEL EN TRANCHES (désactivé)
# ============================================================
# Bloc désactivé par défaut (voir note à la section 6) : regroupe les
# valeurs très fines de defunt_age (ex. "1-2 mois", "10 ans"...) en tranches
# d'âge plus larges et plus régulières (ex. "avant 1 an", "10-14 ans"...),
# pour une analyse fondée sur l'âge réel plutôt que sur les classes d'âge
# (infans, juvenis, adulte...).

# d_filtered <- d_filtered %>%
#   mutate(defunt_age = case_when(
#     defunt_age == "0-1 an" ~ "0-3 ans",
#     defunt_age == "0-2 an" ~ "0-3 ans",
#     defunt_age == "0-3 an" ~ "0-3 ans",
#     defunt_age == "1-12 ans" ~ "1-13 ans",
#     defunt_age == "1-2 an" ~ "1-4 ans",
#     defunt_age == "1 an" ~ "1-4 ans",
#     defunt_age == "1 an et demi" ~ "1-4 ans",
#     defunt_age == "1 an et demi - 2 ans et demi" ~ "1-4 ans",
#     defunt_age == "1-4 ans" ~ "1-4 ans",
#     defunt_age == "2-3 ans" ~ "1-4 ans",
#     defunt_age == "2-4 ans" ~ "1-4 ans",
#     defunt_age == "2 ans" ~ "1-4 ans",
#     defunt_age == "2 ans et demi - 3 ans" ~ "1-4 ans",
#     defunt_age == "1-6 ans" ~ "1-7 ans",
#     defunt_age == "3-4 ans" ~ "3-6 ans",
#     defunt_age == "3-5 ans" ~ "3-6 ans",
#     defunt_age == "3 ans" ~ "3-6 ans",
#     defunt_age == "4-6 ans" ~ "4-10 ans",
#     defunt_age == "4-8 ans" ~ "4-10 ans",
#     defunt_age == "4 ans" ~ "4-10 ans",
#     defunt_age == "5-6 ans" ~ "5-9 ans",
#     defunt_age == "5 ans" ~ "5-9 ans",
#     defunt_age == "6 ans" ~ "6-12 ans",
#     defunt_age == "8 ans" ~ "6-12 ans",
#     defunt_age == "9 ans" ~ "8-14 ans",
#     defunt_age == "9,5 mois" ~ "avant 1 an",
#     defunt_age == "avant terme" ~ "avant 1 an",
#     defunt_age == "9-12 mois" ~ "avant 1 an",
#     defunt_age == "6 mois" ~ "avant 1 an",
#     defunt_age == "6 mois - 1 an" ~ "avant 1 an",
#     defunt_age == "1-2 mois" ~ "avant 1 an",
#     defunt_age == "10 mois" ~ "avant 1 an",
#     defunt_age == "10-12 ans" ~ "10-14 ans",
#     defunt_age == "10 ans" ~ "10-14 ans",
#     defunt_age == "11-13 ans" ~ "10-14 ans",
#     defunt_age == "12-13 ans" ~ "10-14 ans",
#     defunt_age == "12 ans" ~ "10-14 ans",
#     defunt_age == "14-18 ans" ~ "13-19 ans",
#     defunt_age == "14-19 ans" ~ "13-19 ans",
#     defunt_age == "15-16 ans" ~ "15-19 ans",
#     defunt_age == "17-18 ans" ~ "15-19 ans",
#     defunt_age == "18-19 ans" ~ "18-22 ans",
#     defunt_age == "18 ans" ~ "18-22 ans",
#     defunt_age == "20-24 ans" ~ "20-30 ans",
#     defunt_age == "20-25 ans" ~ "20-30 ans",
#     defunt_age == "20-29 ans" ~ "20-30 ans",
#     defunt_age == "20-39 ans" ~ "20-40 ans",
#     defunt_age == "24-40 ans" ~ "20-40 ans",
#     defunt_age == "20-49 ans" ~ "20-60 ans",
#     defunt_age == "20-55 ans" ~ "20-60 ans",
#     defunt_age == "20-59 ans" ~ "20-60 ans",
#     defunt_age == "plus de 25 ans" ~ "20-60 ans",
#     defunt_age == "plus de 30 ans" ~ "20-60 ans",
#     defunt_age == "30-35 ans" ~ "30-50 ans",
#     defunt_age == "30-40 ans" ~ "30-50 ans",
#     defunt_age == "30-59 ans" ~ "30-60 ans",
#     defunt_age == "35-55 ans" ~ "30-60 ans",
#     defunt_age == "40-45 ans" ~ "40-65 ans",
#     defunt_age == "40-60 ans" ~ "40-65 ans",
#     defunt_age == "40 ans" ~ "40-65 ans",
#     defunt_age == "45-65 ans" ~ "40-65 ans",
#     defunt_age == "50-59 ans" ~ "50-70 ans",
#     defunt_age == "56-70 ans" ~ "50-70 ans",
#     defunt_age == "plus de 55 ans" ~ "plus de 50 ans",
#     defunt_age == "plus de 60 ans" ~ "plus de 50 ans",
#     TRUE ~ defunt_age  # Conserve la valeur originale dans les autres cas
#   ))


# ============================================================
# 8. TRI DES TYPES DE SÉPULTURE
# ============================================================
# Regroupement des types de sépulture par crémation (primaire/secondaire) et
# inhumation, en fusionnant les variantes proches sous un intitulé commun.

# --- Crémation primaire ---
d_filtered <- d_filtered %>%
  mutate(typeSep_nom = case_when(
    typeSep_nom == "aire de crémation" ~ "crémation primaire",
    typeSep_nom == "bûcher en fosse" ~ "crémation primaire",
    typeSep_nom == "crémation primaire indéterminée" ~ "crémation primaire",
    typeSep_nom == "tombe-bûcher" ~ "crémation primaire",
    TRUE ~ typeSep_nom  # Conserve la valeur originale dans les autres cas
  ))

# Exclusion des types de sépulture trop incertains ou non définis
d_filtered <- d_filtered %>%
  filter(!typeSep_nom %in% c(
    "bûcher en fosse ?",
    "crémation de type indéterminé",
    "crémation secondaire ?",
    "crémation secondaire avec ossements disperses ?",
    "non définis",
    "réduction d'inhumation en pleine terre"
  ))

# --- Crémation secondaire ---
d_filtered <- d_filtered %>%
  mutate(typeSep_nom = case_when(
    typeSep_nom %in% c(
      "crémation secondaire à amphore",
      "crémation secondaire à coffrage en tuile",
      "crémation secondaire à urne",
      "crémation secondaire à urne ou à amas osseux",
      "crémation secondaire avec amas osseux",
      "crémation secondaire avec amas osseux ?",
      "crémation secondaire avec amas osseux ou ossements dispersés",
      "crémation secondaire avec ossements dispersés",
      "crémation secondaire contenant périssable",
      "crémation secondaire en fosse",
      "crémation secondaire et inhumation",
      "crémation secondaire indéterminée",
      "crémation secondaire mixte",
      "crémation secondaire mixte en coffre monolithe",
      "crémation secondaire ossements dispersés",
      "crémation secondaire remaniée",
      "dépôt secondaire de résidus de crémation"
    ) ~ "crémation secondaire",
    TRUE ~ typeSep_nom  # Autres valeurs restent inchangées
  ))

# --- Inhumation ---
d_filtered <- d_filtered %>%
  mutate(typeSep_nom = case_when(
    typeSep_nom %in% c(
      "inhumation",
      "inhumation collective",
      "inhumation de type indéterminé",
      "inhumation en amphore",
      "inhumation en batière",
      "inhumation en cercueil en bois",
      "inhumation en coffrage de bois",
      "inhumation en coffrage de bois et de tuile",
      "inhumation en coffrage de dalles",
      "inhumation en coffrage de tuile",
      "inhumation en coffrage mixte",
      "inhumation en pleine terre",
      "inhumation en sarcophage de pierre",
      "inhumation en sarcophage en plomb"
    ) ~ "inhumation",
    TRUE ~ typeSep_nom
  ))


# ============================================================
# 9. TRI DES DÉPÔTS
# ============================================================
# Harmonisation des libellés de type de dépôt de mobilier dans la sépulture.
d_filtered <- d_filtered %>%
  mutate(depot_nom = case_when(
    depot_nom %in% c(
      "dépôt secondaire de mobilier"
    ) ~ "dépôt secondaire",
    TRUE ~ depot_nom
  ))

d_filtered <- d_filtered %>%
  mutate(depot_nom = case_when(
    depot_nom %in% c(
      "dépôt primaire de mobilier"
    ) ~ "dépôt primaire",
    TRUE ~ depot_nom
  ))

d_filtered <- d_filtered %>%
  mutate(depot_nom = case_when(
    depot_nom %in% c(
      "dépôt inh"
    ) ~ "mobilier déposé dans une inhumation",
    TRUE ~ depot_nom
  ))


# ============================================================
# 10. CORRECTION ORTHOGRAPHIQUE
# ============================================================
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "intrument écriture sur cire"
    ) ~ "instrument écriture sur cire",
    TRUE ~ nouvelle_categorie
  ))


# ============================================================
# 11. RECONVERSION FINALE EN FACTEURS
# ============================================================
# Supprime les niveaux devenus inutilisés après tous les filtrages et
# regroupements ci-dessus.
d_filtered$nouvelle_categorie <- factor(d_filtered$nouvelle_categorie)
d_filtered$defunt_genre <- factor(d_filtered$defunt_genre)
d_filtered$defunt_classe_age <- factor(d_filtered$defunt_classe_age)
d_filtered$depot_nom <- factor(d_filtered$depot_nom)
d_filtered$typeSep_nom <- factor(d_filtered$typeSep_nom)

# Vérification de la distribution des types de dépôt après nettoyage
table(d_filtered$depot_nom)


# ============================================================
# 12. SÉLECTION DE LA ZONE GÉOGRAPHIQUE
# ============================================================
d_filtered_gaule_nord <- d_filtered %>%
  filter(zone_geo == "Gaule du nord")


# ============================================================
# 13. CRÉATION DES INTERVALLES CHRONOLOGIQUES
# ============================================================
# Même logique que le script équivalent pour l'analyse par sexe : l'intervalle
# est calculé à partir du centre de la fourchette chronologique (moyenne entre
# phase_date_debut et phase_date_fin).
d_intervalle <- d_filtered_gaule_nord

# Bornes chronologiques : 3 tranches [-75;1[, [1;100[, [100;300]
limites_classes <- c(-75, 1, 100, 300)

d_intervalle <- d_intervalle %>%
  # Nettoyage et conversion numérique des bornes de datation
  mutate(
    phase_date_debut = as.numeric(phase_date_debut),
    phase_date_fin = as.numeric(phase_date_fin)
  ) %>%
  
  # Exclusion des cas non exploitables : dates manquantes ou date de début à 0
  filter(
    !is.na(phase_date_debut),
    !is.na(phase_date_fin),
    phase_date_debut != 0
  ) %>%
  
  # Calcul du centre chronologique et classement dans l'intervalle correspondant
  mutate(
    centre_chrono = (phase_date_debut + phase_date_fin) / 2,
    
    intervalle_personnalise = cut(
      centre_chrono,
      breaks = limites_classes,
      include.lowest = TRUE,
      right = FALSE,
      labels = paste(
        head(limites_classes, -1),
        tail(limites_classes, -1),
        sep = " - "
      )
    )
  )

# Vérification de la distribution des effectifs par intervalle
table(d_intervalle$intervalle_personnalise)

# Reconversion en facteurs des variables utilisées dans les analyses suivantes
d_intervalle$nouvelle_categorie <- as.factor(d_intervalle$nouvelle_categorie)
d_intervalle$defunt_genre <- as.factor(d_intervalle$defunt_genre)
d_intervalle$fonction_nom <- as.factor(d_intervalle$fonction_nom)
d_intervalle$zone_geo <- as.factor(d_intervalle$zone_geo)


# ============================================================
# 14. VÉRIFICATION : TOTAL D'OBJETS PAR ZONE GÉOGRAPHIQUE
# ============================================================
data_cumule <- d_intervalle %>%
  summarise(total = sum(obj_nmi))
print(data_cumule)
