####
# Préparation des données - contextes funéraires
# Zone géographique : Gaule du nord
# Analyse : croisement mobilier / sexe du défunt
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
# Toutes les variables catégorielles utilisées dans les analyses ultérieures
# sont converties en facteurs (type de données attendu par R pour ce genre
# de variable, notamment pour les tableaux croisés et tests statistiques).
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


# ============================================================
# 3. FILTRAGE DES MODALITÉS NON EXPLOITABLES
# ============================================================
# Certaines catégories d'objets sont trop vagues, hétérogènes ou non
# pertinentes pour l'analyse du croisement mobilier/sexe et sont exclues du
# corpus. De même, les sépultures dont le sexe du défunt n'a pas été étudié
# sont retirées (elles n'apportent rien à une analyse centrée sur le sexe).
categorie_to_remove <- c("indéterminé", "divers",
                         "divers économique", "divers domestique", 
                         "huisserie  charpente", "urne", "agriculture élevage", 
                         "vase indéterminé", "scorie", "bouchon","récipient indéterminé", "nodule de pigment",
                         "boîte à sceau", "harnachement")

sexe_to_remove <- c("non étudié")

d_filtered <- d %>%
  filter(!nouvelle_categorie %in% categorie_to_remove & !defunt_genre %in% sexe_to_remove)

# Reconversion en facteurs pour supprimer les niveaux (modalités) désormais
# vides suite au filtrage ci-dessus (sinon R garde en mémoire les anciennes
# modalités même si elles n'apparaissent plus dans les données)
d_filtered$nouvelle_categorie <- factor(d_filtered$nouvelle_categorie)
d_filtered$defunt_genre <- factor(d_filtered$defunt_genre)


# ============================================================
# 4. REGROUPEMENT DES CATÉGORIES D'OBJETS
# ============================================================
# De nombreuses sous-catégories d'objets sont fusionnées en catégories plus
# larges et plus interprétables, en vue des analyses statistiques (l'AFC et
# le chi² sont sensibles aux catégories à très faible effectif). Chaque
# case_when() ci-dessous applique un remplacement ponctuel tout en laissant
# les autres modalités inchangées ; le passage par as.character() évite que
# le type factor ne pose problème pendant les remplacements successifs.

# --- Parure, vêtement ---
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "accessoire de coiffure" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(nouvelle_categorie == "fibule" ~ as.character("parure"),TRUE ~ as.character(nouvelle_categorie)))
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
# 5. HARMONISATION DES MODALITÉS DE SEXE INCERTAIN
# ============================================================
# Les déterminations de sexe assorties d'un point d'interrogation ("féminin ?",
# "masculin ?") sont renommées en "féminin probable" / "masculin probable"
# pour harmoniser le libellé des modalités incertaines dans le jeu de données.
d_filtered <- d_filtered %>%
  mutate(defunt_genre = case_when(defunt_genre == "féminin ?" ~ as.character("féminin probable"),TRUE ~ as.character(defunt_genre))) 
d_filtered <- d_filtered %>%
  mutate(defunt_genre = case_when(defunt_genre == "masculin ?" ~ as.character("masculin probable"),TRUE ~ as.character(defunt_genre))) 

# Reconversion finale en facteurs après tous les regroupements ci-dessus
d_filtered$nouvelle_categorie <- as.factor(d_filtered$nouvelle_categorie)
d_filtered$defunt_genre <- as.factor(d_filtered$defunt_genre)


# ============================================================
# 6. SÉLECTION DE LA ZONE GÉOGRAPHIQUE
# ============================================================
# Seules les sépultures situées en Gaule du nord sont conservées pour cette analyse
d_filtered_gaule_nord <- d_filtered %>%
  filter(zone_geo == "Gaule du nord")


# ============================================================
# 7. CRÉATION DES INTERVALLES CHRONOLOGIQUES
# ============================================================
# Contrairement aux scripts des contextes urbains (qui utilisaient uniquement
# phase_date_debut), l'intervalle est ici calculé à partir du centre de la
# fourchette chronologique (moyenne entre phase_date_debut et phase_date_fin),
# ce qui donne une estimation plus représentative pour des datations en
# fourchette (typiques des sépultures).
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
  # (valeur probablement non renseignée plutôt qu'une vraie date de l'an 0)
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

# Export désactivé par défaut (décommenter pour activer, chemin déjà corrigé)
#write.csv2(d_intervalle, "resultats/intervalle_chrono_exemple.csv", row.names = TRUE)
