####
# Préparation des données générales - contextes funéraires
# Portée : ensemble du corpus (pas de filtrage par zone géographique ici,
# contrairement aux scripts de préparation spécifiques nord/sud)
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
# Mêmes catégories d'objets et classes d'âge écartées que dans les scripts de
# préparation par zone. NB : contrairement à ces derniers, ce script ne
# retire pas les lignes sans âge précis renseigné (le filtre sur defunt_age
# est laissé en commentaire) puisqu'il vise des analyses (comme le réseau de
# cooccurrence par sexe) qui n'ont pas besoin de l'âge exact.
categorie_to_remove <- c("indéterminé", "divers", "divers personnel", 
                         "divers économique", "divers domestique", 
                         "huisserie  charpente", 
                         "urne", "agriculture, élevage", "vase indéterminé", 
                         "scorie", "rython", "bouchon", "récipient indéterminé")

sexe_to_remove <- c("non étudié")  # "indéterminé" uniquement quand le sexe est spécifiquement analysé

age_to_remove <- c("pas d'ossements", "non étudié", "indéterminé", "non renseigné")

d_filtered <- d %>%
  filter(!nouvelle_categorie %in% categorie_to_remove & !defunt_genre %in% sexe_to_remove & !defunt_classe_age %in% age_to_remove)
#filter(!is.na(defunt_age) & defunt_age != "")  # Désactivé : l'âge exact n'est pas nécessaire ici


# ============================================================
# 4. FUSION DES GENRES (désactivée)
# ============================================================
# Ce bloc, désactivé par défaut, fusionnerait les déterminations incertaines
# ("féminin ?", "masculin ?") avec leur catégorie certaine correspondante.
# Il est laissé en commentaire pour ne pas mélanger sexe certain et probable
# dans les analyses par défaut de ce script (voir script AFC sexe, qui suit
# une autre approche : renommer en "féminin probable"/"masculin probable"
# plutôt que fusionner).

# d_filtered <- d_filtered %>%
#   mutate(defunt_genre = case_when(defunt_genre == "féminin ?" ~ as.character("féminin"),TRUE ~ as.character(defunt_genre)))
# d_filtered <- d_filtered %>%
#   mutate(defunt_genre = case_when(defunt_genre == "masculin ?" ~ as.character("masculin"),TRUE ~ as.character(defunt_genre)))


# ============================================================
# 5. FUSION DES CLASSES D'ÂGE EN GRANDES CATÉGORIES (active)
# ============================================================
# Contrairement aux scripts de préparation par zone géographique (nord/sud,
# analyse par âge), où ce regroupement était désactivé pour garder les classes
# d'âge détaillées, il est ACTIF ici : les classes d'âge détaillées sont
# fusionnées en 3 grandes catégories (adulte / enfant / bébé), adaptées à une
# analyse par sexe qui n'a pas besoin de la granularité fine des classes d'âge.

# --- Fusion des adultes ---
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte ?" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "jeune adulte ou adulte mature" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte mature" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "jeune adulte" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "jeune sénile" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte sénile" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "adulte mature ou sénile" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "juvenis ou adulte" ~ as.character("adulte"),TRUE ~ as.character(defunt_classe_age)))

# --- Fusion des enfants ---
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I ou II" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infans II" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I ?" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infantile ?" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infans I ou infans II" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infantile" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "juvenis ou jeune adulte" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "juvenis" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "infans II ou juvenis" ~ as.character("enfant"),TRUE ~ as.character(defunt_classe_age)))

# --- Fusion des bébés ---
# NB : contrairement aux catégories ci-dessus, "bébé" n'existe pas comme
# modalité brute dans les données ; elle est créée ici à partir de "périnatal"
# et "prématuré", d'où l'ordre des opérations (ces valeurs auraient pu être
# absorbées par erreur dans "enfant" si ce bloc avait été placé avant).
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "périnatal" ~ as.character("bébé"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "prématuré" ~ as.character("bébé"),TRUE ~ as.character(defunt_classe_age)))
d_filtered <- d_filtered %>%
  mutate(defunt_classe_age = case_when(defunt_classe_age == "périnatal ou infans I" ~ as.character("bébé"),TRUE ~ as.character(defunt_classe_age)))


# ============================================================
# 6. REGROUPEMENT DE L'ÂGE RÉEL EN TRANCHES (désactivé)
# ============================================================
# Bloc désactivé, plus sommaire que celui des scripts de préparation par zone
# (moins de correspondances couvertes) : à ne réactiver que si ce script est
# un jour utilisé pour une analyse sur l'âge exact plutôt que sur le sexe.

# d_filtered <- d_filtered %>%
#   mutate(defunt_age = case_when(
#     defunt_age == "1 an" ~ "1-2 ans",
#     defunt_age == "3 ans" ~ "3-6 ans",
#     defunt_age == "5 ans" ~ "5-9 ans",
#     defunt_age == "8 ans" ~ "5-9 ans",
#     defunt_age == "6 ans" ~ "5-9 ans",
#     defunt_age == "10 ans" ~ "10-14 ans",
#     defunt_age == "plus de 30 ans" ~ "30-59 ans",
#     defunt_age == "12 ans" ~ "10-12 ans",
#     defunt_age == "9 ans" ~ "5-9 ans",
#     defunt_age == "18 ans" ~ "13-19 ans",
#     TRUE ~ defunt_age  # Conserve la valeur originale dans les autres cas
#   ))


# ============================================================
# 7. TRI DES TYPES DE SÉPULTURE
# ============================================================
# Identique aux scripts de préparation par zone (voir ces scripts pour le
# détail : regroupement crémation primaire / secondaire / inhumation).

# --- Crémation primaire ---
d_filtered <- d_filtered %>%
  mutate(typeSep_nom = case_when(
    typeSep_nom == "aire de crémation" ~ "crémation primaire",
    typeSep_nom == "bûcher en fosse" ~ "crémation primaire",
    typeSep_nom == "crémation primaire indéterminée" ~ "crémation primaire",
    typeSep_nom == "tombe-bûcher" ~ "crémation primaire",
    TRUE ~ typeSep_nom
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
    TRUE ~ typeSep_nom
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
# 8. TRI DES DÉPÔTS
# ============================================================
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
# 9. CORRECTION ORTHOGRAPHIQUE
# ============================================================
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "intrument écriture sur cire"
    ) ~ "instrument écriture sur cire",
    TRUE ~ nouvelle_categorie
  ))


# ============================================================
# 10. RECONVERSION FINALE EN FACTEURS
# ============================================================
d_filtered$nouvelle_categorie <- factor(d_filtered$nouvelle_categorie)
d_filtered$defunt_genre <- factor(d_filtered$defunt_genre)
d_filtered$defunt_classe_age <- factor(d_filtered$defunt_classe_age)
d_filtered$depot_nom <- factor(d_filtered$depot_nom)
d_filtered$typeSep_nom <- factor(d_filtered$typeSep_nom)
d_filtered$defunt_age <- factor(d_filtered$defunt_age)

table(d_filtered$depot_nom)


# ============================================================
# 11. CRÉATION DES INTERVALLES CHRONOLOGIQUES
# ============================================================
# Même logique que les autres scripts de préparation : intervalle calculé sur
# le centre de la fourchette chronologique. NB : ici, d_intervalle part de
# d_filtered (l'ensemble du corpus), sans filtrage géographique préalable —
# c'est le script qui utilise ce résultat (ex. analyse réseau par sexe) qui
# se charge ensuite de filtrer par zone_geo si besoin.
d_intervalle <- d_filtered

limites_classes <- c(-75, 1, 100, 300)

d_intervalle <- d_intervalle %>%
  mutate(
    phase_date_debut = as.numeric(phase_date_debut),
    phase_date_fin = as.numeric(phase_date_fin)
  ) %>%
  filter(
    !is.na(phase_date_debut),
    !is.na(phase_date_fin),
    phase_date_debut != 0
  ) %>%
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
