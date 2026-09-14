###
# Auteur : A. Turgis
# Date : 19 déc. 2024
# Sujet : Gaule du Nord : AFC + UMAP sur données non rééchantillonnées
# Analyse : croisement mobilier / sexe du défunt — deuxième intervalle chronologique
###

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(FactoMineR)   # Analyse Factorielle des Correspondances : CA()
library(factoextra)   # Visualisations complémentaires (non utilisées directement ici)
library(dplyr)        # Manipulation de données
library(ggplot2)      # Graphiques
library(explor)       # Exploration interactive des résultats d'AFC
library(tidyr)        # Remise en forme des données
library(reshape2)     # Remise en forme de tableaux

# Palette de couleurs pour les modalités de sexe du défunt (réutilisable dans
# les visualisations ggplot si besoin)
palette_couleurs <- c("féminin" = "#AFA4CE",
                      "féminin probable" = "#C5BADF",
                      "masculin" = "#F8DE65",
                      "masculin probable" = "#FCEFA2")


# ============================================================
# 1. CHARGEMENT DES DONNÉES PRÉPARÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.

# Ce script réutilise l'objet d_filtered (et d_intervalle) créé par le script
# de préparation des données. Le nom de fichier ci-dessous doit correspondre
# exactement à celui du script de préparation présent dans scripts/.
source("scripts/preparation_donnees_Gaule_nord_sexe.R")


# ============================================================
# 2. REGROUPEMENT COMPLÉMENTAIRE DE CATÉGORIES (spécifique à cette analyse)
# ============================================================
# En plus des regroupements déjà faits dans le script de préparation, deux
# fusions supplémentaires sont appliquées ici, propres à l'analyse AFC
# sexe/mobilier (simplification de l'étiquetage pour la lisibilité des graphiques).

d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "parure",
      "fibule",
      "vêtement"
    ) ~ "parure, vêtement",
    TRUE ~ nouvelle_categorie  # Les autres valeurs restent inchangées
  ))

d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "instrument de préparation cosmétique ou pharmaceutique"
    ) ~ "instru. prépa. cosm. ou pharma.",
    TRUE ~ nouvelle_categorie  # Les autres valeurs restent inchangées
  ))

d_filtered$nouvelle_categorie <- as.factor(d_filtered$nouvelle_categorie)


# ============================================================
# 3. SÉLECTION DU DEUXIÈME INTERVALLE CHRONOLOGIQUE
# ============================================================
# d_intervalle vient du script de préparation (section 7) : ses niveaux
# d'intervalle sont ordonnés chronologiquement, donc le deuxième niveau
# correspond bien au deuxième intervalle (ex. "1 - 100")
deuxieme_intervalle <- levels(d_intervalle$intervalle_personnalise)[2]
d_filtered_deuxieme <- d_intervalle %>%
  filter(intervalle_personnalise == deuxieme_intervalle)

# --- Construction du tableau de contingence catégorie x sexe ---
contingence_gaule_nord_deuxieme <- table(d_filtered_deuxieme$nouvelle_categorie, d_filtered_deuxieme$defunt_genre)

# Suppression des lignes (catégories d'objets) qui n'ont aucun effectif sur cet intervalle
contingence_gaule_nord_deuxieme <- contingence_gaule_nord_deuxieme[rowSums(contingence_gaule_nord_deuxieme) > 0, ]

# Suppression des catégories à effectif trop faible (< 10), en excluant la
# colonne "indéterminé" (3e colonne) du calcul de cet effectif puisqu'elle
# sera de toute façon mise en supplémentaire dans l'AFC ci-dessous
effectifs_sans_indet <- rowSums(contingence_gaule_nord_deuxieme[, -3])
contingence_gaule_nord_deuxieme <- contingence_gaule_nord_deuxieme[effectifs_sans_indet >= 10, ]

write.csv2(contingence_gaule_nord_deuxieme, "resultats/contingence_gaule_nord_deuxieme.csv", row.names = TRUE)


# ============================================================
# 4. ANALYSE FACTORIELLE DES CORRESPONDANCES (AFC)
# ============================================================
# La colonne "indéterminé" (colonne 3) est mise en supplémentaire : elle est
# projetée sur les axes mais ne participe pas à leur calcul, car elle
# n'apporte pas d'information sur le sexe du défunt.
afc_gaule_nord_deuxieme_sup <- CA(contingence_gaule_nord_deuxieme, col.sup = 3, graph = FALSE)

summary(afc_gaule_nord_deuxieme_sup)
explor(afc_gaule_nord_deuxieme_sup)
plot.CA(afc_gaule_nord_deuxieme_sup, autoLab = "yes")


# ============================================================
# 5. EXTRACTION DES COORDONNÉES, COS2 ET CONTRIBUTIONS (axes 1 et 2)
# ============================================================
# CORRECTION : le script original référençait ici "afc_gaule_nord_premier_sup",
# un objet qui n'est jamais créé dans ce script (seul "afc_gaule_nord_deuxieme_sup"
# est calculé ci-dessus, à l'étape 4). Il s'agissait probablement d'un reliquat
# de copier-coller depuis un script pour le premier intervalle. La ligne a été
# corrigée pour pointer vers l'objet réellement calculé ici.

# --- Modalités lignes (catégories d'objets) ---
coord_lignes <- afc_gaule_nord_deuxieme_sup$row$coord
cos2_lignes <- afc_gaule_nord_deuxieme_sup$row$cos2
contrib_lignes <- afc_gaule_nord_deuxieme_sup$row$contrib

# NB : les colonnes "Coordonnées", "Cosinus2" et "Contribution" sont chacune
# répétées deux fois (une fois par axe). R renomme automatiquement les
# doublons en ajoutant le suffixe ".1" (ex. "Coordonnées" et "Coordonnées.1"),
# donc aucune donnée n'est perdue, mais l'intitulé de colonne reste ambigu à
# la lecture du CSV exporté (on ne sait pas directement lequel est l'axe 1 et
# lequel est l'axe 2 sans se référer à l'ordre des colonnes).
df_lignes_axe1 <- data.frame(
  Coordonnées = coord_lignes[, 1],    # Coordonnées pour l'axe 1
  Coordonnées = coord_lignes[, 2],    # Coordonnées pour l'axe 2
  Cosinus2 = cos2_lignes[, 1],      # Cos2 pour l'axe 1
  Cosinus2 = cos2_lignes[, 2],      # Cos2 pour l'axe 2
  Contribution = contrib_lignes[, 1],  # Contributions pour l'axe 1
  Contribution = contrib_lignes[, 2]  # Contributions pour l'axe 2
)

write.csv2(df_lignes_axe1, "resultats/axe1_lignes.csv", row.names = TRUE)

# --- Modalités colonnes (sexe du défunt) ---
coord_colonnes <- afc_gaule_nord_deuxieme_sup$col$coord
cos2_colonnes <- afc_gaule_nord_deuxieme_sup$col$cos2
contrib_colonnes <- afc_gaule_nord_deuxieme_sup$col$contrib

# Même remarque que ci-dessus concernant le nommage des colonnes dupliquées
df_colonnes_axe1 <- data.frame(
  Coordonnées = coord_colonnes[, 1],    # Coordonnées pour l'axe 1
  Coordonnées = coord_colonnes[, 2],    # Coordonnées pour l'axe 2
  Cosinus2 = cos2_colonnes[, 1],      # Cos2 pour l'axe 1
  Cosinus2 = cos2_colonnes[, 2],      # Cos2 pour l'axe 2
  Contribution = contrib_colonnes[, 1],  # Contributions pour l'axe 1
  Contribution = contrib_colonnes[, 2]
)

write.csv2(df_colonnes_axe1, "resultats/axe1_colonnes.csv", row.names = TRUE)


# ============================================================
# 6. TEST DU CHI² ET RÉSIDUS DE PEARSON
# ============================================================
# CORRECTION : le script original référençait ici "contingence_gaule_nord_premier",
# un objet jamais créé dans ce script (même type de reliquat de copier-coller
# qu'à l'étape 5). La ligne a été corrigée pour utiliser
# "contingence_gaule_nord_deuxieme", le tableau de contingence effectivement
# construit à l'étape 3.
chisq <- chisq.test(contingence_gaule_nord_deuxieme)

# Résidus de Pearson (contribution de chaque cellule à l'écart entre effectifs
# observés et effectifs attendus sous hypothèse d'indépendance)
round(chisq$residuals, 3)

# --- Visualisation des résidus ---
library(corrplot)
res <- chisq$residuals
# Réordonnancement des lignes/colonnes par force de déviation (valeur absolue
# décroissante) pour une lecture plus claire
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

# Contribution de chaque cellule au chi² total, exprimée en pourcentage : donne
# une indication de la nature de la dépendance entre lignes et colonnes du
# tableau de contingence (quelles cellules pèsent le plus dans l'association globale)
contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

write.csv2(contrib, "resultats/contrib_pourcentage_gaule_nord_deuxieme.csv", row.names = TRUE)

corrplot(contrib, is.cor = FALSE)