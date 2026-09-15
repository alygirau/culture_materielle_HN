###
# Auteur : A. Turgis
# Date : 12 janvier 2025
# Sujet : Gaule du nord : AFC — croisement mobilier / classe d'âge du défunt
# Deux intervalles chronologiques traités ici (pas de premier intervalle dans
# ce script, contrairement à la version Gaule du sud)
###

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(FactoMineR)   # Analyse Factorielle des Correspondances : CA()
library(factoextra)   # Visualisation des résultats de l'AFC (non utilisé directement ici)
library(dplyr)         # Manipulation et transformation des données
library(ggplot2)       # Création de graphiques (non utilisé directement ici, chargé pour compatibilité)
library(explor)        # Exploration interactive des résultats d'AFC
library(tidyr)         # Remise en forme des données
library(corrplot)      # Visualisation des matrices de résidus/contributions


# ============================================================
# 1. CHARGEMENT DES DONNÉES PRÉPARÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.

# Ce script réutilise l'objet d_intervalle créé par le script de préparation.
# Le nom de fichier ci-dessous doit correspondre exactement à celui du script
# de préparation présent dans scripts/.
source("scripts/preparation_donnees_Gaule_nord_age.R")


# ============================================================
# 2. DEUXIÈME INTERVALLE CHRONOLOGIQUE
# ============================================================

# --- 2.1 Sélection de l'intervalle et tableau de contingence ---
deuxieme_intervalle <- levels(d_intervalle$intervalle_personnalise)[2]
d_intervalle_deuxieme <- d_intervalle %>%
  filter(intervalle_personnalise == deuxieme_intervalle)

d_intervalle_deuxieme$defunt_age <- as.factor(d_intervalle_deuxieme$defunt_age)

contingence_gaule_nord_deuxieme <- table(d_intervalle_deuxieme$nouvelle_categorie, d_intervalle_deuxieme$defunt_classe_age)

# Suppression des lignes et colonnes ne contenant que des zéros
contingence_gaule_nord_deuxieme <- contingence_gaule_nord_deuxieme[rowSums(contingence_gaule_nord_deuxieme) > 0, ]
contingence_gaule_nord_deuxieme <- contingence_gaule_nord_deuxieme[, colSums(contingence_gaule_nord_deuxieme) > 0]

write.csv2(contingence_gaule_nord_deuxieme, "resultats/contingence_gaule_nord_deuxieme_age.csv", row.names = TRUE)

# --- 2.2 Analyse Factorielle des Correspondances (AFC) ---
# Contrairement au script équivalent pour la Gaule du sud, la catégorie
# "fibule" est ici mise en ligne supplémentaire (projetée sur les axes sans
# contribuer à leur calcul), probablement parce que son effectif ou son
# profil la rend atypique sur cet intervalle.
modalites_sup_deuxieme <- c("fibule")
indices_sup_deuxieme <- which(rownames(contingence_gaule_nord_deuxieme) %in% modalites_sup_deuxieme)

afc_gaule_nord_deuxieme <- CA(contingence_gaule_nord_deuxieme, row.sup = indices_sup_deuxieme, graph = FALSE)
plot.CA(afc_gaule_nord_deuxieme, autoLab = "yes")

summary(afc_gaule_nord_deuxieme)
explor(afc_gaule_nord_deuxieme)

# --- 2.3 Extraction des informations pour les lignes (catégories d'objets) ---
contrib_lignes <- afc_gaule_nord_deuxieme$row$contrib
coord_lignes <- afc_gaule_nord_deuxieme$row$coord
cos2_lignes <- afc_gaule_nord_deuxieme$row$cos2

df_lignes <- data.frame(
  Modalité = rownames(contrib_lignes),
  Contrib_Dim1 = contrib_lignes[, 1],
  Coord_Dim1 = coord_lignes[, 1],
  Cos2_Dim1 = cos2_lignes[, 1],
  Contrib_Dim2 = contrib_lignes[, 2],
  Coord_Dim2 = coord_lignes[, 2],
  Cos2_Dim2 = cos2_lignes[, 2]
)

# Export désactivé par défaut dans le script d'origine (chemin déjà corrigé
# si tu veux le réactiver)
#write.csv2(df_lignes, "resultats/contrib_lignes_age_gaule_nord_deuxieme.csv", row.names = TRUE)

# --- 2.4 Extraction des informations pour les colonnes (classes d'âge) ---
contrib_col <- afc_gaule_nord_deuxieme$col$contrib
coord_col <- afc_gaule_nord_deuxieme$col$coord
cos2_col <- afc_gaule_nord_deuxieme$col$cos2

df_col <- data.frame(
  Modalité = rownames(contrib_col),
  Contrib_Dim1 = contrib_col[, 1],
  Coord_Dim1 = coord_col[, 1],
  Cos2_Dim1 = cos2_col[, 1],
  Contrib_Dim2 = contrib_col[, 2],
  Coord_Dim2 = coord_col[, 2],
  Cos2_Dim2 = cos2_col[, 2]
)

# Export désactivé par défaut (chemin déjà corrigé si tu veux le réactiver)
#write.csv2(df_col, "resultats/contrib_col_age_gaule_nord_deuxieme.csv", row.names = TRUE)

# --- 2.5 Test du chi² et résidus de Pearson ---
chisq <- chisq.test(contingence_gaule_nord_deuxieme)
round(chisq$residuals, 3)

res <- chisq$residuals
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

# Export désactivé par défaut (chemin déjà corrigé si tu veux le réactiver)
#write.csv2(contrib, "resultats/contrib_pourcentage_gaule_nord_deuxieme.csv", row.names = TRUE)

corrplot(contrib, is.cor = FALSE)


# ============================================================
# 3. TROISIÈME INTERVALLE CHRONOLOGIQUE
# ============================================================

# --- 3.1 Sélection de l'intervalle et tableau de contingence ---
troisieme_intervalle <- levels(d_intervalle$intervalle_personnalise)[3]
d_intervalle_troisieme <- d_intervalle %>%
  filter(intervalle_personnalise == troisieme_intervalle)

d_intervalle_troisieme$defunt_age <- as.factor(d_intervalle_troisieme$defunt_age)

contingence_gaule_nord_troisieme <- table(d_intervalle_troisieme$nouvelle_categorie, d_intervalle_troisieme$defunt_classe_age)

# Export désactivé par défaut dans le script d'origine (chemin déjà corrigé
# si tu veux le réactiver) — NB : contrairement aux autres exports de ce
# script, celui-ci est fait avant le nettoyage des lignes/colonnes à zéro
# juste en dessous, donc le fichier obtenu si réactivé inclurait ces lignes vides
#write.csv2(contingence_gaule_nord_troisieme, "resultats/contingence_gaule_nord_troisieme_age.csv", row.names = TRUE)

# Suppression des lignes et colonnes ne contenant que des zéros
contingence_gaule_nord_troisieme <- contingence_gaule_nord_troisieme[rowSums(contingence_gaule_nord_troisieme) > 0, ]
contingence_gaule_nord_troisieme <- contingence_gaule_nord_troisieme[, colSums(contingence_gaule_nord_troisieme) > 0]

# --- 3.2 Analyse Factorielle des Correspondances (AFC) ---
# Trois catégories mises en ligne supplémentaire sur cet intervalle
modalites_sup_troisieme <- c("instrument écriture sur cire", "éclairage", "balsamaire")
indices_sup_troisieme <- which(rownames(contingence_gaule_nord_troisieme) %in% modalites_sup_troisieme)

afc_gaule_nord_troisieme <- CA(contingence_gaule_nord_troisieme, row.sup = indices_sup_troisieme, graph = FALSE)
plot.CA(afc_gaule_nord_troisieme, autoLab = "yes")

summary(afc_gaule_nord_troisieme)
explor(afc_gaule_nord_troisieme)

# --- 3.3 Extraction des informations pour les lignes (catégories d'objets) ---
contrib_lignes <- afc_gaule_nord_troisieme$row$contrib
coord_lignes <- afc_gaule_nord_troisieme$row$coord
cos2_lignes <- afc_gaule_nord_troisieme$row$cos2

# NB : ce tableau inclut la dimension 3 en plus des dimensions 1 et 2. Les
# colonnes "Contrib_Dim2", "Coord_Dim2" et "Cos2_Dim2" apparaissent deux fois
# (la deuxième occurrence concerne en réalité la dimension 3, malgré son nom).
# R renomme automatiquement les doublons avec le suffixe ".1" (aucune donnée
# perdue), mais l'intitulé de colonne reste trompeur à la lecture du CSV exporté.
df_lignes <- data.frame(
  Modalité = rownames(contrib_lignes),
  Contrib_Dim1 = contrib_lignes[, 1],
  Coord_Dim1 = coord_lignes[, 1],
  Cos2_Dim1 = cos2_lignes[, 1],
  Contrib_Dim2 = contrib_lignes[, 2],
  Coord_Dim2 = coord_lignes[, 2],
  Cos2_Dim2 = cos2_lignes[, 2],
  Contrib_Dim2 = contrib_lignes[, 3],   # NB : il s'agit en réalité de la dimension 3
  Coord_Dim2 = coord_lignes[, 3],       # idem
  Cos2_Dim2 = cos2_lignes[, 3]          # idem
)

write.csv2(df_lignes, "resultats/contrib_lignes_age_gaule_nord_troisieme.csv", row.names = TRUE)

# --- 3.4 Extraction des informations pour les colonnes (classes d'âge) ---
contrib_col <- afc_gaule_nord_troisieme$col$contrib
coord_col <- afc_gaule_nord_troisieme$col$coord
cos2_col <- afc_gaule_nord_troisieme$col$cos2

# Même remarque que ci-dessus concernant le nommage des colonnes
df_col <- data.frame(
  Modalité = rownames(contrib_col),
  Contrib_Dim1 = contrib_col[, 1],
  Coord_Dim1 = coord_col[, 1],
  Cos2_Dim1 = cos2_col[, 1],
  Contrib_Dim2 = contrib_col[, 2],
  Coord_Dim2 = coord_col[, 2],
  Cos2_Dim2 = cos2_col[, 2],
  Contrib_Dim2 = contrib_col[, 3],
  Coord_Dim2 = coord_col[, 3],
  Cos2_Dim2 = cos2_col[, 3]
)

write.csv2(df_col, "resultats/contrib_col_age_gaule_nord_troisieme.csv", row.names = TRUE)

# --- 3.5 Test du chi² et résidus de Pearson ---
chisq <- chisq.test(contingence_gaule_nord_troisieme)
round(chisq$residuals, 3)

res <- chisq$residuals
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

write.csv2(contrib, "resultats/contrib_pourcentage_gaule_nord_troisieme.csv", row.names = TRUE)

corrplot(contrib, is.cor = FALSE)