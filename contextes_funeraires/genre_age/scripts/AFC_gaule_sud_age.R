###
# Auteur : A. Turgis
# Date : 12 janvier 2025
# Sujet : Gaule du sud : AFC — croisement mobilier / classe d'âge du défunt
# Trois intervalles chronologiques traités successivement dans ce script
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
source("scripts/preparation_donnees_Gaule_sud_age.R")


# ============================================================
# 2. PREMIER INTERVALLE CHRONOLOGIQUE
# ============================================================

# --- 2.1 Sélection de l'intervalle et tableau de contingence ---
premier_intervalle <- levels(d_intervalle$intervalle_personnalise)[1]
d_intervalle_premier <- d_intervalle %>%
  filter(intervalle_personnalise == premier_intervalle)

d_intervalle_premier$defunt_age <- as.factor(d_intervalle_premier$defunt_classe_age)

contingence_gaule_sud_premier <- table(d_intervalle_premier$nouvelle_categorie, d_intervalle_premier$defunt_classe_age)

# Suppression des lignes et colonnes ne contenant que des zéros
contingence_gaule_sud_premier <- contingence_gaule_sud_premier[rowSums(contingence_gaule_sud_premier) > 0, ]
contingence_gaule_sud_premier <- contingence_gaule_sud_premier[, colSums(contingence_gaule_sud_premier) > 0]

write.csv2(contingence_gaule_sud_premier, "resultats/contingence_gaule_sud_premier_age.csv", row.names = TRUE)

# --- 2.2 Analyse Factorielle des Correspondances (AFC) ---
afc_gaule_sud_premier <- CA(contingence_gaule_sud_premier, graph = FALSE)
plot.CA(afc_gaule_sud_premier, autolab = "yes")

summary(afc_gaule_sud_premier)
explor(afc_gaule_sud_premier)

# --- 2.3 Extraction des informations pour les lignes (catégories d'objets) ---
contrib_lignes <- afc_gaule_sud_premier$row$contrib
coord_lignes <- afc_gaule_sud_premier$row$coord
cos2_lignes <- afc_gaule_sud_premier$row$cos2

df_lignes <- data.frame(
  Modalité = rownames(contrib_lignes),
  Contrib_Dim1 = contrib_lignes[, 1],
  Coord_Dim1 = coord_lignes[, 1],
  Cos2_Dim1 = cos2_lignes[, 1],
  Contrib_Dim2 = contrib_lignes[, 2],
  Coord_Dim2 = coord_lignes[, 2],
  Cos2_Dim2 = cos2_lignes[, 2]
)

write.csv2(df_lignes, "resultats/contrib_lignes_age_gaule_sud_premier.csv", row.names = TRUE)

# --- 2.4 Extraction des informations pour les colonnes (classes d'âge) ---
contrib_col <- afc_gaule_sud_premier$col$contrib
coord_col <- afc_gaule_sud_premier$col$coord
cos2_col <- afc_gaule_sud_premier$col$cos2

df_col <- data.frame(
  Modalité = rownames(contrib_col),
  Contrib_Dim1 = contrib_col[, 1],
  Coord_Dim1 = coord_col[, 1],
  Cos2_Dim1 = cos2_col[, 1],
  Contrib_Dim2 = contrib_col[, 2],
  Coord_Dim2 = coord_col[, 2],
  Cos2_Dim2 = cos2_col[, 2]
)

write.csv2(df_col, "resultats/contrib_col_age_gaule_sud_premier.csv", row.names = TRUE)

# --- 2.5 Test du chi² et résidus de Pearson ---
chisq <- chisq.test(contingence_gaule_sud_premier)
round(chisq$residuals, 3)  # Résidus de Pearson (écart observé/attendu par cellule)

# Réordonnancement par force de déviation pour une lecture plus claire du corrplot
res <- chisq$residuals
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

# Contribution de chaque cellule au chi² total, en pourcentage : donne une
# indication de la nature de la dépendance entre lignes et colonnes
contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

write.csv2(contrib, "resultats/contrib_pourcentage_gaule_sud_premier.csv", row.names = TRUE)

corrplot(contrib, is.cor = FALSE)


# ============================================================
# 3. DEUXIÈME INTERVALLE CHRONOLOGIQUE
# ============================================================

# --- 3.1 Sélection de l'intervalle et tableau de contingence ---
deuxieme_intervalle <- levels(d_intervalle$intervalle_personnalise)[2]
d_intervalle_deuxieme <- d_intervalle %>%
  filter(intervalle_personnalise == deuxieme_intervalle)

d_intervalle_deuxieme$defunt_age <- as.factor(d_intervalle_deuxieme$defunt_age)

contingence_gaule_sud_deuxieme <- table(d_intervalle_deuxieme$nouvelle_categorie, d_intervalle_deuxieme$defunt_classe_age)

contingence_gaule_sud_deuxieme <- contingence_gaule_sud_deuxieme[rowSums(contingence_gaule_sud_deuxieme) > 0, ]
contingence_gaule_sud_deuxieme <- contingence_gaule_sud_deuxieme[, colSums(contingence_gaule_sud_deuxieme) > 0]

write.csv2(contingence_gaule_sud_deuxieme, "resultats/contingence_gaule_sud_deuxieme_age.csv", row.names = TRUE)

# --- 3.2 Analyse Factorielle des Correspondances (AFC) ---
afc_gaule_sud_deuxieme <- CA(contingence_gaule_sud_deuxieme, graph = FALSE)
plot.CA(afc_gaule_sud_deuxieme, autoLab = "yes")

summary(afc_gaule_sud_deuxieme)
explor(afc_gaule_sud_deuxieme)

# --- 3.3 Extraction des informations pour les lignes (catégories d'objets) ---
contrib_lignes <- afc_gaule_sud_deuxieme$row$contrib
coord_lignes <- afc_gaule_sud_deuxieme$row$coord
cos2_lignes <- afc_gaule_sud_deuxieme$row$cos2


df_lignes <- data.frame(
  Modalité = rownames(contrib_lignes),
  Contrib_Dim1 = contrib_lignes[, 1],
  Coord_Dim1 = coord_lignes[, 1],
  Cos2_Dim1 = cos2_lignes[, 1],
  Contrib_Dim2 = contrib_lignes[, 2],
  Coord_Dim2 = coord_lignes[, 2],
  Cos2_Dim2 = cos2_lignes[, 2],
  Contrib_Dim3 = contrib_lignes[, 3],   
  Coord_Dim3 = coord_lignes[, 3],       
  Cos2_Dim3 = cos2_lignes[, 3]          
)

write.csv2(df_lignes, "resultats/contrib_lignes_age_gaule_sud_deuxieme.csv", row.names = TRUE)

# --- 3.4 Extraction des informations pour les colonnes (classes d'âge) ---
contrib_col <- afc_gaule_sud_deuxieme$col$contrib
coord_col <- afc_gaule_sud_deuxieme$col$coord
cos2_col <- afc_gaule_sud_deuxieme$col$cos2

df_col <- data.frame(
  Modalité = rownames(contrib_col),
  Contrib_Dim1 = contrib_col[, 1],
  Coord_Dim1 = coord_col[, 1],
  Cos2_Dim1 = cos2_col[, 1],
  Contrib_Dim2 = contrib_col[, 2],
  Coord_Dim2 = coord_col[, 2],
  Cos2_Dim2 = cos2_col[, 2],
  Contrib_Dim3 = contrib_col[, 3],
  Coord_Dim3 = coord_col[, 3],
  Cos2_Dim3 = cos2_col[, 3]
)

write.csv2(df_col, "resultats/contrib_col_age_gaule_sud_deuxieme.csv", row.names = TRUE)

# --- 3.5 Test du chi² et résidus de Pearson ---
chisq <- chisq.test(contingence_gaule_sud_deuxieme)
round(chisq$residuals, 3)

res <- chisq$residuals
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

write.csv2(contrib, "resultats/contrib_pourcentage_gaule_sud_deuxieme.csv", row.names = TRUE)

corrplot(contrib, is.cor = FALSE)


# ============================================================
# 4. TROISIÈME INTERVALLE CHRONOLOGIQUE
# ============================================================

# --- 4.1 Sélection de l'intervalle et tableau de contingence ---
troisieme_intervalle <- levels(d_intervalle$intervalle_personnalise)[3]
d_intervalle_troisieme <- d_intervalle %>%
  filter(intervalle_personnalise == troisieme_intervalle)

d_intervalle_troisieme$defunt_age <- as.factor(d_intervalle_troisieme$defunt_age)

contingence_gaule_sud_troisieme <- table(d_intervalle_troisieme$nouvelle_categorie, d_intervalle_troisieme$defunt_classe_age)

contingence_gaule_sud_troisieme <- contingence_gaule_sud_troisieme[rowSums(contingence_gaule_sud_troisieme) > 0, ]
contingence_gaule_sud_troisieme <- contingence_gaule_sud_troisieme[, colSums(contingence_gaule_sud_troisieme) > 0]

write.csv2(contingence_gaule_sud_troisieme, "resultats/contingence_gaule_sud_troisieme_age.csv", row.names = TRUE)

# --- 4.2 Analyse Factorielle des Correspondances (AFC) ---
afc_gaule_sud_troisieme <- CA(contingence_gaule_sud_troisieme, graph = FALSE)
plot.CA(afc_gaule_sud_troisieme, autoLab = "yes")

summary(afc_gaule_sud_troisieme)
explor(afc_gaule_sud_troisieme)

# --- 4.3 Extraction des informations pour les lignes (catégories d'objets) ---
contrib_lignes <- afc_gaule_sud_troisieme$row$contrib
coord_lignes <- afc_gaule_sud_troisieme$row$coord
cos2_lignes <- afc_gaule_sud_troisieme$row$cos2

df_lignes <- data.frame(
  Modalité = rownames(contrib_lignes),
  Contrib_Dim1 = contrib_lignes[, 1],
  Coord_Dim1 = coord_lignes[, 1],
  Cos2_Dim1 = cos2_lignes[, 1],
  Contrib_Dim2 = contrib_lignes[, 2],
  Coord_Dim2 = coord_lignes[, 2],
  Cos2_Dim2 = cos2_lignes[, 2],
  Contrib_Dim3 = contrib_lignes[, 3], 
  Coord_Dim3 = coord_lignes[, 3],   
  Cos2_Dim3 = cos2_lignes[, 3]        
)

write.csv2(df_lignes, "resultats/contrib_lignes_age_gaule_sud_troisieme.csv", row.names = TRUE)

# --- 4.4 Extraction des informations pour les colonnes (classes d'âge) ---
contrib_col <- afc_gaule_sud_troisieme$col$contrib
coord_col <- afc_gaule_sud_troisieme$col$coord
cos2_col <- afc_gaule_sud_troisieme$col$cos2

df_col <- data.frame(
  Modalité = rownames(contrib_col),
  Contrib_Dim1 = contrib_col[, 1],
  Coord_Dim1 = coord_col[, 1],
  Cos2_Dim1 = cos2_col[, 1],
  Contrib_Dim2 = contrib_col[, 2],
  Coord_Dim2 = coord_col[, 2],
  Cos2_Dim2 = cos2_col[, 2],
  Contrib_Dim3 = contrib_col[, 3],
  Coord_Dim3 = coord_col[, 3],
  Cos2_Dim3 = cos2_col[, 3]
)

write.csv2(df_col, "resultats/contrib_col_age_gaule_sud_troisieme.csv", row.names = TRUE)

# --- 4.5 Test du chi² et résidus de Pearson ---
chisq <- chisq.test(contingence_gaule_sud_troisieme)
round(chisq$residuals, 3)

res <- chisq$residuals
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

write.csv2(contrib, "resultats/contrib_pourcentage_gaule_sud_troisieme.csv", row.names = TRUE)

corrplot(contrib, is.cor = FALSE)
