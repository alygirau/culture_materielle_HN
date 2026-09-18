####
# Date : 31 déc. 2024
# Sujet : Gaule du nord — forêt aléatoire pour la prédiction du sexe du
# défunt (féminin / masculin) à partir du mobilier funéraire
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(caret)
library(randomForest)
library(ggplot2)
library(reshape2)
library(pROC)
library(PRROC)
library(dplyr)


# ============================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
source("scripts/preparation_donnees_Gaule_nord_sexe.R")

# --- Simplification du sexe en 2 classes strictes ---
# Même traitement que le script arbre de décision équivalent : fusion des
# déterminations "probables" avec leur catégorie certaine, exclusion des
# sépultures au sexe indéterminé.
d_filtered_gaule_nord <- d_filtered_gaule_nord %>%
  mutate(defunt_genre = case_when(
    defunt_genre %in% c("féminin probable") ~ "féminin",
    defunt_genre %in% c("masculin probable") ~ "masculin",
    TRUE ~ defunt_genre
  )) %>%
  filter(defunt_genre != "indéterminé")

d_filtered_gaule_nord$defunt_classe_age <- as.factor(d_filtered_gaule_nord$defunt_classe_age)
d_filtered_gaule_nord$defunt_genre <- as.factor(d_filtered_gaule_nord$defunt_genre)

df <- d_filtered_gaule_nord[, c("nouvelle_categorie", "depot_nom", "defunt_classe_age")]
df$target <- factor(d_filtered_gaule_nord$defunt_genre, levels = c("féminin", "masculin"))

X_orig <- df[, !names(df) %in% c("target")]
y_orig <- df$target

cat("Distribution avant rééchantillonnage :\n")
print(table(y_orig))


# ============================================================
# 2. FONCTION D'ENTRAÎNEMENT DE LA FORÊT ALÉATOIRE
# ============================================================
train_random_forest <- function(X, y, title) {
  set.seed(123)
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",  # Permet de récupérer les prédictions out-of-fold
    verboseIter = TRUE
  )
  
  # Grille de recherche de mtry, avec 3 valeurs classiques (2, racine carrée
  # du nombre de prédicteurs, moitié du nombre de prédicteurs) plutôt que la
  # répartition régulière utilisée dans le script équivalent sur l'âge
  grid <- expand.grid(mtry = unique(floor(c(2, sqrt(ncol(X)), ncol(X) / 2))))
  
  model <- train(
    x = X, y = y,
    method = "rf",
    trControl = ctrl,
    tuneGrid = grid,
    metric = "ROC",  
    ntree = 500
  )
  
  cat(title, "\n")
  print(model)
  plot(model)
  
  # --- Métriques sur prédictions out-of-fold (OOF) ---
  oof_preds <- model$pred[model$pred$mtry == model$bestTune$mtry, ]
  
  accuracy <- mean(oof_preds$pred == oof_preds$obs)
  cat(sprintf("Accuracy (OOF) : %.2f\n", accuracy))
  
  conf_mat <- confusionMatrix(oof_preds$pred, oof_preds$obs, positive = "féminin")
  print(conf_mat$table)
  
  precision_feminin  <- conf_mat$byClass["Pos Pred Value"]
  precision_masculin <- conf_mat$byClass["Neg Pred Value"]
  cat(sprintf("Précision (féminin) : %.2f\n", precision_feminin))
  cat(sprintf("Précision (masculin) : %.2f\n", precision_masculin))
  
  rappel_feminin  <- conf_mat$byClass["Sensitivity"]
  rappel_masculin <- conf_mat$byClass["Specificity"]
  cat(sprintf("Rappel (féminin) : %.2f\n", rappel_feminin))
  cat(sprintf("Rappel (masculin) : %.2f\n", rappel_masculin))
  
  precision <- conf_mat$byClass["Pos Pred Value"]
  rappel    <- conf_mat$byClass["Sensitivity"]
  f1_score  <- 2 * (precision * rappel) / (precision + rappel)
  cat(sprintf("F1-score : %.2f\n", f1_score))
  
  proportions <- prop.table(table(y))
  accuracy_hasard <- sum(proportions^2)
  cat("Accuracy au hasard :", accuracy_hasard, "\n")
  
  # --- AUC-ROC sur prédictions OOF ---
  roc_obj   <- roc(oof_preds$obs, oof_preds[, "féminin"])
  auc_value <- auc(roc_obj)
  cat(sprintf("AUC-ROC (OOF) : %.2f\n", auc_value))
  
  cat(sprintf("AUC moyenne par fold : %.3f  |  Écart-type : %.3f\n",
              mean(model$resample$ROC),
              sd(model$resample$ROC)))
  
  roc_data <- data.frame(specificite = 1 - roc_obj$specificities, rappel = roc_obj$sensitivities)
  
  ggplot() +
    geom_line(data = roc_data, aes(x = specificite, y = rappel, color = "Courbe ROC"), linewidth = 1.5) +
    geom_abline(aes(slope = 1, intercept = 0, linetype = "Modèle aléatoire"), color = "gray", linewidth = 1) +
    labs(title = paste(title, "(AUC =", round(auc_value, 2), ")"),
         x = "Taux de Faux Positifs", y = "Taux de Vrais Positifs") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "bottom")
}


# ============================================================
# 3. ENTRAÎNEMENT DU MODÈLE
# ============================================================
train_random_forest(X_orig, y_orig, "Forêt Aléatoire - Données originales")