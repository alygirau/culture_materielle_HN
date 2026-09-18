####
# Date : 11/03/2025
# Sujet : KNN — Gaule du nord — prédiction du sexe du défunt (féminin /
# masculin) à partir du mobilier funéraire
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(caret)
library(pROC)
library(ggplot2)
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
d_filtered_gaule_nord <- d_filtered_gaule_nord %>%
  mutate(defunt_genre = case_when(
    defunt_genre %in% c("féminin probable") ~ "féminin",
    defunt_genre %in% c("masculin probable") ~ "masculin",
    TRUE ~ defunt_genre
  )) %>%
  filter(defunt_genre != "indéterminé")

d_filtered_gaule_nord$defunt_classe_age <- as.factor(d_filtered_gaule_nord$defunt_classe_age)
d_filtered_gaule_nord$defunt_genre      <- as.factor(d_filtered_gaule_nord$defunt_genre)

df <- d_filtered_gaule_nord[, c("nouvelle_categorie", "depot_nom", "defunt_classe_age")]

# --- Encodage one-hot des variables prédictives ---
df_one_hot <- dummyVars("~.", data = df)
df_encoded <- data.frame(predict(df_one_hot, newdata = df))
df_encoded$target <- factor(d_filtered_gaule_nord$defunt_genre, levels = c("féminin", "masculin"))

X_orig <- df_encoded[, !names(df_encoded) %in% c("target")]
y_orig <- df_encoded$target

cat("Distribution avant rééchantillonnage :\n")
print(table(y_orig))


# ============================================================
# 2. FONCTION D'ENTRAÎNEMENT DU MODÈLE KNN
# ============================================================
# Même logique que le script équivalent sur l'âge, à deux différences près :
# - tuneLength = 10 (caret choisit automatiquement 10 valeurs de k à tester)
#   au lieu d'une grille fixe k = 1, 3, 5
# - le dernier ggplot() (courbe ROC) n'est pas enveloppé dans print() (voir
#   remarque plus bas)
train_knn <- function(X, y, title) {
  set.seed(123)
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",  # Nécessaire pour récupérer les prédictions OOF
    verboseIter = TRUE
  )
  
  model <- train(
    x = X, y = y,
    method = "knn",
    trControl = ctrl,
    tuneLength = 10,
    metric = "ROC"
  )
  
  cat(title, "\n")
  print(model)
  plot(model)
  
  # --- Métriques sur prédictions out-of-fold (OOF) ---
  oof_preds <- model$pred[model$pred$k == model$bestTune$k, ]
  
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
  
  roc_obj   <- roc(oof_preds$obs, oof_preds[, "féminin"])
  auc_value <- auc(roc_obj)
  cat(sprintf("AUC-ROC (OOF) : %.2f\n", auc_value))
  
  cat(sprintf("AUC moyenne par fold : %.3f  |  Écart-type : %.3f\n",
              mean(model$resample$ROC),
              sd(model$resample$ROC)))
  
  roc_data <- data.frame(specificite = 1 - roc_obj$specificities, rappel = roc_obj$sensitivities)
  
  # NB : comme dans le script forêt aléatoire/sexe, ce ggplot n'est pas
  # enveloppé dans print() — à l'intérieur d'une fonction, il ne s'affichera
  # donc pas automatiquement lors de l'appel à train_knn(). Ajoute print()
  # autour de l'appel ci-dessous si tu veux que la courbe ROC s'affiche.
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
train_knn(X_orig, y_orig, "KNN - Données originales")