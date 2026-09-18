####
# SVM - Gaule du nord — prédiction de la classe d'âge (adulte / enfant) à
# partir du mobilier funéraire
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(dplyr)
library(caret)       # Entraînement et validation croisée du modèle (train())
library(ggplot2)     # Graphiques (matrice de confusion)
library(reshape2)    # Remise en forme de tableaux (chargé pour compatibilité)
library(e1071)       # Moteur SVM utilisé en arrière-plan par caret (method = "svmRadial")
library(ROCR)        # Courbes ROC (chargé pour compatibilité)
library(pROC)        # Calcul et visualisation de courbes ROC / AUC
library(tibble)      # Manipulation de tableaux
library(stringr)     # Manipulation de chaînes de caractères
library(MLmetrics)   # Métriques de machine learning complémentaires


# ============================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
source("scripts/preparation_donnees_Gaule_nord_age.R")

# ATTENTION : la ligne ci-dessous suppose que defunt_classe_age ne contient QUE deux
# valeurs, "adulte" et "enfant". Pour que ce script fonctionne comme prévu, il faut 
# activer la section 5 du script de préparation (fusion grossière) et désactiver la 
# section 6, avant de lancer ce script.
df <- d_filtered_gaule_nord[, c("nouvelle_categorie", "depot_nom", "defunt_genre")]

# --- Encodage one-hot des variables prédictives ---
# Nécessaire ici (contrairement à l'arbre de décision et à la forêt
# aléatoire) car le SVM ne gère pas nativement les variables catégorielles.
df_one_hot <- dummyVars("~.", data = df)
df_encoded <- data.frame(predict(df_one_hot, newdata = df))

df_encoded$target <- factor(d_filtered_gaule_nord$defunt_classe_age, levels = c("adulte", "enfant"))

X_orig <- df_encoded[, !names(df_encoded) %in% c("target")]
y_orig <- df_encoded$target

cat("Distribution avant rééchantillonnage :\n")
print(table(y_orig))


# ============================================================
# 2. FONCTION DE CALCUL DU MCC (Matthews Correlation Coefficient)
# ============================================================
# Métrique équilibrée, utile en cas de classes déséquilibrées. Calcul manuel
# avec indices FP/FN corrects (Prediction en ligne, Reference en colonne).
calc_mcc_manual <- function(cm) {
  TP <- as.numeric(cm["enfant", "enfant"])
  TN <- as.numeric(cm["adulte", "adulte"])
  FP <- as.numeric(cm["adulte", "enfant"])   # prédit adulte, réel enfant
  FN <- as.numeric(cm["enfant", "adulte"])   # prédit enfant, réel adulte
  
  numerator   <- (TP * TN) - (FP * FN)
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  if (is.na(denominator) || denominator == 0) return(NA)
  return(numerator / denominator)
}


# ============================================================
# 3. FONCTION D'ENTRAÎNEMENT DU MODÈLE SVM
# ============================================================
# Entraîne un SVM à noyau radial (svmRadial) avec validation croisée à 10
# plis, recherche des meilleurs hyperparamètres (sigma, C) par grille, puis
# évalue les performances sur les prédictions "out-of-fold" (OOF).
train_svm <- function(X, y, title, seed = 123) {
  set.seed(seed)  # graine fixée pour la reproductibilité
  
  # Suppression des colonnes constantes (une seule valeur unique), qui
  # n'apportent aucune information et peuvent poser problème au SVM
  X <- X[, apply(X, 2, function(col) length(unique(col)) > 1)]
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE,
    savePredictions = "final"
  )
  
  grid <- expand.grid(sigma = c(0.01, 0.05, 0.1), C = c(0.1, 1, 10))
  
  model <- train(
    x = X, y = y,
    method = "svmRadial",
    trControl = ctrl,
    tuneGrid = grid,
    metric = "ROC"
  )
  
  cat(title, "\n")
  print(model)
  
  # --- Extraction des prédictions OOF pour le meilleur jeu d'hyperparamètres ---
  oof_preds <- model$pred[
    model$pred$sigma == model$bestTune$sigma &
      model$pred$C     == model$bestTune$C,
  ]
  
  oof_preds$pred <- factor(oof_preds$pred, levels = levels(y))
  oof_preds$obs  <- factor(oof_preds$obs,  levels = levels(y))
  
  cat("Distribution dans obs (OOF) :\n");  print(table(oof_preds$obs))
  cat("Distribution dans pred (OOF) :\n"); print(table(oof_preds$pred))
  
  conf_mat <- confusionMatrix(oof_preds$pred, oof_preds$obs, positive = "enfant")
  print(conf_mat$table)
  
  accuracy <- mean(oof_preds$pred == oof_preds$obs)
  cat(sprintf("Accuracy (OOF) : %.2f\n", accuracy))
  
  mcc <- calc_mcc_manual(conf_mat$table)
  cat(sprintf("MCC (OOF) : %.2f\n", mcc))
  
  # --- Matrice de confusion (heatmap) ---
  cm_melted <- as.data.frame(as.table(conf_mat$table))
  colnames(cm_melted) <- c("Prédiction", "Réel", "Fréquence")
  print(
    ggplot(cm_melted, aes(x = Réel, y = Prédiction, fill = Fréquence)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "blue") +
      geom_text(aes(label = Fréquence), color = "black", size = 6) +
      labs(title = "Matrice de Confusion (OOF)", x = "Réel", y = "Prédiction") +
      theme_minimal(base_size = 14)
  )
  
  precision_enfant <- conf_mat$byClass["Pos Pred Value"]
  precision_adulte <- conf_mat$byClass["Neg Pred Value"]
  cat(sprintf("Précision (enfant) : %.2f\n", precision_enfant))
  cat(sprintf("Précision (adulte) : %.2f\n", precision_adulte))
  
  rappel_enfant <- conf_mat$byClass["Sensitivity"]
  rappel_adulte <- conf_mat$byClass["Specificity"]
  cat(sprintf("Rappel (enfant) : %.2f\n", rappel_enfant))
  cat(sprintf("Rappel (adulte) : %.2f\n", rappel_adulte))
  
  precision <- conf_mat$byClass["Pos Pred Value"]
  rappel    <- conf_mat$byClass["Sensitivity"]
  f1_score  <- 2 * (precision * rappel) / (precision + rappel)
  cat(sprintf("F1-score : %.2f\n", f1_score))
  
  proportions <- prop.table(table(y))
  accuracy_hasard <- sum(proportions^2)
  cat("Accuracy au hasard :", accuracy_hasard, "\n")
}


# ============================================================
# 4. ENTRAÎNEMENT DU MODÈLE
# ============================================================
train_svm(X_orig, y_orig, "SVM")
