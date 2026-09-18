####
# Auteur : A. Turgis
# Date : 11/03/2025
# Sujet : KNN — Gaule du nord — prédiction de la classe d'âge (adulte /
# enfant) à partir du mobilier funéraire
#
# NB : l'en-tête du script original mentionnait "narbo" (reliquat de
# copier-coller depuis un script sur un autre corpus, comme pour le script
# forêt aléatoire/âge vu précédemment) — corrigé ci-dessus.
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(caret)
library(pROC)
library(ggplot2)


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
df <- d_filtered_gaule_nord[, c("nouvelle_categorie", "depot_nom", "defunt_genre", "zone_geo")]

# --- Encodage one-hot des variables prédictives ---
# Nécessaire pour le KNN, qui calcule des distances entre observations et a
# donc besoin d'entrées numériques (comme le SVM, contrairement à l'arbre de
# décision et à la forêt aléatoire).
df_one_hot <- dummyVars("~.", data = df)
df_encoded <- data.frame(predict(df_one_hot, newdata = df))
df_encoded$target <- factor(d_filtered_gaule_nord$defunt_classe_age, levels = c("adulte", "enfant"))

X_orig <- df_encoded[, !names(df_encoded) %in% c("target")]
y_orig <- df_encoded$target

cat("Distribution avant rééchantillonnage :\n")
print(table(y_orig))


# ============================================================
# 2. FONCTION D'ENTRAÎNEMENT DU MODÈLE KNN
# ============================================================
# Entraîne un modèle des k plus proches voisins (knn) avec validation croisée
# à 10 plis, en testant k = 1, 3, 5 (valeurs impaires pour éviter les
# égalités de vote entre les deux classes).
train_knn <- function(X, y, title) {
  set.seed(123)  # graine fixée pour la reproductibilité
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
    verboseIter = TRUE
  )
  
  model <- train(
    x = X, y = y,
    method = "knn",
    trControl = ctrl,
    tuneGrid = data.frame(k = seq(1, 6, by = 2)),  # k = 1, 3, 5
    metric = "ROC"
  )
  
  cat(title, "\n")
  print(model)
  plot(model)
  
  # --- Prédictions OOF pour le meilleur k ---
  oof_preds <- model$pred[model$pred$k == model$bestTune$k, ]
  
  oof_preds$pred <- factor(oof_preds$pred, levels = levels(y))
  oof_preds$obs  <- factor(oof_preds$obs,  levels = levels(y))
  
  cat("Distribution dans obs (OOF) :\n");  print(table(oof_preds$obs))
  cat("Distribution dans pred (OOF) :\n"); print(table(oof_preds$pred))
  
  conf_mat <- confusionMatrix(oof_preds$pred, oof_preds$obs, positive = "enfant")
  print(conf_mat$table)
  
  accuracy <- mean(oof_preds$pred == oof_preds$obs)
  cat(sprintf("Accuracy (OOF) : %.2f\n", accuracy))
  
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
  
  roc_obj   <- roc(oof_preds$obs, oof_preds[, "enfant"])
  auc_value <- auc(roc_obj)
  cat(sprintf("AUC-ROC (OOF) : %.2f\n", auc_value))
  
  cat(sprintf("AUC moyenne par fold : %.3f  |  Écart-type : %.3f\n",
              mean(model$resample$ROC),
              sd(model$resample$ROC)))
  
  roc_data <- data.frame(
    specificite = 1 - roc_obj$specificities,
    rappel      = roc_obj$sensitivities
  )
  
  print(
    ggplot() +
      geom_line(data = roc_data, aes(x = specificite, y = rappel, color = "Courbe ROC"), linewidth = 1.5) +
      geom_abline(aes(slope = 1, intercept = 0, linetype = "Modèle aléatoire"), color = "gray", linewidth = 1) +
      labs(title = paste(title, "(AUC =", round(auc_value, 2), ")"),
           x = "Taux de Faux Positifs", y = "Taux de Vrais Positifs") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text  = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "bottom")
  )
}


# ============================================================
# 3. ENTRAÎNEMENT DU MODÈLE
# ============================================================
train_knn(X_orig, y_orig, "KNN - Données originales")