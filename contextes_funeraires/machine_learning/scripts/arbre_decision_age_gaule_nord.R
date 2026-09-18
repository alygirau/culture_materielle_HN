####
# Date : 11/03/2025
# Sujet : arbre de décision sur les données de Gaule du nord — prédiction de
# la classe d'âge (adulte / enfant) à partir du mobilier funéraire
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(caret)        # Entraînement et validation croisée du modèle (train())
library(rpart.plot)   # Visualisation graphique de l'arbre de décision
library(ggplot2)      # Graphiques (courbe cp, matrice de confusion)


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
df$target <- factor(d_filtered_gaule_nord$defunt_classe_age, levels = c("adulte", "enfant"))

X_orig <- df[, !names(df) %in% c("target")]
y_orig <- df$target

cat("Distribution avant rééchantillonnage :\n")
print(table(y_orig))


# ============================================================
# 2. FONCTION DE CALCUL DU MCC (Matthews Correlation Coefficient)
# ============================================================
# Métrique équilibrée, utile en cas de classes déséquilibrées. Calcul manuel
# avec indices FP/FN corrigés (Prediction en ligne, Reference en colonne).
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
# 3. FONCTION D'ENTRAÎNEMENT DE L'ARBRE DE DÉCISION
# ============================================================
# Entraîne un arbre de décision (rpart) avec validation croisée à 10 plis,
# recherche du meilleur paramètre de complexité (cp) par grille, puis évalue
# les performances sur les prédictions "out-of-fold" (OOF). NB : contrairement
# aux scripts SVM vus précédemment, aucun encodage one-hot n'est nécessaire
# ici — rpart gère nativement les variables catégorielles (facteurs).
train_decision_tree <- function(X, y, title) {
  set.seed(123)  # graine fixée pour la reproductibilité
  
  # Grille de recherche du paramètre de complexité (cp) : plus cp est petit,
  # plus l'arbre peut être complexe (risque de sur-apprentissage)
  grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))
  
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
    method = "rpart",
    trControl = ctrl,
    tuneGrid = grid,
    metric = "ROC"
  )
  
  cat(title, "\n")
  print(model)
  plot(model)  # Évolution de la métrique ROC selon la valeur de cp
  
  # Visualisation graphique de l'arbre final retenu
  rpart.plot(model$finalModel, type = 2, extra = 1, under = TRUE, cex = 0.8)
  
  # --- Extraction des prédictions OOF pour le meilleur cp ---
  oof_preds <- model$pred[model$pred$cp == model$bestTune$cp, ]
  
  oof_preds$pred <- factor(oof_preds$pred, levels = levels(y))
  oof_preds$obs  <- factor(oof_preds$obs,  levels = levels(y))
  
  cat("Distribution dans obs (OOF) :\n");  print(table(oof_preds$obs))
  cat("Distribution dans pred (OOF) :\n"); print(table(oof_preds$pred))
  
  # --- Matrice de confusion et métriques dérivées ---
  conf_mat <- confusionMatrix(oof_preds$pred, oof_preds$obs, positive = "enfant")
  print(conf_mat$table)
  
  accuracy <- mean(oof_preds$pred == oof_preds$obs)
  cat(sprintf("Accuracy (OOF) : %.2f\n", accuracy))
  
  mcc <- calc_mcc_manual(conf_mat$table)
  cat(sprintf("MCC (OOF) : %.2f\n", mcc))
  
  # --- Visualisation de la matrice de confusion sous forme de heatmap ---
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
  
  # --- Précision et rappel par classe ---
  precision_enfant <- conf_mat$byClass["Pos Pred Value"]
  precision_adulte <- conf_mat$byClass["Neg Pred Value"]
  cat(sprintf("Précision (enfant) : %.2f\n", precision_enfant))
  cat(sprintf("Précision (adulte) : %.2f\n", precision_adulte))
  
  rappel_enfant <- conf_mat$byClass["Sensitivity"]
  rappel_adulte <- conf_mat$byClass["Specificity"]
  cat(sprintf("Rappel (enfant) : %.2f\n", rappel_enfant))
  cat(sprintf("Rappel (adulte) : %.2f\n", rappel_adulte))
  
  # --- F1-score ---
  precision <- conf_mat$byClass["Pos Pred Value"]
  rappel    <- conf_mat$byClass["Sensitivity"]
  f1_score  <- 2 * (precision * rappel) / (precision + rappel)
  cat(sprintf("F1-score : %.2f\n", f1_score))
  
  # --- Accuracy théorique au hasard (référence de comparaison) ---
  proportions <- prop.table(table(y))
  accuracy_hasard <- sum(proportions^2)
  cat("Accuracy au hasard :", accuracy_hasard, "\n")
}


# ============================================================
# 4. ENTRAÎNEMENT DU MODÈLE
# ============================================================

train_decision_tree(X_orig, y_orig, "Arbre de décision - Données originales")
