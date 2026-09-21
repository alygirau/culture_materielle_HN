####
# Sujet : Gaule du sud — forêt aléatoire pour la prédiction de la classe
# d'âge (adulte / enfant) à partir du mobilier funéraire
#
# NB : l'en-tête du script original mentionnait "narbo" (probablement un
# reliquat de copier-coller depuis un script sur un autre corpus/site) — j'ai
# corrigé le sujet ci-dessus pour refléter le contenu réel du script.
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


# ============================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
source("scripts/preparation_donnees_Gaule_sud_age_commente.R")

# ATTENTION : la ligne ci-dessous suppose que defunt_classe_age ne contient QUE deux
# valeurs, "adulte" et "enfant". Pour que ce script fonctionne comme prévu, il faut 
# activer la section 5 du script de préparation (fusion grossière) et désactiver la 
# section 6, avant de lancer ce script.
df <- d_filtered_gaule_sud[, c("nouvelle_categorie", "depot_nom", "defunt_genre", "zone_geo")]
df$target <- factor(d_filtered_gaule_sud$defunt_classe_age, levels = c("adulte", "enfant"))

X_orig <- df[, !names(df) %in% c("target")]
y_orig <- df$target

cat("Distribution avant rééchantillonnage :\n")
print(table(y_orig))


# ============================================================
# 2. FONCTION D'ENTRAÎNEMENT DE LA FORÊT ALÉATOIRE
# ============================================================
# Entraîne une forêt aléatoire (randomForest, via caret) avec validation
# croisée à 10 plis et recherche du meilleur nombre de variables tirées à
# chaque split (mtry) parmi 3 valeurs réparties sur l'intervalle possible.
# NB : comme pour l'arbre de décision, aucun encodage one-hot n'est
# nécessaire — randomForest gère nativement les variables catégorielles.
train_random_forest <- function(X, y, title) {
  set.seed(123)  # graine fixée pour la reproductibilité
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
    verboseIter = TRUE
  )
  
  # Grille de recherche de mtry : 3 valeurs réparties entre 1 et le nombre
  # total de variables prédictives
  nb_pred <- ncol(X)
  grid <- expand.grid(mtry = unique(floor(seq(1, nb_pred, length.out = 3))))
  
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
  
  # --- Prédictions OOF pour le meilleur mtry ---
  oof_preds <- model$pred[model$pred$mtry == model$bestTune$mtry, ]
  
  oof_preds$pred <- factor(oof_preds$pred, levels = levels(y))
  oof_preds$obs  <- factor(oof_preds$obs,  levels = levels(y))
  
  cat("Distribution dans obs (OOF) :\n");  print(table(oof_preds$obs))
  cat("Distribution dans pred (OOF) :\n"); print(table(oof_preds$pred))
  
  conf_mat <- confusionMatrix(oof_preds$pred, oof_preds$obs, positive = "enfant")
  print(conf_mat$table)
  
  accuracy <- mean(oof_preds$pred == oof_preds$obs)
  cat(sprintf("Accuracy (OOF) : %.2f\n", accuracy))
  
  # --- Précision, rappel, F1 ---
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
  
  # --- Courbe ROC / AUC sur les prédictions OOF ---
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
train_random_forest(X_orig, y_orig, "Forêt Aléatoire - Données originales")
