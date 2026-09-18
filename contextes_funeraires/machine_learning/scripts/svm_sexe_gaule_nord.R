####
# SVM - Gaule du nord — prédiction du sexe du défunt (féminin / masculin) à
# partir du mobilier funéraire
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(dplyr)
library(caret)
library(ggplot2)
library(reshape2)
library(e1071)
library(ROCR)
library(pROC)
library(tibble)
library(stringr)
library(MLmetrics)


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
# 2. FONCTION DE CALCUL DU MCC (Matthews Correlation Coefficient)
# ============================================================
calc_mcc_manual <- function(cm) {
  TP <- as.numeric(cm["féminin",  "féminin"])
  TN <- as.numeric(cm["masculin", "masculin"])
  FP <- as.numeric(cm["masculin", "féminin"])   # prédit masculin, réel féminin
  FN <- as.numeric(cm["féminin",  "masculin"])  # prédit féminin, réel masculin
  
  numerator   <- (TP * TN) - (FP * FN)
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  if (is.na(denominator) || denominator == 0) return(NA)
  return(numerator / denominator)
}


# ============================================================
# 3. FONCTION D'ENTRAÎNEMENT DU MODÈLE SVM
# ============================================================
# Même logique que le script équivalent sur l'âge, avec en plus le calcul et
# le tracé d'une courbe ROC/AUC (absents du script âge).
train_svm <- function(X, y, title, seed = 123) {
  set.seed(seed)
  
  X <- X[, apply(X, 2, function(col) length(unique(col)) > 1)]
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE,
    savePredictions = "final"  # Nécessaire pour récupérer les prédictions OOF
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
  
  # --- Prédictions OOF (cohérent avec l'arbre de décision) ---
  oof_preds <- model$pred[
    model$pred$sigma == model$bestTune$sigma &
      model$pred$C     == model$bestTune$C, 
  ]
  
  oof_preds$pred <- factor(oof_preds$pred, levels = levels(y))
  oof_preds$obs  <- factor(oof_preds$obs,  levels = levels(y))
  
  cat("Distribution dans obs (OOF) :\n");  print(table(oof_preds$obs))
  cat("Distribution dans pred (OOF) :\n"); print(table(oof_preds$pred))
  
  conf_mat <- confusionMatrix(oof_preds$pred, oof_preds$obs, positive = "féminin")
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
  
  # --- Courbe ROC / AUC sur les prédictions OOF ---
  roc_obj   <- roc(oof_preds$obs, oof_preds[, "féminin"])
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
# 4. ENTRAÎNEMENT DU MODÈLE
# ============================================================
train_svm(X_orig, y_orig, "SVM - Données originales")