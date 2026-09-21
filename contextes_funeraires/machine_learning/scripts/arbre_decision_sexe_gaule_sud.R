####
# Sujet : arbre de décision sur les données de Gaule du sud — prédiction du
# sexe du défunt (féminin / masculin) à partir du mobilier funéraire
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(caret)
library(rpart.plot)
library(pROC)     # Courbe ROC et AUC
library(ggplot2)
library(dplyr)


# ============================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
source("scripts/preparation_donnees_Gaule_sud_sexe_commente.R")

# --- Simplification du sexe en 2 classes strictes ---
# Les déterminations "probables" sont fusionnées avec leur catégorie certaine,
# et les sépultures au sexe indéterminé sont exclues (le modèle ne peut
# prédire que 2 classes : féminin ou masculin).
d_filtered_gaule_sud <- d_filtered_gaule_sud %>%
  mutate(defunt_genre = case_when(
    defunt_genre %in% c("féminin probable") ~ "féminin",
    defunt_genre %in% c("masculin probable") ~ "masculin",
    TRUE ~ defunt_genre
  )) %>%
  filter(defunt_genre != "indéterminé")

d_filtered_gaule_sud$defunt_classe_age <- as.factor(d_filtered_gaule_sud$defunt_classe_age)
d_filtered_gaule_sud$defunt_genre      <- as.factor(d_filtered_gaule_sud$defunt_genre)

# NB : contrairement au script équivalent sur l'âge, celui-ci n'a pas besoin
# de fusionner defunt_classe_age en grandes catégories — la classe d'âge sert
# ici de simple variable prédictive (parmi d'autres), pas de variable cible.
df <- d_filtered_gaule_sud[, c("nouvelle_categorie", "depot_nom", "defunt_classe_age")]
df$target <- factor(d_filtered_gaule_sud$defunt_genre, levels = c("féminin", "masculin"))

X_orig <- df[, !names(df) %in% c("target")]
y_orig <- df$target

cat("Nombre de variables prédictives :", ncol(X_orig), "\n")
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
# 3. FONCTION D'ENTRAÎNEMENT DE L'ARBRE DE DÉCISION
# ============================================================
# Même logique que le script équivalent sur l'âge, avec en plus le calcul et
# le tracé d'une courbe ROC/AUC (absents du script âge).
train_decision_tree <- function(X, y, title) {
  set.seed(123)
  
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
  plot(model)
  
  rpart.plot(model$finalModel, type = 2, extra = 1, under = TRUE, cex = 0.8)
  
  # --- Prédictions OOF ---
  oof_preds <- model$pred[model$pred$cp == model$bestTune$cp, ]
  
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
  
  # --- Précision, rappel, F1 ---
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
train_decision_tree(X_orig, y_orig, "Arbre de décision - Données originales")