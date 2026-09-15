####
# Analyse de réseau : cooccurrence d'objets par classe d'âge du défunt et
# intervalle chronologique, à l'échelle des contextes funéraires (US)
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)


# ============================================================
# 1. CHARGEMENT DES DONNÉES PRÉPARÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.

# Ce script réutilise l'objet d_intervalle créé par le script de préparation
# générale. Le nom de fichier ci-dessous doit correspondre exactement à celui
# du script de préparation présent dans scripts/.
source("scripts/preparation_donnees_generales.R")

dir.create("resultats/cooccurrence_age", showWarnings = FALSE, recursive = TRUE)

# --- Préparation des sous-corpus par zone géographique ---
d_intervalle_gaule_nord <- d_intervalle %>% filter(zone_geo == "Gaule du nord")
d_intervalle_gaule_sud <- d_intervalle %>% filter(zone_geo == "Gaule du sud")


# ============================================================
# 2. FONCTIONS DE CALCUL DES MATRICES DE COOCCURRENCE
# ============================================================
# Identiques à celles du script équivalent sur le sexe (voir ce script pour
# le détail des explications) : cooccurrence brute, indice de Jaccard, Lift,
# et test chi²/Fisher par paire d'objets.

compute_cooccurrence_matrix <- function(df, objet_col = "obj_nom", group_col = "us_nom") {
  df_binary <- df %>%
    distinct(.data[[group_col]], .data[[objet_col]]) %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = .data[[objet_col]], values_from = value, values_fill = 0)
  
  mat <- as.matrix(df_binary[,-1])
  cooc_mat <- t(mat) %*% mat
  diag(cooc_mat) <- 0
  
  return(list(cooc_mat = cooc_mat, binary_mat = mat))
}

compute_jaccard_matrix <- function(df, objet_col = "obj_nom", group_col = "us_nom") {
  df_binary <- df %>%
    distinct(.data[[group_col]], .data[[objet_col]]) %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = .data[[objet_col]], values_from = value, values_fill = 0)
  
  mat <- as.matrix(df_binary[,-1])
  n_objets <- ncol(mat)
  jaccard_mat <- matrix(0, nrow = n_objets, ncol = n_objets)
  rownames(jaccard_mat) <- colnames(jaccard_mat) <- colnames(mat)
  
  for (i in 1:n_objets) {
    for (j in 1:n_objets) {
      if (i != j) {
        intersection <- sum(mat[,i] & mat[,j])
        union <- sum(mat[,i] | mat[,j])
        jaccard_mat[i,j] <- ifelse(union > 0, intersection / union, 0)
      }
    }
  }
  
  return(jaccard_mat)
}

compute_lift_matrix <- function(binary_mat, cooc_mat) {
  n_contextes <- nrow(binary_mat)
  n_objets <- ncol(binary_mat)
  
  freq_objets <- colSums(binary_mat) / n_contextes
  
  lift_mat <- matrix(0, nrow = n_objets, ncol = n_objets)
  rownames(lift_mat) <- colnames(lift_mat) <- colnames(binary_mat)
  
  for (i in 1:n_objets) {
    for (j in 1:n_objets) {
      if (i != j) {
        prob_obs <- cooc_mat[i,j] / n_contextes
        prob_exp <- freq_objets[i] * freq_objets[j]
        lift_mat[i,j] <- ifelse(prob_exp > 0, prob_obs / prob_exp, 0)
      }
    }
  }
  
  return(lift_mat)
}

compute_chisq_matrix <- function(binary_mat) {
  n_objets <- ncol(binary_mat)
  
  chisq_mat <- matrix(NA, nrow = n_objets, ncol = n_objets)
  pvalue_mat <- matrix(NA, nrow = n_objets, ncol = n_objets)
  rownames(chisq_mat) <- colnames(chisq_mat) <- colnames(binary_mat)
  rownames(pvalue_mat) <- colnames(pvalue_mat) <- colnames(binary_mat)
  
  for (i in 1:(n_objets - 1)) {
    for (j in (i + 1):n_objets) {
      
      both <- sum(binary_mat[, i] == 1 & binary_mat[, j] == 1)
      only_i <- sum(binary_mat[, i] == 1 & binary_mat[, j] == 0)
      only_j <- sum(binary_mat[, i] == 0 & binary_mat[, j] == 1)
      neither <- sum(binary_mat[, i] == 0 & binary_mat[, j] == 0)
      
      contingency <- matrix(c(both, only_i, only_j, neither), nrow = 2)
      
      expected <- suppressWarnings(chisq.test(contingency)$expected)
      
      if (any(expected < 5)) {
        test <- fisher.test(contingency)
        chisq_mat[i, j] <- chisq_mat[j, i] <- NA
        pvalue_mat[i, j] <- pvalue_mat[j, i] <- test$p.value
      } else {
        test <- chisq.test(contingency, correct = TRUE)
        chisq_mat[i, j] <- chisq_mat[j, i] <- as.numeric(test$statistic)
        pvalue_mat[i, j] <- pvalue_mat[j, i] <- test$p.value
      }
    }
  }
  
  return(list(chisq = chisq_mat, pvalue = pvalue_mat))
}


# ============================================================
# 3. FONCTIONS DE VISUALISATION
# ============================================================
# Identiques au script équivalent sur le sexe (heatmap, heatmap avec
# significativité, graphe de cooccurrence avec mesures de centralité).

plot_heatmap <- function(cooc_mat, title = "Heatmap des cooccurrences") {
  hc <- hclust(as.dist(max(cooc_mat) - cooc_mat))
  cooc_mat_ordered <- cooc_mat[hc$order, hc$order]
  
  df_plot <- as.data.frame(as.table(cooc_mat_ordered))
  names(df_plot) <- c("Objet1", "Objet2", "Cooccurrence")
  
  ggplot(df_plot, aes(x = Objet1, y = Objet2, fill = Cooccurrence)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9)) +
    labs(title = title, fill = "Cooccurrence")
}

plot_heatmap_significance <- function(cooc_mat, pvalue_mat, alpha = 0.05, title = "Cooccurrences significatives") {
  hc <- hclust(as.dist(max(cooc_mat) - cooc_mat))
  cooc_mat_ordered <- cooc_mat[hc$order, hc$order]
  pvalue_mat_ordered <- pvalue_mat[hc$order, hc$order]
  
  df_plot <- as.data.frame(as.table(cooc_mat_ordered))
  df_pval <- as.data.frame(as.table(pvalue_mat_ordered))
  names(df_plot) <- c("Objet1", "Objet2", "Cooccurrence")
  names(df_pval) <- c("Objet1", "Objet2", "Pvalue")
  
  df_plot <- df_plot %>%
    left_join(df_pval, by = c("Objet1", "Objet2")) %>%
    mutate(Significatif = ifelse(Pvalue < alpha & !is.na(Pvalue), "Oui", "Non"))
  
  ggplot(df_plot, aes(x = Objet1, y = Objet2, fill = Cooccurrence, alpha = Significatif)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    scale_alpha_manual(values = c("Non" = 0.3, "Oui" = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9)) +
    labs(title = title, fill = "Cooccurrence", alpha = paste("p <", alpha))
}

plot_cooccurrence_graph <- function(cooc_mat, lift_mat, pvalue_mat = NULL, min_weight = 2, 
                                    alpha = 0.05, title = "Graphe des cooccurrences") {
  df_edges <- as.data.frame(as.table(lift_mat))
  names(df_edges) <- c("Objet1", "Objet2", "Lift")
  
  df_edges <- df_edges %>% filter(Lift >= min_weight)
  
  if (!is.null(pvalue_mat)) {
    df_pval <- as.data.frame(as.table(pvalue_mat))
    names(df_pval) <- c("Objet1", "Objet2", "Pvalue")
    
    df_edges <- df_edges %>%
      left_join(df_pval, by = c("Objet1", "Objet2")) %>%
      mutate(Significatif = ifelse(Pvalue < alpha & !is.na(Pvalue), "Oui", "Non")) %>%
      filter(Significatif == "Oui")
  }
  
  if (nrow(df_edges) == 0) {
    cat("Aucun lien avec Lift >=", min_weight, "\n")
    return(NULL)
  }
  
  graph <- graph_from_data_frame(df_edges, directed = FALSE)
  V(graph)$degree <- degree(graph)
  
  centralite <- data.frame(
    objet = V(graph)$name,
    degre = degree(graph),
    betweenness = betweenness(graph),
    closeness = closeness(graph)
  ) %>%
    arrange(desc(degre))
  
  cat("\n--- Mesures de centralité du réseau ---\n")
  print(centralite)
  
  set.seed(123)
  
  p <- ggraph(graph, layout = "fr") +
    geom_edge_link(aes(width = Lift, color = if(!is.null(pvalue_mat)) Significatif else NULL), alpha = 0.6) +
    geom_node_point(aes(size = degree), color = "steelblue", alpha = 0.8) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    scale_edge_width(range = c(0.5, 3)) +
    scale_size_continuous(range = c(3, 10)) +
    theme_void() +
    labs(title = title, size = "Degré")
  
  if (!is.null(pvalue_mat)) {
    p <- p + scale_edge_color_manual(values = c("Non" = "gray70", "Oui" = "red3"),
                                     name = paste("p <", alpha))
  }
  
  return(list(plot = p, centralite = centralite))
}


# ============================================================
# 4. FONCTION PRINCIPALE : ANALYSE D'UNE ZONE GÉOGRAPHIQUE
# ============================================================
# Même logique que le script équivalent sur le sexe, mais les sous-ensembles
# sont ici construits par classe_age (defunt_classe_age) plutôt que par genre.
analyser_province <- function(data, province_name, seuil_contextes = 3, seuil_objets = 2, 
                              min_weight = 2, alpha = 0.05) {
  cat("\n========================================\n")
  cat("ANALYSE POUR :", province_name, "\n")
  cat("========================================\n")
  
  df <- data %>%
    select(us_nom, obj_nom, defunt_classe_age, intervalle_personnalise) %>%
    filter(!is.na(us_nom), !is.na(obj_nom), !is.na(defunt_classe_age), !is.na(intervalle_personnalise))
  
  classe_ages <- unique(df$defunt_classe_age)
  intervals <- unique(df$intervalle_personnalise)
  resultats_synthese <- data.frame()
  
  for (classe_age in classe_ages) {
    for (intervalle in intervals) {
      cat("\n### classe_age:", classe_age, "| Intervalle:", intervalle, "###\n")
      
      df_subset <- df %>% filter(defunt_classe_age == classe_age, intervalle_personnalise == intervalle)
      
      n_contextes <- length(unique(df_subset$us_nom))
      n_objets_total <- length(unique(df_subset$obj_nom))
      
      if (n_contextes < seuil_contextes) next
      
      objets_frequents <- df_subset %>%
        group_by(obj_nom) %>%
        summarise(n_contextes_obj = n_distinct(us_nom)) %>%
        filter(n_contextes_obj >= seuil_objets) %>%
        pull(obj_nom)
      
      df_subset_filtre <- df_subset %>% filter(obj_nom %in% objets_frequents)
      if (length(unique(df_subset_filtre$obj_nom)) < 2) next
      
      cooc_result <- compute_cooccurrence_matrix(df_subset_filtre)
      cooc_mat <- cooc_result$cooc_mat
      binary_mat <- cooc_result$binary_mat
      jaccard_mat <- compute_jaccard_matrix(df_subset_filtre)
      lift_mat <- compute_lift_matrix(binary_mat, cooc_mat)
      chisq_result <- compute_chisq_matrix(binary_mat)
      pvalue_mat <- chisq_result$pvalue
      
      cat("\n--- Statistiques descriptives ---\n")
      cat("Nombre de contextes (tombes) analysés :", n_contextes, "\n")
      cat("Nombre d'objets avant filtrage :", n_objets_total, "\n")
      cat("Nombre d'objets après filtrage :", length(objets_frequents), "\n")
      
      freq_objets_table <- df_subset_filtre %>%
        group_by(obj_nom) %>%
        summarise(
          n_tombes = n_distinct(us_nom),
          freq_pct = round(100 * n_distinct(us_nom) / n_contextes, 1)
        ) %>%
        arrange(desc(n_tombes))
      
      cat("\nFréquence des objets dans les tombes :\n")
      print(freq_objets_table)
      
      write.csv(freq_objets_table,
                paste0("resultats/cooccurrence_age/frequences_objets_",
                       gsub(" ", "_", province_name), "_",
                       gsub(" ", "_", classe_age), "_",
                       gsub(" ", "_", intervalle), ".csv"),
                row.names = FALSE)
      
      objets_par_tombe <- df_subset_filtre %>%
        group_by(us_nom) %>%
        summarise(n_objets = n_distinct(obj_nom)) %>%
        pull(n_objets)
      
      cat("\nNombre moyen d'objets par tombe :", round(mean(objets_par_tombe), 2), "\n")
      cat("Min :", min(objets_par_tombe), "| Max :", max(objets_par_tombe), "\n")
      
      signif_pairs <- which(pvalue_mat < alpha, arr.ind = TRUE)
      
      if (!is.null(dim(signif_pairs)) && nrow(signif_pairs) > 0) {
        signif_pairs <- signif_pairs[signif_pairs[,1] < signif_pairs[,2], , drop = FALSE]
        
        if (nrow(signif_pairs) > 0) {
          signif_objects <- data.frame(
            Objet1 = rownames(pvalue_mat)[signif_pairs[,1]],
            Objet2 = colnames(pvalue_mat)[signif_pairs[,2]],
            Cooccurrence = cooc_mat[signif_pairs],
            Jaccard = jaccard_mat[signif_pairs],
            Lift = lift_mat[signif_pairs],
            Pvalue = pvalue_mat[signif_pairs]
          ) %>%
            arrange(Pvalue)
          
          cat("\nPaires significatives (p <", alpha, ") avec indices :\n")
          print(signif_objects)
          
          nom_fichier <- paste0(gsub(" ", "_", province_name), "_", gsub(" ", "_", classe_age), "_", gsub(" ", "_", intervalle))
          write.csv(signif_objects, 
                    paste0("resultats/cooccurrence_age/paires_significatives_", nom_fichier, ".csv"),
                    row.names = FALSE)
        } else {
          cat("\nAucune paire significative pour cet intervalle/classe_age.\n")
        }
      } else {
        cat("\nAucune paire significative pour cet intervalle/classe_age.\n")
      }
      
      print(plot_heatmap(cooc_mat, paste(province_name, "- Cooccurrences\nclasse_age:", classe_age, "| Intervalle:", intervalle)))
      print(plot_heatmap_significance(cooc_mat, pvalue_mat, alpha,
                                      paste(province_name, "- Cooccurrences significatives\nclasse_age:", classe_age, "| Intervalle:", intervalle)))
      print(plot_heatmap(jaccard_mat, paste(province_name, "- Jaccard\nclasse_age:", classe_age, "| Intervalle:", intervalle)))
      print(plot_heatmap(lift_mat, paste(province_name, "- Lift\nclasse_age:", classe_age, "| Intervalle:", intervalle)))
      
      p_graph_result <- plot_cooccurrence_graph(cooc_mat, lift_mat, pvalue_mat, min_weight, alpha,
                                                paste(province_name, "- Graphe des cooccurrences\nclasse_age:", classe_age, "| Intervalle:", intervalle))
      
      if (!is.null(p_graph_result)) {
        print(p_graph_result$plot)
        nom_fichier <- paste0(gsub(" ", "_", province_name), "_", gsub(" ", "_", classe_age), "_", gsub(" ", "_", intervalle))
        write.csv(p_graph_result$centralite, 
                  paste0("resultats/cooccurrence_age/centralite_", nom_fichier, ".csv"),
                  row.names = FALSE)
      }
      
      nom_fichier <- paste0(gsub(" ", "_", province_name), "_", gsub(" ", "_", classe_age), "_", gsub(" ", "_", intervalle))
      write.csv(cooc_mat, paste0("resultats/cooccurrence_age/matrice_cooc_", nom_fichier, ".csv"))
      write.csv(jaccard_mat, paste0("resultats/cooccurrence_age/matrice_jaccard_", nom_fichier, ".csv"))
      write.csv(lift_mat, paste0("resultats/cooccurrence_age/matrice_lift_", nom_fichier, ".csv"))
      write.csv(pvalue_mat, paste0("resultats/cooccurrence_age/matrice_pvalue_", nom_fichier, ".csv"))
      
      n_signif <- if (!is.null(dim(signif_pairs))) nrow(signif_pairs) else 0
      
      resultats_synthese <- rbind(
        resultats_synthese,
        data.frame(
          province = province_name,
          classe_age = classe_age,
          intervalle = intervalle,
          n_contextes = n_contextes,
          n_objets_total = n_objets_total,
          n_objets_filtres = length(objets_frequents),
          n_paires_testees = sum(upper.tri(pvalue_mat, diag = FALSE)),
          n_paires_significatives = n_signif,
          moy_objets_par_tombe = round(mean(objets_par_tombe), 2),
          min_objets_par_tombe = min(objets_par_tombe),
          max_objets_par_tombe = max(objets_par_tombe)
        )
      )
    }
  }
  return(resultats_synthese)
}


# ============================================================
# 5. EXÉCUTION DE L'ANALYSE
# ============================================================

seuil_contextes <- 3
seuil_objets <- 2
min_weight <- 2
alpha <- 0.05

cat("\nANALYSE PRINCIPALE - Gaule du nord\n")
synthese_gaule_nord <- analyser_province(d_intervalle_gaule_nord, 
                                         "Gaule du nord",
                                         seuil_contextes = seuil_contextes,
                                         seuil_objets = seuil_objets,
                                         min_weight = min_weight,
                                         alpha = alpha)

cat("\nANALYSE PRINCIPALE - Gaule du sud\n")
synthese_gaule_sud <- analyser_province(d_intervalle_gaule_sud,
                                        "Gaule du sud",
                                        seuil_contextes = seuil_contextes,
                                        seuil_objets = seuil_objets,
                                        min_weight = min_weight,
                                        alpha = alpha)

synthese_globale <- rbind(synthese_gaule_nord, synthese_gaule_sud)
print(synthese_globale)
write.csv(synthese_globale, "resultats/cooccurrence_age/synthese_globale.csv", row.names = FALSE)