####
# Analyse de réseau : cooccurrence d'objets par sexe du défunt et intervalle
# chronologique, à l'échelle des contextes funéraires (unités stratigraphiques)
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(dplyr)    # Manipulation de données
library(tidyr)    # Remise en forme des données (pivot_wider)
library(igraph)   # Construction et mesures de centralité sur le graphe
library(ggraph)   # Visualisation du graphe (extension ggplot2 pour les réseaux)
library(ggplot2)  # Graphiques (heatmaps)


# ============================================================
# 1. CHARGEMENT DES DONNÉES PRÉPARÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.

# Ce script réutilise l'objet d_intervalle créé par le script de préparation
# générale (portée : tout le corpus, sans filtrage géographique préalable —
# voir ce script pour le détail). Le nom de fichier ci-dessous doit
# correspondre exactement à celui du script de préparation présent dans scripts/.
source("scripts/preparation_donnees_generales.R")

# Création des dossiers de sortie (sous-dossiers de resultats/)
dir.create("resultats/cooccurrence_sexe", showWarnings = FALSE, recursive = TRUE)

# --- Préparation des sous-corpus par zone géographique ---
d_intervalle_gaule_nord <- d_intervalle %>% filter(zone_geo == "Gaule du nord")
d_intervalle_gaule_sud <- d_intervalle %>% filter(zone_geo == "Gaule du sud")


# ============================================================
# 2. FONCTIONS DE CALCUL DES MATRICES DE COOCCURRENCE
# ============================================================

# --- 2.1 Matrice de cooccurrence brute ---
# Compte, pour chaque paire d'objets, le nombre de contextes (tombes/US) où
# ils apparaissent ensemble. df_binary est une matrice contexte x objet
# (1 si l'objet est présent dans ce contexte, 0 sinon) ; le produit matriciel
# t(mat) %*% mat donne directement le nombre de coprésences par paire d'objets.
compute_cooccurrence_matrix <- function(df, objet_col = "obj_nom", group_col = "us_nom") {
  df_binary <- df %>%
    distinct(.data[[group_col]], .data[[objet_col]]) %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = .data[[objet_col]], values_from = value, values_fill = 0)
  
  mat <- as.matrix(df_binary[,-1])
  cooc_mat <- t(mat) %*% mat
  diag(cooc_mat) <- 0  # La cooccurrence d'un objet avec lui-même n'a pas de sens ici
  
  return(list(cooc_mat = cooc_mat, binary_mat = mat))
}

# --- 2.2 Indice de Jaccard ---
# Mesure de similarité entre deux objets : taille de l'intersection des
# contextes où ils apparaissent, divisée par la taille de l'union. Contrairement
# à la cooccurrence brute, cet indice est normalisé (entre 0 et 1) et ne
# favorise donc pas artificiellement les objets très fréquents.
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

# --- 2.3 Lift ---
# Mesure à quel point deux objets apparaissent ensemble plus (ou moins)
# souvent que ce que l'on attendrait s'ils étaient indépendants l'un de
# l'autre. Lift > 1 : association positive ; Lift < 1 : association négative
# (les objets s'excluent plutôt qu'ils ne s'associent) ; Lift = 1 : indépendance.
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

# --- 2.4 Test statistique de chaque paire d'objets ---
# Teste, pour chaque paire, si la cooccurrence observée diffère significativement
# de ce qui serait attendu sous indépendance. Le chi² est utilisé par défaut,
# mais le test exact de Fisher lui est automatiquement substitué quand les
# effectifs attendus sont trop faibles (< 5), condition sous laquelle
# l'approximation du chi² n'est plus fiable.
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
        # Effectifs attendus trop faibles : test exact de Fisher (pas de
        # statistique chi² associée, d'où le NA dans chisq_mat)
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

# --- 3.1 Heatmap hiérarchisée ---
# Les objets sont réordonnés par classification hiérarchique (sur la
# dissimilarité 1 - cooccurrence normalisée) afin que les objets qui
# s'associent souvent se retrouvent visuellement proches sur la heatmap.
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

# --- 3.2 Heatmap avec mise en évidence des paires significatives ---
# Même principe que ci-dessus, mais les cellules correspondant à des paires
# non significatives (p >= alpha) sont estompées (transparence réduite),
# pour distinguer visuellement le signal du bruit.
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

# --- 3.3 Graphe de cooccurrence (réseau) ---
# Construit un graphe où chaque nœud est un objet et chaque arête relie deux
# objets dont le Lift dépasse min_weight (et, si pvalue_mat est fourni, dont
# l'association est statistiquement significative). Calcule également des
# mesures de centralité (degré, betweenness, closeness) pour identifier les
# objets les plus "centraux" dans le réseau d'associations.
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
  
  # Mesures de centralité : degré (nombre de connexions directes),
  # betweenness (rôle de "pont" entre groupes d'objets), closeness (proximité
  # moyenne à tous les autres nœuds du réseau)
  centralite <- data.frame(
    objet = V(graph)$name,
    degre = degree(graph),
    betweenness = betweenness(graph),
    closeness = closeness(graph)
  ) %>%
    arrange(desc(degre))
  
  cat("\n--- Mesures de centralité du réseau ---\n")
  print(centralite)
  
  set.seed(123)  # graine fixée pour la reproductibilité de la disposition du graphe (layout = "fr")
  
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
# Pour chaque combinaison (genre du défunt x intervalle chronologique) au sein
# de la zone fournie, cette fonction : filtre les objets trop rares, calcule
# les matrices de cooccurrence/Jaccard/Lift/chi², produit les visualisations
# associées, exporte les résultats en CSV, et retourne un tableau de synthèse
# (une ligne par combinaison genre x intervalle).
analyser_province <- function(data, province_name, seuil_contextes = 3, seuil_objets = 2, 
                              min_weight = 2, alpha = 0.05) {
  cat("\n========================================\n")
  cat("ANALYSE POUR :", province_name, "\n")
  cat("========================================\n")
  
  df <- data %>%
    select(us_nom, obj_nom, defunt_genre, intervalle_personnalise) %>%
    filter(!is.na(us_nom), !is.na(obj_nom), !is.na(defunt_genre), !is.na(intervalle_personnalise))
  
  genres <- unique(df$defunt_genre)
  intervals <- unique(df$intervalle_personnalise)
  resultats_synthese <- data.frame()
  
  for (genre in genres) {
    for (intervalle in intervals) {
      cat("\n### Genre:", genre, "| Intervalle:", intervalle, "###\n")
      
      df_subset <- df %>% filter(defunt_genre == genre, intervalle_personnalise == intervalle)
      
      n_contextes <- length(unique(df_subset$us_nom))
      n_objets_total <- length(unique(df_subset$obj_nom))
      
      # Combinaison genre/intervalle ignorée si trop peu de contextes disponibles
      if (n_contextes < seuil_contextes) next
      
      # Ne conserver que les objets présents dans au moins seuil_objets contextes
      # (les objets trop rares génèrent des matrices creuses peu exploitables
      # et des tests statistiques peu fiables)
      objets_frequents <- df_subset %>%
        group_by(obj_nom) %>%
        summarise(n_contextes_obj = n_distinct(us_nom)) %>%
        filter(n_contextes_obj >= seuil_objets) %>%
        pull(obj_nom)
      
      df_subset_filtre <- df_subset %>% filter(obj_nom %in% objets_frequents)
      if (length(unique(df_subset_filtre$obj_nom)) < 2) next  # Il faut au moins 2 objets pour une cooccurrence
      
      cooc_result <- compute_cooccurrence_matrix(df_subset_filtre)
      cooc_mat <- cooc_result$cooc_mat
      binary_mat <- cooc_result$binary_mat
      jaccard_mat <- compute_jaccard_matrix(df_subset_filtre)
      lift_mat <- compute_lift_matrix(binary_mat, cooc_mat)
      chisq_result <- compute_chisq_matrix(binary_mat)
      pvalue_mat <- chisq_result$pvalue
      
      # --- Statistiques descriptives ---
      cat("\n--- Statistiques descriptives ---\n")
      cat("Nombre de contextes (tombes) analysés :", n_contextes, "\n")
      cat("Nombre d'objets avant filtrage :", n_objets_total, "\n")
      cat("Nombre d'objets après filtrage :", length(objets_frequents), "\n")
      
      # Fréquence de chaque objet parmi les contextes (tombes) de ce sous-ensemble
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
                paste0("resultats/cooccurrence_sexe/frequences_objets_",
                       gsub(" ", "_", province_name), "_",
                       gsub(" ", "_", genre), "_",
                       gsub(" ", "_", intervalle), ".csv"),
                row.names = FALSE)
      
      # Nombre moyen d'objets par tombe (indicateur de la richesse du mobilier)
      objets_par_tombe <- df_subset_filtre %>%
        group_by(us_nom) %>%
        summarise(n_objets = n_distinct(obj_nom)) %>%
        pull(n_objets)
      
      cat("\nNombre moyen d'objets par tombe :", round(mean(objets_par_tombe), 2), "\n")
      cat("Min :", min(objets_par_tombe), "| Max :", max(objets_par_tombe), "\n")
      
      # --- Extraction des paires d'objets statistiquement significatives ---
      signif_pairs <- which(pvalue_mat < alpha, arr.ind = TRUE)
      
      if (!is.null(dim(signif_pairs)) && nrow(signif_pairs) > 0) {
        # La matrice pvalue_mat est symétrique : on ne garde que la moitié
        # supérieure pour éviter de compter chaque paire deux fois
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
          
          nom_fichier <- paste0(gsub(" ", "_", province_name), "_", gsub(" ", "_", genre), "_", gsub(" ", "_", intervalle))
          write.csv(signif_objects, 
                    paste0("resultats/cooccurrence_sexe/paires_significatives_", nom_fichier, ".csv"),
                    row.names = FALSE)
        } else {
          cat("\nAucune paire significative pour cet intervalle/genre.\n")
        }
      } else {
        cat("\nAucune paire significative pour cet intervalle/genre.\n")
      }
      
      # --- Visualisations ---
      print(plot_heatmap(cooc_mat, paste(province_name, "- Cooccurrences\nGenre:", genre, "| Intervalle:", intervalle)))
      print(plot_heatmap_significance(cooc_mat, pvalue_mat, alpha,
                                      paste(province_name, "- Cooccurrences significatives\nGenre:", genre, "| Intervalle:", intervalle)))
      print(plot_heatmap(jaccard_mat, paste(province_name, "- Jaccard\nGenre:", genre, "| Intervalle:", intervalle)))
      print(plot_heatmap(lift_mat, paste(province_name, "- Lift\nGenre:", genre, "| Intervalle:", intervalle)))
      
      p_graph_result <- plot_cooccurrence_graph(cooc_mat, lift_mat, pvalue_mat, min_weight, alpha,
                                                paste(province_name, "- Graphe des cooccurrences\nGenre:", genre, "| Intervalle:", intervalle))
      
      if (!is.null(p_graph_result)) {
        print(p_graph_result$plot)
        nom_fichier <- paste0(gsub(" ", "_", province_name), "_", gsub(" ", "_", genre), "_", gsub(" ", "_", intervalle))
        write.csv(p_graph_result$centralite, 
                  paste0("resultats/cooccurrence_sexe/centralite_", nom_fichier, ".csv"),
                  row.names = FALSE)
      }
      
      # --- Sauvegarde des matrices complètes ---
      nom_fichier <- paste0(gsub(" ", "_", province_name), "_", gsub(" ", "_", genre), "_", gsub(" ", "_", intervalle))
      write.csv(cooc_mat,    paste0("resultats/cooccurrence_sexe/matrice_cooc_",    nom_fichier, ".csv"))
      write.csv(jaccard_mat, paste0("resultats/cooccurrence_sexe/matrice_jaccard_", nom_fichier, ".csv"))
      write.csv(lift_mat,    paste0("resultats/cooccurrence_sexe/matrice_lift_",    nom_fichier, ".csv"))
      write.csv(pvalue_mat,  paste0("resultats/cooccurrence_sexe/matrice_pvalue_",  nom_fichier, ".csv"))
      
      n_signif <- if (!is.null(dim(signif_pairs))) nrow(signif_pairs) else 0
      
      # --- Ajout d'une ligne de synthèse pour cette combinaison genre x intervalle ---
      resultats_synthese <- rbind(
        resultats_synthese,
        data.frame(
          province = province_name,
          genre = genre,
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

# --- Paramètres globaux ---
seuil_contextes <- 3   # Nombre minimum de contextes (tombes) par sous-ensemble genre/intervalle
seuil_objets <- 2       # Nombre minimum de contextes où un objet doit apparaître pour être conservé
min_weight <- 2         # Seuil de Lift minimum pour qu'une arête soit tracée dans le graphe
alpha <- 0.05           # Seuil de significativité statistique

cat("\nANALYSE PRINCIPALE - Gaule du nord\n")
synthese_gaule_nord <- analyser_province(d_intervalle_gaule_nord, 
                                         "Gaule du nord",
                                         seuil_contextes = seuil_contextes,
                                         seuil_objets = seuil_objets,
                                         min_weight = min_weight,
                                         alpha = alpha)

# --- Analyse Gaule du sud (désactivée par défaut dans le script d'origine) ---
# Si vous voulez comparer les deux zones, décommentez ce bloc ainsi que la
# synthèse globale ci-dessous.

# cat("\nANALYSE PRINCIPALE - Gaule du sud\n")
# synthese_gaule_sud <- analyser_province(d_intervalle_gaule_sud, 
#                                         "Gaule du sud",
#                                         seuil_contextes = seuil_contextes,
#                                         seuil_objets = seuil_objets,
#                                         min_weight = min_weight,
#                                         alpha = alpha)
#
# synthese_globale <- rbind(synthese_gaule_nord, synthese_gaule_sud)
# print(synthese_globale)
# write.csv(synthese_globale, "resultats/cooccurrence_sexe/synthese_globale.csv", row.names = FALSE)
