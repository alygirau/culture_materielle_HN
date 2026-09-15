####
# Analyse des cooccurrences par groupe CAH — Gaule du nord
# Protocole : Cooccurrence + Jaccard + Lift + Fisher/Chi² + Réseau
#
# Contexte : "groupe_cah" désigne ici le groupe d'appartenance de chaque tombe
# issu d'une Classification Ascendante Hiérarchique menée en amont (dans un
# autre script), et utilisé comme proxy du statut social. Ce script analyse,
# au sein de chaque groupe CAH, quels marqueurs de statut (amphore, monnaie,
# armement, etc.) ont tendance à apparaître ensemble dans les mêmes tombes.
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(reshape2)


# ============================================================
# 1. CHARGEMENT DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.

# NB : contrairement aux scripts d'analyse de réseau par sexe/âge, ce script
# ne part pas des données générales du corpus (via source()) mais lit
# directement un tableau déjà préparé, contenant pour chaque tombe son groupe
# CAH et des indicateurs binaires de présence/absence de chaque marqueur de
# statut.
df <- read.csv2("data/cah_gaule_nord.csv",
                header = TRUE,
                encoding = "utf8",
                row.names = "ID")

# Liste des marqueurs de statut social étudiés (colonnes binaires 0/1 du
# tableau ci-dessus, une colonne par type d'objet)
vars_binaires <- c("amphore", "monnaie", "lampe", "armement",
                   "parure", "strigile", "instrument.de.toilette",
                   "miroir", "vaisselle.métallique",
                   "vaisselle.en.verre", "matériaux.précieux",
                   "char", "lit.funéraire", "instrument.écriture")

# Dossier de sortie
dir.create("resultats/cooccurrence_cah", showWarnings = FALSE, recursive = TRUE)


# ============================================================
# 2. FONCTIONS DE CALCUL DES INDICES DE COOCCURRENCE
# ============================================================
# Mêmes principes que dans les scripts d'analyse de réseau par sexe/âge (voir
# ces scripts pour le détail des explications), mais réécrits ici de façon
# plus compacte et appliqués directement à une matrice binaire déjà construite
# (pas de pivot_wider nécessaire, la matrice existe déjà sous cette forme).

# --- Cooccurrence brute ---
compute_cooc <- function(mat) {
  cooc <- t(mat) %*% mat
  diag(cooc) <- 0
  return(cooc)
}

# --- Indice de Jaccard ---
compute_jaccard <- function(mat) {
  n_objets <- ncol(mat)
  jaccard <- matrix(0, n_objets, n_objets)
  rownames(jaccard) <- colnames(jaccard) <- colnames(mat)
  
  for (i in 1:n_objets) {
    for (j in 1:n_objets) {
      if (i != j) {
        intersection <- sum(mat[,i] & mat[,j])
        union        <- sum(mat[,i] | mat[,j])
        jaccard[i,j] <- ifelse(union > 0, intersection / union, 0)
      }
    }
  }
  return(jaccard)
}

# --- Lift ---
compute_lift <- function(mat, cooc) {
  n <- nrow(mat)
  freq <- colSums(mat) / n
  n_objets <- ncol(mat)
  
  lift <- matrix(0, n_objets, n_objets)
  rownames(lift) <- colnames(lift) <- colnames(mat)
  
  for (i in 1:n_objets) {
    for (j in 1:n_objets) {
      if (i != j) {
        pxy  <- cooc[i,j] / n
        pxpy <- freq[i] * freq[j]
        lift[i,j] <- ifelse(pxpy > 0, pxy / pxpy, 0)
      }
    }
  }
  return(lift)
}

# --- Test statistique (Fisher ou Chi² selon les effectifs attendus) ---
compute_pvalue <- function(mat) {
  n_objets <- ncol(mat)
  pval <- matrix(1, n_objets, n_objets)
  rownames(pval) <- colnames(pval) <- colnames(mat)
  
  for (i in 1:(n_objets - 1)) {
    for (j in (i + 1):n_objets) {
      a <- sum(mat[,i] & mat[,j])
      b <- sum(mat[,i] & !mat[,j])
      c <- sum(!mat[,i] & mat[,j])
      d <- sum(!mat[,i] & !mat[,j])
      
      tab <- matrix(c(a, b, c, d), nrow = 2)
      expected <- suppressWarnings(chisq.test(tab)$expected)
      
      p <- tryCatch({
        if (any(expected < 5)) {
          fisher.test(tab)$p.value
        } else {
          chisq.test(tab, correct = TRUE)$p.value
        }
      }, error = function(e) 1)
      
      pval[i,j] <- pval[j,i] <- p
    }
  }
  return(pval)
}


# ============================================================
# 3. FONCTIONS DE VISUALISATION
# ============================================================

# --- Heatmap brute, réordonnée par classification hiérarchique ---
plot_heatmap <- function(mat, title = "Heatmap") {
  hc      <- hclust(as.dist(max(mat, na.rm = TRUE) - mat))
  mat_ord <- mat[hc$order, hc$order]
  df_plot <- as.data.frame(as.table(mat_ord))
  names(df_plot) <- c("Objet1", "Objet2", "Valeur")
  
  ggplot(df_plot, aes(x = Objet1, y = Objet2, fill = Valeur)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9)) +
    labs(title = title, fill = "Valeur")
}

# --- Heatmap avec transparence sur les paires non significatives ---
plot_heatmap_signif <- function(cooc_mat, pval_mat, alpha = 0.05,
                                title = "Cooccurrences significatives") {
  hc       <- hclust(as.dist(max(cooc_mat, na.rm = TRUE) - cooc_mat))
  cooc_ord <- cooc_mat[hc$order, hc$order]
  pval_ord <- pval_mat[hc$order, hc$order]
  
  df_cooc <- as.data.frame(as.table(cooc_ord))
  df_pval <- as.data.frame(as.table(pval_ord))
  names(df_cooc) <- c("Objet1", "Objet2", "Cooccurrence")
  names(df_pval) <- c("Objet1", "Objet2", "Pvalue")
  
  df_plot <- df_cooc %>%
    left_join(df_pval, by = c("Objet1", "Objet2")) %>%
    mutate(Significatif = ifelse(!is.na(Pvalue) & Pvalue < alpha, "Oui", "Non"))
  
  ggplot(df_plot, aes(x = Objet1, y = Objet2, fill = Cooccurrence, alpha = Significatif)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    scale_alpha_manual(values = c("Non" = 0.2, "Oui" = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9)) +
    labs(title = title, fill = "Cooccurrence", alpha = paste("p <", alpha))
}

# --- Réseau des cooccurrences significatives ---
# Contrairement aux scripts sexe/âge, la construction des arêtes se fait ici
# par double boucle explicite plutôt que par filtrage d'un tableau complet —
# fonctionnellement équivalent, juste une autre façon d'écrire la même logique.
plot_reseau <- function(cooc_mat, lift_mat, pval_mat,
                        min_cooc = 2, alpha = 0.05, title = "Réseau") {
  edges <- data.frame()
  n     <- ncol(cooc_mat)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (cooc_mat[i,j] >= min_cooc &&
          !is.na(pval_mat[i,j]) &&
          pval_mat[i,j] < alpha) {
        edges <- rbind(edges, data.frame(
          from = rownames(cooc_mat)[i],
          to   = colnames(cooc_mat)[j],
          cooc = cooc_mat[i,j],
          lift = lift_mat[i,j],
          pval = pval_mat[i,j]
        ))
      }
    }
  }
  
  if (nrow(edges) == 0) {
    cat("Aucun lien significatif avec min_cooc =", min_cooc, "\n")
    return(NULL)
  }
  
  g <- graph_from_data_frame(edges, directed = FALSE)
  V(g)$degre       <- degree(g)
  V(g)$betweenness <- betweenness(g)
  V(g)$closeness   <- closeness(g)
  
  centralite <- data.frame(
    objet       = V(g)$name,
    degre       = V(g)$degre,
    betweenness = V(g)$betweenness,
    closeness   = V(g)$closeness
  ) %>% arrange(desc(degre))
  
  cat("\n--- Centralité du réseau ---\n")
  print(centralite)
  
  set.seed(123)
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = lift), alpha = 0.6, color = "darkred") +
    geom_node_point(aes(size = degre), color = "steelblue", alpha = 0.8) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    scale_edge_width(range = c(0.5, 3)) +
    scale_size_continuous(range = c(3, 10)) +
    theme_void() +
    labs(title = title, size = "Degré")
  
  return(list(plot = p, centralite = centralite, edges = edges))
}


# ============================================================
# 4. FONCTION PRINCIPALE : ANALYSE D'UN GROUPE CAH
# ============================================================
analyser_groupe_cah <- function(df, groupe, min_cooc = 2, alpha = 0.05) {
  
  cat("\n============================\n")
  cat("Groupe CAH :", groupe, "\n")
  cat("============================\n")
  
  # --- 4.1 Filtrage sur le groupe et construction de la matrice binaire ---
  df_g <- df %>%
    filter(groupe_cah == groupe) %>%
    select(all_of(vars_binaires))
  
  mat <- as.matrix(df_g)
  mat <- (mat > 0) * 1  # Binarisation stricte (au cas où certaines valeurs seraient > 1)
  
  # Suppression des marqueurs jamais présents dans ce groupe (colonnes
  # constantes à 0), qui n'apporteraient rien à l'analyse et pourraient
  # perturber les calculs de cooccurrence/Jaccard
  cols_actives <- which(colSums(mat) > 0)
  mat <- mat[, cols_actives, drop = FALSE]
  
  n_tombes   <- nrow(mat)
  n_objets   <- ncol(mat)
  moy_objets <- mean(rowSums(mat))
  min_objets <- min(rowSums(mat))
  max_objets <- max(rowSums(mat))
  
  cat("Nombre de tombes    :", n_tombes, "\n")
  cat("Objets actifs       :", n_objets, "\n")
  cat("Objets moy/tombe    :", round(moy_objets, 2), "\n")
  cat("Min/Max par tombe   :", min_objets, "/", max_objets, "\n\n")
  
  if (n_objets < 2) {
    cat("Pas assez de marqueurs actifs pour l'analyse.\n")
    return(NULL)
  }
  
  # --- 4.2 Fréquence individuelle de chaque marqueur dans ce groupe ---
  freq_tab <- data.frame(
    objet    = colnames(mat),
    n_tombes = colSums(mat),
    freq_pct = round(100 * colSums(mat) / n_tombes, 1)
  ) %>% arrange(desc(n_tombes))
  
  cat("--- Fréquence des marqueurs ---\n")
  print(freq_tab)
  
  write.csv(freq_tab,
            paste0("resultats/cooccurrence_cah/frequences_groupe", groupe, ".csv"),
            row.names = FALSE)
  
  # --- 4.3 Calcul des indices de cooccurrence ---
  cooc    <- compute_cooc(mat)
  jaccard <- compute_jaccard(mat)
  lift    <- compute_lift(mat, cooc)
  pval    <- compute_pvalue(mat)
  
  # Seuil minimal de cooccurrence appliqué pour les visualisations/réseau
  cooc_filtre <- cooc
  cooc_filtre[cooc_filtre < min_cooc] <- 0
  
  # --- 4.4 Extraction des paires de marqueurs significatives ---
  idx <- which(pval < alpha, arr.ind = TRUE)
  idx <- idx[idx[,1] < idx[,2], , drop = FALSE]  # Matrice symétrique : ne garder qu'une occurrence par paire
  
  if (nrow(idx) > 0) {
    paires <- data.frame(
      objet1  = rownames(pval)[idx[,1]],
      objet2  = colnames(pval)[idx[,2]],
      cooc    = cooc[idx],
      jaccard = jaccard[idx],
      lift    = lift[idx],
      pval    = pval[idx]
    ) %>%
      filter(cooc >= min_cooc) %>%
      arrange(pval)
    
    cat("\n--- Paires significatives (p <", alpha, ", cooc >=", min_cooc, ") ---\n")
    print(paires)
    
    write.csv(paires,
              paste0("resultats/cooccurrence_cah/paires_significatives_groupe", groupe, ".csv"),
              row.names = FALSE)
  } else {
    cat("\nAucune paire significative.\n")
    paires <- data.frame()
  }
  
  # --- 4.5 Visualisations ---
  print(plot_heatmap(cooc_filtre,
                     paste("Co-occurrence brute — Groupe CAH", groupe)))
  
  print(plot_heatmap_signif(cooc_filtre, pval, alpha,
                            paste("Co-occurrence significative — Groupe CAH", groupe)))
  
  print(plot_heatmap(jaccard,
                     paste("Jaccard — Groupe CAH", groupe)))
  
  print(plot_heatmap(lift,
                     paste("Lift — Groupe CAH", groupe)))
  
  # --- 4.6 Réseau ---
  reseau <- plot_reseau(cooc_filtre, lift, pval, min_cooc, alpha,
                        paste("Réseau significatif — Groupe CAH", groupe))
  
  if (!is.null(reseau)) {
    print(reseau$plot)
    write.csv(reseau$centralite,
              paste0("resultats/cooccurrence_cah/centralite_groupe", groupe, ".csv"),
              row.names = FALSE)
    write.csv(reseau$edges,
              paste0("resultats/cooccurrence_cah/edges_groupe", groupe, ".csv"),
              row.names = FALSE)
  }
  
  # --- 4.7 Sauvegarde des matrices complètes ---
  write.csv(cooc,    paste0("resultats/cooccurrence_cah/matrice_cooc_groupe",    groupe, ".csv"))
  write.csv(jaccard, paste0("resultats/cooccurrence_cah/matrice_jaccard_groupe", groupe, ".csv"))
  write.csv(lift,    paste0("resultats/cooccurrence_cah/matrice_lift_groupe",    groupe, ".csv"))
  write.csv(pval,    paste0("resultats/cooccurrence_cah/matrice_pval_groupe",    groupe, ".csv"))
  
  return(list(
    n_tombes   = n_tombes,
    moy_objets = moy_objets,
    freq       = freq_tab,
    paires     = paires,
    cooc       = cooc,
    jaccard    = jaccard,
    lift       = lift,
    pval       = pval,
    reseau     = reseau
  ))
}


# ============================================================
# 5. EXÉCUTION SUR TOUS LES GROUPES CAH + TABLEAU DE SYNTHÈSE
# ============================================================
groupes    <- sort(unique(df$groupe_cah))
resultats  <- list()
synthese   <- data.frame()

for (g in groupes) {
  res <- analyser_groupe_cah(df, g, min_cooc = 2, alpha = 0.05)
  resultats[[paste0("groupe_", g)]] <- res
  
  if (!is.null(res)) {
    synthese <- rbind(synthese, data.frame(
      groupe              = g,
      n_tombes            = res$n_tombes,
      moy_objets_par_tombe = round(res$moy_objets, 2),
      n_paires_signif     = nrow(res$paires)
    ))
  }
}

cat("\n========== SYNTHÈSE GLOBALE ==========\n")
print(synthese)
write.csv(synthese,
          "resultats/cooccurrence_cah/synthese_globale.csv",
          row.names = FALSE)