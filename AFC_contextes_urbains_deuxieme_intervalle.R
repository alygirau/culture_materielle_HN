####
# AFC + CAH + kmeans
# Contextes urbains
# 1/100
####

# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(FactoMineR)   # Analyse Factorielle des Correspondances : CA()
library(factoextra)   # Visualisations (fviz_cluster, fviz_ca_row, etc.)
library(dplyr)        # Manipulation de données (mutate, filter, group_by...)
library(ggplot2)      # Graphiques personnalisés
library(tidyr)        # Remise en forme des données (pivot_longer, etc.)
library(cluster)      # Calcul de l'indice de silhouette
library(corrplot)     # Visualisation des matrices de résidus/contributions
library(explor)       # Exploration interactive des résultats d'AFC (optionnel)
library(dendextend)   # Amélioration visuelle des dendrogrammes
library(pheatmap)     # Heatmaps pour les tableaux de profils
library(mclust)       # Calcul de l'ARI (Adjusted Rand Index)
library(lsr)          # Calcul du V de Cramér


# ============================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur la
# racine du dossier téléchargé/décompressé (celui qui contient data/, scripts/
# et resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
d <- read.csv2("data/corpus_urbain.csv", header = TRUE, encoding = "utf8", row.names = "ID")

# --- Création des intervalles chronologiques ---
# Les bornes définissent 3 tranches : [-60;1[, [1;100[, [100;300]
limites_classes <- c(-60, 1, 100, 300)

d_intervalle <- d %>%
  mutate(intervalle_personnalise = cut(phase_date_debut,
                                       breaks = limites_classes,
                                       include.lowest = TRUE,
                                       right = FALSE,
                                       labels = paste(head(limites_classes, -1), "-", tail(limites_classes, -1), sep = "")))

# Vérification du nombre d'intervalles créés et de leurs effectifs
nombre_intervalles <- length(levels(d_intervalle$intervalle_personnalise))
cat("Le nombre d'intervalles créés est :", nombre_intervalles, "\n")
table(d_intervalle$intervalle_personnalise)

# --- Regroupement de sites ---
# Fusion du site "CLCJ" dans "CLF" (même entité archéologique regroupée sous un même code)
d_intervalle <- d_intervalle %>%
  mutate(site_abbreviation = case_when(
    site_abbreviation %in% c(
      "CLCJ"
    ) ~ "CLF",
    TRUE ~ site_abbreviation
  ))

# --- Regroupement des catégories d'artisanat ---
# Toutes les catégories liées à un travail de matière première sont fusionnées en "artisanat"
d_intervalle <- d_intervalle %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "travail du métal", "travail du textile", "travail du verre",
      "travail du cuir", "travail du bois", "travail de la pierre",
      "travail des matières dures animales"
    ) ~ "artisanat",
    TRUE ~ nouvelle_categorie
  ))

# --- Regroupement des catégories de parure ---
# Toutes les sous-catégories de parure/bijouterie sont fusionnées en "parure"
d_intervalle <- d_intervalle %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "parure indéterminée", "parure de cou", "parure de bras",
      "fibule", "parure annulaire bague a intaille",
      "parure annulaire bague et anneau", "pendentif", "perle",
      "accessoire de coiffure"
    ) ~ "parure",
    TRUE ~ nouvelle_categorie
  ))

# --- Filtrage du corpus ---
# On exclut le site funéraire "STGRRFUN" (non pertinent pour cette analyse en
# contexte urbain) et on ne garde que le deuxième intervalle chronologique (1-100)
d_filtered <- d_intervalle %>%
  filter(!site_abbreviation == "STGRRFUN") %>%
  filter(intervalle_personnalise == "1-100")


# ============================================================
# 2. ANALYSE FACTORIELLE DES CORRESPONDANCES (AFC)
# ============================================================

# --- Construction du tableau de contingence sites x catégories d'objets ---
contingence <- table(d_filtered$site_abbreviation, d_filtered$nouvelle_categorie)

# --- Regroupement des catégories rares ---
# Toute catégorie dont l'effectif total est inférieur au seuil est fusionnée dans "autres"
# afin d'éviter que des modalités trop peu représentées ne biaisent l'AFC
seuil_rare <- 30
freq_modalites <- colSums(contingence)
modalites_rares <- names(freq_modalites[freq_modalites < seuil_rare])

d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = ifelse(
    nouvelle_categorie %in% modalites_rares,
    "autres",
    as.character(nouvelle_categorie)
  )) %>%
  mutate(nouvelle_categorie = factor(nouvelle_categorie))

# Reconstruction du tableau de contingence après regroupement des catégories rares
contingence <- table(d_filtered$site_abbreviation, d_filtered$nouvelle_categorie)

# Export du tableau de contingence pour vérification/archivage
write.csv2(contingence,
           "resultats/contingence_urbain_deuxieme_intervalle.csv",
           row.names = TRUE)

# --- Définition des modalités supplémentaires (non actives dans l'AFC) ---
# "modalite_sup" : catégories d'objets mises en supplémentaire (n'influencent pas les axes)
# Ici, contrairement au premier intervalle, aucun site n'est mis en individu
# supplémentaire (pas de row.sup) : tous les sites contribuent activement à l'AFC.
modalite_sup <- c("indéterminé", "huisserie charpente", "divers", "récipient culinaire")
indices_sup <- which(colnames(contingence) %in% modalite_sup)

# --- Calcul de l'AFC ---
# ncp = 10 : on conserve 10 dimensions
# col.sup : colonnes (catégories) projetées en supplémentaire
result_afc <- CA(contingence, ncp = 10, col.sup = indices_sup, graph = FALSE)

summary(result_afc)
plot.CA(result_afc, autoLab = "yes")
#explor(result_afc)  # exploration interactive (décommenter si besoin)

# --- Extraction des coordonnées, cos2 et contributions des LIGNES (sites) ---
coord_lignes <- result_afc$row$coord
cos2_lignes <- result_afc$row$cos2
contrib_lignes <- result_afc$row$contrib

df_lignes <- data.frame(
  Coord_Axe1 = coord_lignes[, 1],
  Coord_Axe2 = coord_lignes[, 2],
  Coord_Axe3 = coord_lignes[, 3],
  Cos2_Axe1 = cos2_lignes[, 1],
  Cos2_Axe2 = cos2_lignes[, 2],
  Cos2_Axe3 = cos2_lignes[, 3],
  Contrib_Axe1 = contrib_lignes[, 1],
  Contrib_Axe2 = contrib_lignes[, 2],
  Contrib_Axe3 = contrib_lignes[, 3]
)

write.csv2(df_lignes,
           "resultats/modalites_lignes_deuxieme_intervalle.csv",
           row.names = TRUE)

# --- Extraction des coordonnées, cos2 et contributions des COLONNES (catégories d'objets) ---
coord_colonnes <- result_afc$col$coord
cos2_colonnes <- result_afc$col$cos2
contrib_colonnes <- result_afc$col$contrib

df_colonnes <- data.frame(
  Coord_Axe1 = coord_colonnes[, 1],
  Coord_Axe2 = coord_colonnes[, 2],
  Coord_Axe3 = coord_colonnes[, 3],
  Cos2_Axe1 = cos2_colonnes[, 1],
  Cos2_Axe2 = cos2_colonnes[, 2],
  Cos2_Axe3 = cos2_colonnes[, 3],
  Contrib_Axe1 = contrib_colonnes[, 1],
  Contrib_Axe2 = contrib_colonnes[, 2],
  Contrib_Axe3 = contrib_colonnes[, 3]
)

write.csv2(df_colonnes,
           "resultats/modalites_colonnes_deuxieme_intervalle.csv",
           row.names = TRUE)


# ============================================================
# 3. TEST DU CHI² ET RÉSIDUS DE PEARSON
# ============================================================

# On exclut les modalités supplémentaires avant de calculer le chi²
# (seules les données actives doivent entrer dans le test statistique).
# Ici "CLF" est également exclu du calcul du chi² (mais reste actif dans l'AFC).
contingence_active <- contingence[
  !rownames(contingence) %in% c("CLF"),
  !colnames(contingence) %in% modalite_sup
]

chisq <- chisq.test(contingence_active)
round(chisq$residuals, 3)

# Réordonnancement des lignes/colonnes par intensité des résidus (valeur absolue décroissante)
# pour une lecture plus claire du corrplot
res <- chisq$residuals
row_order <- order(rowSums(abs(res)), decreasing = TRUE)
col_order <- order(colSums(abs(res)), decreasing = TRUE)
res_ordered <- res[row_order, col_order]

corrplot(res_ordered, is.cor = FALSE, tl.cex = 0.8, order = "original")

# Contribution de chaque cellule au chi² total, exprimée en pourcentage
contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)

write.csv2(contrib,
           "resultats/contrib_pourcentage_deuxieme_intervalle.csv",
           row.names = TRUE)


# ============================================================
# 4. CLASSIFICATION ASCENDANTE HIÉRARCHIQUE (CAH)
# ============================================================

# La CAH est réalisée sur les coordonnées des sites dans les 5 premiers axes de l'AFC
coord_sites <- result_afc$row$coord[, 1:5]
dist_sites <- dist(coord_sites, method = "euclidean")
cah_sites <- hclust(dist_sites, method = "ward.D2")

# --- Choix du nombre optimal de groupes via l'indice de silhouette ---
# On teste toutes les partitions de k = 2 à k = 10 groupes
silhouette_scores <- sapply(2:10, function(k) {
  groupes <- cutree(cah_sites, k = k)
  sil <- silhouette(groupes, dist_sites)
  mean(sil[, 3])
})

df_silhouette <- data.frame(
  Nombre_groupes = 2:10,
  Silhouette = silhouette_scores
)

# Visualisation du score de silhouette selon le nombre de groupes
# (le trait rouge pointillé indique le k qui maximise la silhouette)
ggplot(df_silhouette, aes(x = Nombre_groupes, y = Silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  geom_vline(xintercept = which.max(df_silhouette$Silhouette) + 1,
             linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Score de silhouette selon le nombre de groupes",
       x = "Nombre de groupes",
       y = "Score de silhouette moyen") +
  theme_minimal()

print(df_silhouette)

# Contrairement au premier intervalle (où k était fixé manuellement à 3),
# ici le k optimal est repris directement du maximum de silhouette calculé ci-dessus
k_optimal <- df_silhouette$Nombre_groupes[which.max(df_silhouette$Silhouette)]
print(paste("Nombre optimal de groupes :", k_optimal))

# --- Dendrogramme ---
plot(cah_sites,
     main = paste("CAH - Découpage en", k_optimal, "groupes"),
     xlab = "Sites",
     ylab = "Hauteur",
     hang = -1)
rect.hclust(cah_sites, k = k_optimal, border = "red")

# --- Caractérisation des groupes obtenus ---
groupes_cah <- cutree(cah_sites, k = k_optimal)

# Ajout du groupe CAH à chaque objet du corpus (via son site d'appartenance)
d_filtered <- d_filtered %>%
  mutate(Groupe_CAH = groupes_cah[site_abbreviation])

# Profil de chaque groupe selon les catégories d'objets (en %)
tab_groupes_categories <- table(d_filtered$Groupe_CAH, d_filtered$nouvelle_categorie)
tab_pct <- prop.table(tab_groupes_categories, margin = 1) * 100
round(tab_pct, 1)

pheatmap(tab_pct,
         main = "Profil des groupes (%)",
         display_numbers = TRUE,
         number_format = "%.1f",
         cluster_rows = FALSE,
         cluster_cols = FALSE)

# Profil de chaque groupe selon le quartier d'origine (en %)
tab_quartier_typo <- table(d_filtered$Groupe_CAH, d_filtered$quartier_nom)
tab_typo_pct <- prop.table(tab_quartier_typo, margin = 1) * 100
round(tab_typo_pct, 1)

pheatmap(tab_typo_pct,
         main = "Profil des groupes par typo_ER (%)",
         display_numbers = TRUE,
         number_format = "%.1f")

# Test du chi² pour vérifier l'association groupe x catégorie d'objet
chi_categories <- chisq.test(tab_groupes_categories)
print(chi_categories)
residus_categorie <- chi_categories$residuals
round(residus_categorie, 2)
corrplot(residus_categorie, is.cor = FALSE)

# Projection des groupes CAH sur le plan factoriel de l'AFC
sites_actifs <- rownames(result_afc$row$coord)
couleurs_groupes <- as.factor(groupes_cah[sites_actifs])

fviz_ca_row(result_afc,
            col.row = couleurs_groupes,
            repel = TRUE) + labs(color = "Groupes")

# --- Synthèse statistique par groupe ---
table(groupes_cah)

d_filtered %>%
  group_by(Groupe_CAH) %>%
  summarise(
    Nb_sites = n_distinct(site_abbreviation),
    Nb_objets = n(),
    Nb_categories = n_distinct(nouvelle_categorie)
  )


# ============================================================
# 5. K-MEANS (comparaison avec la CAH)
# ============================================================

set.seed(123)  # graine fixée pour la reproductibilité du tirage aléatoire initial
kmeans_result <- kmeans(coord_sites, centers = k_optimal, nstart = 25)

# Visualisation des clusters k-means sur les 2 premiers axes
fviz_cluster(kmeans_result, data = coord_sites, repel = TRUE) +
  labs(color = "Groupe", fill = "Groupe", shape = "Groupe")

# Récupération des affectations de groupe (k-means et CAH)
groupes_kmeans <- kmeans_result$cluster
groupes_cah <- cutree(cah_sites, k = k_optimal)

# Tableau comparatif site par site entre les deux méthodes de classification
data.frame(
  Site = names(groupes_cah),
  Groupe_CAH = groupes_cah,
  Groupe_Kmeans = groupes_kmeans[names(groupes_cah)]
) %>% arrange(Groupe_CAH)

# Table de contingence croisant les deux partitions
table_comparaison <- table(CAH = groupes_cah, Kmeans = groupes_kmeans)
print(table_comparaison)

# ARI (Adjusted Rand Index) : mesure l'accord entre les deux partitions
# (1 = accord parfait, 0 = accord équivalent au hasard)
ari_cah_kmeans <- adjustedRandIndex(groupes_cah, groupes_kmeans)
print(paste("ARI (CAH vs k-means) :", round(ari_cah_kmeans, 3)))


# ============================================================
# 6. CAH SANS LE(S) SITE(S) ISOLÉ(S) — ANALYSE DE SENSIBILITÉ
# ============================================================
# Objectif : vérifier si un ou plusieurs sites atypiques (formant un groupe
# isolé de faible effectif) influencent excessivement la structure globale.
# On les retire et on relance la classification sur le reste du corpus.

cat("\nEffectifs par groupe CAH :\n")
print(table(groupes_cah))

# Numéro du groupe isolé à mettre de côté (à adapter selon les effectifs observés)
groupe_isole <- 2

sites_groupe_isole <- names(groupes_cah[groupes_cah == groupe_isole])
cat("Sites mis de côté :", paste(sites_groupe_isole, collapse = ", "), "\n")

# --- Nouvelle CAH sur les sites restants ---
sites_restants <- names(groupes_cah[groupes_cah != groupe_isole])
coord_sites_restants <- coord_sites[sites_restants, ]
cat("Nombre de sites restants :", nrow(coord_sites_restants), "\n")

dist_sites_restants <- dist(coord_sites_restants, method = "euclidean")
cah_restants <- hclust(dist_sites_restants, method = "ward.D2")

# Indice de silhouette pour k = 2 à 10, avec protection contre les cas dégénérés
# (utile quand peu de sites restent après la mise à l'écart des sites isolés) :
# - si une partition ne produit qu'un seul groupe, ou si silhouette() renvoie un
#   résultat non exploitable, on retourne NA plutôt que de faire planter le script
silhouette_scores_restants <- sapply(2:min(10, nrow(coord_sites_restants) - 1), function(k) {
  groupes <- cutree(cah_restants, k = k)
  if (length(unique(groupes)) < 2) return(NA)
  sil <- silhouette(groupes, dist_sites_restants)
  if (is.null(dim(sil))) return(NA)
  mean(sil[, 3])
})

df_silhouette_restants <- data.frame(
  Nombre_groupes = 2:min(10, nrow(coord_sites_restants) - 1),
  Silhouette = silhouette_scores_restants
)

ggplot(df_silhouette_restants, aes(x = Nombre_groupes, y = Silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  geom_vline(xintercept = which.max(df_silhouette_restants$Silhouette) + 1,
             linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Score de silhouette (sans sites isolés)",
       x = "Nombre de groupes",
       y = "Score de silhouette moyen") +
  theme_minimal()

# Ici encore, k est repris automatiquement du maximum de silhouette
k_optimal_restants <- df_silhouette_restants$Nombre_groupes[which.max(df_silhouette_restants$Silhouette)]
print(paste("Nouveau k optimal :", k_optimal_restants))

# Dendrogramme de la seconde CAH (sans les sites isolés)
plot(cah_restants,
     main = paste("CAH v2 (sans sites isolés) - Découpage en", k_optimal_restants, "groupes"),
     xlab = "Sites", ylab = "Hauteur", hang = -1)
rect.hclust(cah_restants, k = k_optimal_restants, border = "red")

# Nouvelle partition
groupes_cah_restants <- cutree(cah_restants, k = k_optimal_restants)

# Mise à jour du corpus : les sites isolés sont étiquetés "0", les autres reprennent
# leur nouveau groupe issu de la CAH v2
d_filtered <- d_filtered %>%
  mutate(Groupe_CAH_v2 = case_when(
    site_abbreviation %in% sites_groupe_isole ~ 0,
    TRUE ~ groupes_cah_restants[site_abbreviation]
  ))

# --- Caractérisation des nouveaux groupes (sites isolés exclus) ---
d_restants <- d_filtered %>% filter(!site_abbreviation %in% sites_groupe_isole)

tab_groupes_categories_v2 <- table(d_restants$Groupe_CAH_v2, d_restants$nouvelle_categorie)
tab_pct_v2 <- prop.table(tab_groupes_categories_v2, margin = 1) * 100
round(tab_pct_v2, 1)

pheatmap(tab_pct_v2,
         main = "Profil des groupes v2 (%)",
         display_numbers = TRUE,
         number_format = "%.1f",
         cluster_rows = FALSE,
         cluster_cols = FALSE)

tab_quartier_typo_v2 <- table(d_restants$Groupe_CAH_v2, d_restants$quartier_nom)
tab_typo_pct_v2 <- prop.table(tab_quartier_typo_v2, margin = 1) * 100
round(tab_typo_pct_v2, 1)

pheatmap(tab_typo_pct_v2,
         main = "Profil des groupes v2 par quartier (%)",
         display_numbers = TRUE,
         number_format = "%.1f")

chi_categories_v2 <- chisq.test(tab_groupes_categories_v2)
print(chi_categories_v2)
residus_categorie_v2 <- chi_categories_v2$residuals
round(residus_categorie_v2, 2)
corrplot(residus_categorie_v2, is.cor = FALSE)

# Synthèse statistique par groupe v2
table(groupes_cah_restants)

d_restants %>%
  group_by(Groupe_CAH_v2) %>%
  summarise(
    Nb_sites = n_distinct(site_abbreviation),
    Nb_objets = n(),
    Nb_categories = n_distinct(nouvelle_categorie)
  )

# Projection sur l'AFC en mettant en évidence les sites isolés
couleurs_v2 <- as.factor(ifelse(
  sites_actifs %in% sites_groupe_isole,
  "isolé",
  as.character(groupes_cah_restants[sites_actifs])
))

fviz_ca_row(result_afc,
            col.row = couleurs_v2,
            repel = TRUE) +
  labs(color = "Groupes v2", title = "AFC - Groupes CAH v2 (sites isolés en évidence)")

# --- K-means sur les sites restants (comparaison avec la CAH v2) ---
set.seed(123)
kmeans_result_v2 <- kmeans(coord_sites_restants, centers = k_optimal_restants, nstart = 25)

fviz_cluster(kmeans_result_v2, data = coord_sites_restants, repel = TRUE) +
  labs(color = "Groupe", fill = "Groupe", shape = "Groupe",
       title = "K-means v2 (sans sites isolés)")

groupes_kmeans_v2 <- kmeans_result_v2$cluster

# Tableau comparatif CAH v2 / k-means v2
data.frame(
  Site = names(groupes_cah_restants),
  Groupe_CAH_v2 = groupes_cah_restants,
  Groupe_Kmeans_v2 = groupes_kmeans_v2[names(groupes_cah_restants)]
) %>% arrange(Groupe_CAH_v2)

table_comparaison_v2 <- table(CAH_v2 = groupes_cah_restants, Kmeans_v2 = groupes_kmeans_v2)
print(table_comparaison_v2)

# ARI v2 : accord entre CAH et k-means sur le corpus sans sites isolés
ari_cah_kmeans_v2 <- adjustedRandIndex(groupes_cah_restants, groupes_kmeans_v2)
print(paste("ARI v2 (CAH vs k-means, sans isolés) :", round(ari_cah_kmeans_v2, 3)))