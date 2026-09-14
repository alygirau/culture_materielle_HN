####
# AFC + CAH + kmeans
# Zone géographique : moyenne vallée du Rhône
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

# Palette de couleurs personnalisée pour les typologies architecturales
# (réutilisée dans les visualisations en fin de script)
palette_couleurs <- c(
  "ER2" = "#d9dbbc",
  "ER3" = "#f56960",
  "relai_routier" = "#c46d5e")


# ============================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================

# IMPORTANT : avant d'exécuter ce script, définir le dossier de travail sur le
# sous-dossier de ce contexte (celui qui contient data/, scripts/ et
# resultats/). Dans RStudio : menu Session > Set Working Directory >
# Choose Directory... puis sélectionner ce dossier. Voir le README pour le détail.
d <- read.csv2("data/corpus_rural_v2.csv", header = TRUE, encoding = "utf8", row.names = "ID")

# --- Filtrage des sites ---
# Seuls les sites de la zone géographique étudiée (moyenne vallée du Rhône) sont conservés
sites_cibles <- c("PAN","SLA","FAGP","PBL","GZGS","QCR","VMLB","RVS","ABA","BEYGT","CRB","MOM","VAM")
d_filtered <- d %>% filter(site_abbreviation %in% sites_cibles)

# --- Regroupement des catégories d'artisanat ---
# Toutes les catégories liées à un travail de matière première sont fusionnées en "artisanat"
d_filtered <- d_filtered %>%
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
d_filtered <- d_filtered %>%
  mutate(nouvelle_categorie = case_when(
    nouvelle_categorie %in% c(
      "parure indéterminée", "parure de cou", "parure de bras",
      "fibule", "parure annulaire bague a intaille",
      "parure annulaire bague et anneau", "pendentif", "perle",
      "accessoire de coiffure"
    ) ~ "parure",
    TRUE ~ nouvelle_categorie
  ))

d_filtered$nouvelle_categorie <- factor(d_filtered$nouvelle_categorie)


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

# Export du tableau de contingence pour vérification/archivage (désactivé par défaut)
# write.csv2(contingence,
#            "resultats/contingence_ER_rhone.csv",
#            row.names = TRUE)

# --- Définition des modalités et individus supplémentaires (non actifs dans l'AFC) ---
# "VAM" : site mis en individu supplémentaire (projeté sur les axes sans contribuer
# à leur calcul) — analysé plus en détail comme cas particulier en fin de script
# "modalite_sup" : catégories d'objets mises en supplémentaire
row_sup_indices <- which(rownames(contingence) == "VAM")
modalite_sup <- c("indéterminé", "huisserie charpente", "divers")
indices_sup <- which(colnames(contingence) %in% modalite_sup)

# --- Calcul de l'AFC ---
# ncp = 10 : on conserve 10 dimensions
result_afc <- CA(contingence, ncp = 10, row.sup = row_sup_indices, col.sup = indices_sup, graph = FALSE)

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

# Export désactivé par défaut (décommenter les 3 lignes pour l'activer)
# write.csv2(df_lignes,
#            "resultats/modalites_lignes_ER_rhone.csv",
#            row.names = TRUE)

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

# Export désactivé par défaut (décommenter les 3 lignes pour l'activer)
# write.csv2(df_colonnes,
#            "resultats/modalites_colonnes_ER_rhone.csv",
#            row.names = TRUE)


# ============================================================
# 3. TEST DU CHI² ET RÉSIDUS DE PEARSON
# ============================================================

# On exclut les modalités et individus supplémentaires avant de calculer le chi²
# (seules les données actives doivent entrer dans le test statistique)
contingence_active <- contingence[
  !rownames(contingence) %in% c("VAM"),
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
           "resultats/contrib_pourcentage_ER_rhone.csv",
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

# k est repris automatiquement du maximum de silhouette
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

# Profil de chaque groupe selon la typologie architecturale (typo_ER, en %)
tab_groupes_typo <- table(d_filtered$Groupe_CAH, d_filtered$typo_ER)
tab_typo_pct <- prop.table(tab_groupes_typo, margin = 1) * 100
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

# Récupération des affectations de groupe (k-means et CAH)
groupes_kmeans <- kmeans_result$cluster
groupes_cah <- cutree(cah_sites, k = k_optimal)

# Visualisation des clusters k-means sur les 2 premiers axes
fviz_cluster(kmeans_result, data = coord_sites, repel = TRUE) +
  labs(color = "Groupe", fill = "Groupe", shape = "Groupe")

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
# 6. COMPARAISON CLASSIFICATION MOBILIER VS TYPOLOGIE ARCHITECTURALE
# ============================================================
# Objectif : vérifier si les groupes obtenus à partir du mobilier (CAH) recoupent
# la typologie architecturale des sites (typo_ER), à l'aide du V de Cramér
# (mesure d'association entre deux variables catégorielles, indépendante des effectifs)

typo_archi <- d_filtered %>%
  distinct(site_abbreviation, typo_ER) %>%
  arrange(site_abbreviation)

# Réordonnancement de la typologie architecturale dans le même ordre que les groupes CAH
sites_ordonnes <- names(groupes_cah)
typo_archi_ordonnee <- typo_archi$typo_ER[match(sites_ordonnes, typo_archi$site_abbreviation)]

contingence_typo <- table(Groupe_CAH = groupes_cah, Typo_ER = typo_archi_ordonnee)

# V de Cramér : 0 = aucune association, 1 = association parfaite
v_cramer <- cramersV(contingence_typo)
print(paste("V de Cramér :", round(v_cramer, 3)))

# Test du chi² correspondant (désactivé par défaut, décommenter si besoin de la p-value)
# chi_test <- chisq.test(contingence_typo)
# print(paste("Chi² =", round(chi_test$statistic, 2),
#             ", p-value =", format.pval(chi_test$p.value, digits = 3)))


# ============================================================
# 7. ANALYSE DU SITE SUPPLÉMENTAIRE VAM
# ============================================================
# VAM a été mis en individu supplémentaire dans l'AFC (section 2) : il ne
# participe pas au calcul des axes, mais on peut tout de même évaluer sa
# position par rapport aux groupes actifs pour discuter de son statut
# (site atypique, isolé, ou au contraire proche d'un groupe existant).

# Coordonnées de VAM dans l'espace factoriel (5 premiers axes)
coord_vam <- result_afc$row.sup$coord[, 1:5]

# Centroïdes des groupes CAH, calculés uniquement sur les sites actifs
centroides <- aggregate(coord_sites,
                        by = list(Groupe = groupes_cah),
                        FUN = mean)

# Distance euclidienne de VAM à chaque centroïde de groupe
dist_vam_centroides <- sapply(1:k_optimal, function(i) {
  dist(rbind(coord_vam, centroides[i, -1]))
})
names(dist_vam_centroides) <- paste("Groupe", 1:k_optimal)
print("Distances de VAM aux centroïdes :")
print(round(dist_vam_centroides, 2))

# Distance moyenne intra-groupe (pour chaque groupe), utilisée comme référence
# de comparaison : permet de juger si VAM est "proche" ou "loin" à l'échelle
# de la dispersion habituelle des sites au sein d'un même groupe
distances_intra <- lapply(1:k_optimal, function(g) {
  sites_groupe <- names(groupes_cah)[groupes_cah == g]
  if (length(sites_groupe) > 1) {
    coords_groupe <- coord_sites[sites_groupe, ]
    dist_matrice <- as.matrix(dist(coords_groupe))
    mean(dist_matrice[upper.tri(dist_matrice)])
  } else {
    NA
  }
})
names(distances_intra) <- paste("Groupe", 1:k_optimal)
print("Distance moyenne intra-groupe :")
print(round(unlist(distances_intra), 2))

# Ratio distance VAM / dispersion intra-groupe : un ratio proche de 1 (ou inférieur)
# suggère que VAM pourrait raisonnablement appartenir à ce groupe ; un ratio
# nettement supérieur à 1 confirme son caractère atypique vis-à-vis de ce groupe
ratios <- sapply(1:k_optimal, function(i) {
  if (!is.na(distances_intra[[i]])) {
    round(dist_vam_centroides[i] / distances_intra[[i]], 2)
  } else {
    NA
  }
})
names(ratios) <- paste("Groupe", 1:k_optimal)
print("Ratios distance VAM / dispersion intra-groupe :")
print(ratios)


# ============================================================
# 8. VISUALISATION DE LA POSITION DE VAM
# ============================================================
# Représentation graphique combinant : les sites actifs colorés par groupe CAH,
# les centroïdes de chaque groupe (croix noires), VAM (en rouge), et un segment
# pointillé reliant VAM à son groupe le plus proche.

# Coordonnées des sites actifs sur les 2 premiers axes
coords_actifs <- result_afc$row$coord[, 1:2]

# Construction du tableau de base pour le graphique (sites actifs)
df_plot <- data.frame(
  Site  = rownames(coords_actifs),
  Axe1  = coords_actifs[, 1],
  Axe2  = coords_actifs[, 2],
  Groupe = NA
)

# Attribution du groupe CAH à chaque site actif
df_plot$Groupe[match(names(groupes_cah), df_plot$Site)] <- paste("Groupe", groupes_cah)

# Ajout de VAM comme point distinct (étiqueté "VAM (outlier)")
df_vam <- data.frame(
  Site   = "VAM",
  Axe1   = result_afc$row.sup$coord[1, 1],
  Axe2   = result_afc$row.sup$coord[1, 2],
  Groupe = "VAM (outlier)"
)

df_plot <- rbind(df_plot, df_vam)

# Calcul des coordonnées des centroïdes (pour affichage sur le graphique)
centroides_coords <- aggregate(result_afc$row$coord[names(groupes_cah), 1:2],
                               by = list(Groupe = groupes_cah),
                               FUN = mean)
df_centroides <- data.frame(
  Groupe = paste("Groupe", centroides_coords$Groupe),
  Axe1   = centroides_coords[, 2],
  Axe2   = centroides_coords[, 3]
)

# Identification du groupe le plus proche de VAM (d'après les distances calculées en section 7)
groupe_proche <- which.min(dist_vam_centroides)

# Segment reliant VAM à son centroïde le plus proche
df_segment <- data.frame(
  x    = df_plot$Axe1[df_plot$Site == "VAM"],
  y    = df_plot$Axe2[df_plot$Site == "VAM"],
  xend = df_centroides$Axe1[groupe_proche],
  yend = df_centroides$Axe2[groupe_proche]
)

ggplot() +
  geom_point(data = df_plot,
             aes(x = Axe1, y = Axe2, color = Groupe, shape = Groupe),
             size = 4) +
  geom_text(data = df_plot,
            aes(x = Axe1, y = Axe2, label = Site),
            vjust = -1, size = 3) +
  geom_point(data = df_centroides,
             aes(x = Axe1, y = Axe2),
             size = 6, shape = 3, color = "black", stroke = 2) +
  geom_segment(data = df_segment,
               aes(x = x, y = y, xend = xend, yend = yend),
               linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_color_manual(values = c(
    "Groupe 1"     = "#d9dbbc",
    "Groupe 2"     = "#f56960",
    "Groupe 3"     = "#c46d5e",
    "Groupe 4"     = "#7a9e9f",
    "VAM (outlier)" = "red"
  )) +
  labs(title = "Position de VAM par rapport aux groupes CAH",
       subtitle = paste("Distance VAM → Groupe", groupe_proche, "=",
                        round(dist_vam_centroides[groupe_proche], 2)),
       x = "Axe 1",
       y = "Axe 2") +
  theme_minimal() +
  theme(legend.position = "bottom")
