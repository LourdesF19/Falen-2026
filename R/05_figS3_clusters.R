# =============================================================================
# 05_figS3_clusters.R — Supplementary Figure 3 (cluster validity diagnostics)
#
# Three diagnostics applied to the NMDS site scores of the 13 schemes meeting
# the documentation threshold:
#   (a) Average silhouette width across k = 2 to 5 (Rousseeuw 1987)
#   (b) Gap statistic (Tibshirani, Walther & Hastie 2001), Tibshirani 1-SE
#       rule for selecting optimal k
#   (c) Within-cluster sum of squares (elbow plot)
#
# These diagnostics address Reviewer 5 comments R5.7 and R5.8 and motivate
# the continuous-gradient interpretation adopted in the main manuscript.
# =============================================================================

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_load_and_score.R"))

section_header("05 — Supplementary Figure 3: cluster validity diagnostics")

set.seed(42)

# ---- Recompute NMDS for N = 13 to get site scores --------------------------
M13 <- score_matrix[n13_schemes, , drop = FALSE]
nmds_n13 <- metaMDS(M13, distance = "bray", k = 2, trymax = 200,
                    trace = FALSE, autotransform = FALSE)
site_scores <- scores(nmds_n13, display = "sites")

# ---- (a) Silhouette width across k -----------------------------------------
sil_results <- data.frame(k = 2:5, silhouette = NA_real_)
for (k in 2:5) {
  km <- kmeans(site_scores, centers = k, nstart = 50, iter.max = 100)
  sil <- silhouette(km$cluster, dist(site_scores))
  sil_results$silhouette[sil_results$k == k] <- mean(sil[, "sil_width"])
}
cat("Average silhouette width:\n")
print(sil_results, row.names = FALSE)
write.csv(sil_results,
          file.path(OUTPUT_DIR, "table_silhouette_k2to5.csv"),
          row.names = FALSE)

panel_a <- ggplot(sil_results, aes(x = k, y = silhouette)) +
  geom_hline(yintercept = 0.5, linetype = "dotted",
             colour = PALETTE$dark, linewidth = 0.3) +
  geom_hline(yintercept = 0.7, linetype = "dotted",
             colour = PALETTE$dark, linewidth = 0.3) +
  geom_line(colour = PALETTE$governance, linewidth = 0.7) +
  geom_point(size = 3, colour = PALETTE$governance) +
  geom_point(data = sil_results[sil_results$k == 3, ],
             aes(x = k, y = silhouette),
             size = 5, shape = 21,
             colour = PALETTE$highlight, fill = NA, stroke = 1.2) +
  annotate("text", x = 5, y = 0.5, label = "Reasonable",
           hjust = 1, vjust = -0.3, size = 2.5, colour = PALETTE$dark) +
  annotate("text", x = 5, y = 0.7, label = "Strong",
           hjust = 1, vjust = -0.3, size = 2.5, colour = PALETTE$dark) +
  scale_x_continuous(breaks = 2:5) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Number of clusters (k)", y = "Average silhouette width",
       title = "(a) Silhouette") +
  theme_manuscript(base_size = 9)

# ---- (b) Gap statistic -----------------------------------------------------
gap <- clusGap(site_scores, FUN = kmeans, K.max = 5, B = 100,
               nstart = 50, iter.max = 100)
gap_df <- as.data.frame(gap$Tab)
gap_df$k <- seq_len(nrow(gap_df))
optimal_k <- maxSE(gap_df$gap, gap_df$SE.sim, method = "firstSEmax")
cat("\nGap statistic:\n")
print(gap_df[, c("k", "gap", "SE.sim")], row.names = FALSE)
cat("Optimal k (Tibshirani 1-SE rule):", optimal_k, "\n")
write.csv(gap_df, file.path(OUTPUT_DIR, "table_gap_statistic.csv"),
          row.names = FALSE)

panel_b <- ggplot(gap_df, aes(x = k, y = gap)) +
  geom_errorbar(aes(ymin = gap - SE.sim, ymax = gap + SE.sim),
                width = 0.2, colour = PALETTE$dark, linewidth = 0.4) +
  geom_line(colour = PALETTE$integrity, linewidth = 0.7) +
  geom_point(size = 3, colour = PALETTE$integrity) +
  geom_point(data = gap_df[gap_df$k == optimal_k, ],
             aes(x = k, y = gap),
             size = 5, shape = 21,
             colour = PALETTE$highlight, fill = NA, stroke = 1.2) +
  scale_x_continuous(breaks = seq_len(nrow(gap_df))) +
  labs(x = "Number of clusters (k)", y = "Gap statistic",
       title = "(b) Gap statistic") +
  theme_manuscript(base_size = 9)

# ---- (c) Within-cluster sum of squares (elbow) -----------------------------
wss_results <- data.frame(k = 1:5, WSS = NA_real_)
for (k in 1:5) {
  if (k == 1) {
    wss_results$WSS[wss_results$k == 1] <- sum(scale(site_scores,
                                                     scale = FALSE)^2)
  } else {
    km <- kmeans(site_scores, centers = k, nstart = 50, iter.max = 100)
    wss_results$WSS[wss_results$k == k] <- km$tot.withinss
  }
}
write.csv(wss_results, file.path(OUTPUT_DIR, "table_wss_elbow.csv"),
          row.names = FALSE)

panel_c <- ggplot(wss_results, aes(x = k, y = WSS)) +
  geom_line(colour = PALETTE$monitoring, linewidth = 0.7) +
  geom_point(size = 3, colour = PALETTE$monitoring) +
  scale_x_continuous(breaks = 1:5) +
  labs(x = "Number of clusters (k)", y = "Total within-cluster SS",
       title = "(c) Elbow") +
  theme_manuscript(base_size = 9)

# ---- Combine and save ------------------------------------------------------
figS3 <- panel_a + panel_b + panel_c + plot_layout(ncol = 3)

save_figure(figS3, "SupplFigure3_cluster_diagnostics.png",
            width_mm = 290, height_mm = 105)
save_figure(figS3, "SupplFigure3_cluster_diagnostics.pdf",
            width_mm = 290, height_mm = 105)

message("05_figS3_clusters.R complete.")
