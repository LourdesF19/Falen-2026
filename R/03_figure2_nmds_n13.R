# =============================================================================
# 03_figure2_nmds_n13.R — Figure 2 of the manuscript (primary NMDS, N = 13)
#
# Non-metric multidimensional scaling on the 13 schemes meeting the
# documentation threshold, using Bray-Curtis dissimilarities. envfit projects
# each of the 15 evaluation criteria onto the ordination, with 9,999
# permutations and Benjamini-Hochberg FDR correction.
#
# Outputs:
#   - Figure2_NMDS_N13.png / .pdf
#   - table_envfit_N13.csv (reproduces left half of Supplementary Table 4)
# =============================================================================

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_load_and_score.R"))

section_header("03 — Figure 2: NMDS ordination (N = 13)")

set.seed(42)

# ---- Subset to N=13 schemes -------------------------------------------------
M13 <- score_matrix[n13_schemes, , drop = FALSE]

# ---- NMDS via vegan ---------------------------------------------------------
nmds_n13 <- metaMDS(
  comm     = M13,
  distance = "bray",
  k        = 2,
  trymax   = 200,
  trace    = FALSE,
  autotransform = FALSE
)
cat("NMDS stress (N = 13):", round(nmds_n13$stress, 3), "\n\n")

site_scores_n13 <- as.data.frame(scores(nmds_n13, display = "sites"))
site_scores_n13$Scheme <- rownames(site_scores_n13)

# ---- envfit on the 15 criteria ---------------------------------------------
env_n13 <- envfit(nmds_n13, M13, permutations = 9999)

vectors_n13 <- as.data.frame(scores(env_n13, display = "vectors"))
vectors_n13$Criterion <- rownames(vectors_n13)
vectors_n13$r2        <- env_n13$vectors$r
vectors_n13$pval      <- env_n13$vectors$pvals

# Benjamini-Hochberg FDR correction (Benjamini & Hochberg 1995)
vectors_n13$p_adj <- p.adjust(vectors_n13$pval, method = "BH")
vectors_n13$Significant <- vectors_n13$p_adj < 0.05

# Print and save
vectors_n13 <- vectors_n13[order(-vectors_n13$r2), ]
print(vectors_n13[, c("Criterion", "r2", "pval", "p_adj", "Significant")],
      row.names = FALSE)

write.csv(vectors_n13, file.path(OUTPUT_DIR, "table_envfit_N13.csv"),
          row.names = FALSE)

# ---- Arrow scaling for plotting --------------------------------------------
# Scale arrows by sqrt(r²) so that significant criteria visually dominate
display_scale <- 2.0
arrow_scale   <- 1.3 * display_scale
vectors_n13$NMDS1_scaled <- vectors_n13$NMDS1 * sqrt(vectors_n13$r2) * arrow_scale
vectors_n13$NMDS2_scaled <- vectors_n13$NMDS2 * sqrt(vectors_n13$r2) * arrow_scale

# Replace internal labels with the canonical short labels
vectors_n13$Label <- unname(CRITERION_LABELS[vectors_n13$Criterion])

# ---- Build the plot ---------------------------------------------------------
fig2 <- ggplot() +
  # Significant envfit arrows (vermillion, thick)
  geom_segment(
    data = vectors_n13[vectors_n13$Significant, ],
    aes(x = 0, y = 0, xend = NMDS1_scaled, yend = NMDS2_scaled),
    arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
    colour = PALETTE$highlight, linewidth = 1.2, alpha = 0.95
  ) +
  # Non-significant envfit arrows (grey, thin)
  geom_segment(
    data = vectors_n13[!vectors_n13$Significant, ],
    aes(x = 0, y = 0, xend = NMDS1_scaled, yend = NMDS2_scaled),
    arrow = arrow(length = unit(1.8, "mm"), type = "closed"),
    colour = PALETTE$neutral, linewidth = 0.5, alpha = 0.45
  ) +
  # Criterion labels
  geom_text_repel(
    data = vectors_n13,
    aes(x = NMDS1_scaled, y = NMDS2_scaled, label = Label,
        colour = Significant, fontface = ifelse(Significant, "bold", "plain")),
    size = 2.8, segment.size = 0.2, segment.alpha = 0.4,
    show.legend = FALSE, max.overlaps = Inf
  ) +
  # Scheme points
  geom_point(data = site_scores_n13,
             aes(x = NMDS1, y = NMDS2),
             size = 3, colour = PALETTE$dark, fill = "white",
             shape = 21, stroke = 0.6) +
  geom_text_repel(data = site_scores_n13,
                  aes(x = NMDS1, y = NMDS2, label = Scheme),
                  size = 2.6, colour = PALETTE$dark,
                  segment.size = 0.2, segment.alpha = 0.4,
                  max.overlaps = Inf,
                  point.padding = unit(0.4, "lines")) +
  scale_colour_manual(values = c("TRUE" = PALETTE$highlight,
                                 "FALSE" = PALETTE$neutral),
                      guide = "none") +
  labs(
    x = "NMDS1", y = "NMDS2",
    subtitle = paste0("Stress = ", round(nmds_n13$stress, 3),
                      " | N = 13 schemes | 9,999 permutations | BH-FDR \u03b1 = 0.05")
  ) +
  coord_equal() +
  theme_manuscript(base_size = 9)

# ---- Save -------------------------------------------------------------------
save_figure(fig2, "Figure2_NMDS_N13.png", width_mm = 290, height_mm = 210)
save_figure(fig2, "Figure2_NMDS_N13.pdf", width_mm = 290, height_mm = 210)

cat("\nFDR-significant criteria (N = 13):\n")
sig <- vectors_n13[vectors_n13$Significant, c("Criterion", "r2", "p_adj")]
print(sig, row.names = FALSE)

message("03_figure2_nmds_n13.R complete.")
