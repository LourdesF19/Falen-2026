# =============================================================================
# 04_figS2_nmds_n20.R — Supplementary Figure 2 (sensitivity analysis, N = 20)
#
# NMDS ordination repeated with all 20 schemes (including the seven that did
# not meet the documentation threshold). Same pipeline as Figure 2:
# Bray-Curtis dissimilarity, metaMDS, envfit with 9,999 permutations,
# Benjamini-Hochberg FDR correction.
#
# This analysis addresses Reviewer 5 comment R5.6 (robustness check with all
# 20 schemes) and produces the right half of Supplementary Table 4.
# =============================================================================

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_load_and_score.R"))

section_header("04 — Supplementary Figure 2: NMDS sensitivity (N = 20)")

set.seed(42)

# ---- NMDS via vegan ---------------------------------------------------------
nmds_n20 <- metaMDS(
  comm     = score_matrix,
  distance = "bray",
  k        = 2,
  trymax   = 200,
  trace    = FALSE,
  autotransform = FALSE
)
cat("NMDS stress (N = 20):", round(nmds_n20$stress, 3), "\n\n")

site_scores_n20 <- as.data.frame(scores(nmds_n20, display = "sites"))
site_scores_n20$Scheme        <- rownames(site_scores_n20)
site_scores_n20$TotalScore    <- dim_scores$Total[match(site_scores_n20$Scheme,
                                                        dim_scores$Scheme)]
site_scores_n20$ThresholdMet  <- dim_scores$ThresholdMet[match(
  site_scores_n20$Scheme, dim_scores$Scheme)]
site_scores_n20$ID            <- seq_len(nrow(site_scores_n20))

# ---- envfit on the 15 criteria ---------------------------------------------
env_n20 <- envfit(nmds_n20, score_matrix, permutations = 9999)
vectors_n20 <- as.data.frame(scores(env_n20, display = "vectors"))
vectors_n20$Criterion <- rownames(vectors_n20)
vectors_n20$r2        <- env_n20$vectors$r
vectors_n20$pval      <- env_n20$vectors$pvals
vectors_n20$p_adj     <- p.adjust(vectors_n20$pval, method = "BH")
vectors_n20$Significant <- vectors_n20$p_adj < 0.05

vectors_n20 <- vectors_n20[order(-vectors_n20$r2), ]
print(vectors_n20[, c("Criterion", "r2", "pval", "p_adj", "Significant")],
      row.names = FALSE)
write.csv(vectors_n20, file.path(OUTPUT_DIR, "table_envfit_N20.csv"),
          row.names = FALSE)

# ---- Arrow scaling ---------------------------------------------------------
display_scale <- 2.0
arrow_scale   <- 1.3 * display_scale
vectors_n20$NMDS1_scaled <- vectors_n20$NMDS1 * sqrt(vectors_n20$r2) * arrow_scale
vectors_n20$NMDS2_scaled <- vectors_n20$NMDS2 * sqrt(vectors_n20$r2) * arrow_scale
vectors_n20$Label <- unname(CRITERION_LABELS[vectors_n20$Criterion])

# ---- Plot ------------------------------------------------------------------
figS2 <- ggplot() +
  geom_segment(
    data = vectors_n20[vectors_n20$Significant, ],
    aes(x = 0, y = 0, xend = NMDS1_scaled, yend = NMDS2_scaled),
    arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
    colour = PALETTE$highlight, linewidth = 1.2, alpha = 0.95
  ) +
  geom_segment(
    data = vectors_n20[!vectors_n20$Significant, ],
    aes(x = 0, y = 0, xend = NMDS1_scaled, yend = NMDS2_scaled),
    arrow = arrow(length = unit(1.8, "mm"), type = "closed"),
    colour = PALETTE$neutral, linewidth = 0.5, alpha = 0.45
  ) +
  geom_text_repel(
    data = vectors_n20,
    aes(x = NMDS1_scaled, y = NMDS2_scaled, label = Label,
        colour = Significant,
        fontface = ifelse(Significant, "bold", "plain")),
    size = 2.6, segment.size = 0.2, segment.alpha = 0.4,
    show.legend = FALSE, max.overlaps = Inf
  ) +
  # Scheme points coloured by total documentation score
  geom_point(data = site_scores_n20,
             aes(x = NMDS1, y = NMDS2, fill = TotalScore),
             size = 3.5, shape = 21, colour = PALETTE$dark, stroke = 0.5) +
  geom_text(data = site_scores_n20,
            aes(x = NMDS1, y = NMDS2, label = ID),
            size = 2.4, colour = PALETTE$dark, fontface = "bold") +
  scale_fill_gradient(low = "#DEEBF7", high = "#08519C",
                      name = "Total\nscore") +
  scale_colour_manual(values = c("TRUE" = PALETTE$highlight,
                                 "FALSE" = PALETTE$neutral),
                      guide = "none") +
  labs(
    x = "NMDS1", y = "NMDS2",
    subtitle = paste0("Stress = ", round(nmds_n20$stress, 3),
                      " | N = 20 schemes | sensitivity analysis")
  ) +
  coord_equal() +
  theme_manuscript(base_size = 9)

save_figure(figS2, "SupplFigure2_NMDS_N20.png",
            width_mm = 290, height_mm = 210)
save_figure(figS2, "SupplFigure2_NMDS_N20.pdf",
            width_mm = 290, height_mm = 210)

cat("\nFDR-significant criteria (N = 20):\n")
sig_n20 <- vectors_n20[vectors_n20$Significant, c("Criterion", "r2", "p_adj")]
print(sig_n20, row.names = FALSE)
cat("\nNumber of FDR-significant criteria:", sum(vectors_n20$Significant),
    "of 15\n")

message("04_figS2_nmds_n20.R complete.")
