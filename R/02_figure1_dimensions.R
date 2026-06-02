# =============================================================================
# 02_figure1_dimensions.R — Figure 1 of the manuscript
#
# Horizontal bar charts showing mean scores per scheme across the three
# evaluation dimensions (Governance, Credit Integrity, Biodiversity
# Monitoring). Bars are coloured by whether the dimension score meets the
# 10-point documentation threshold.
# =============================================================================

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_load_and_score.R"))

section_header("02 — Building Figure 1 (dimension bar charts)")

# ---- Long-format data for plotting ------------------------------------------
fig1_data <- dim_scores %>%
  select(Scheme, Governance, Integrity, Monitoring) %>%
  pivot_longer(
    cols      = c(Governance, Integrity, Monitoring),
    names_to  = "Dimension",
    values_to = "Score"
  ) %>%
  mutate(
    Dimension = factor(
      Dimension,
      levels = c("Governance", "Integrity", "Monitoring"),
      labels = c("Governance, Rights & Equity",
                 "Credit Integrity",
                 "Biodiversity Monitoring")
    ),
    MeetsThreshold = Score >= 10
  )

# Order schemes by total score (descending) so high performers appear on top
scheme_order <- dim_scores$Scheme[order(dim_scores$Total)]
fig1_data$Scheme <- factor(fig1_data$Scheme, levels = scheme_order)

# ---- Plot -------------------------------------------------------------------
fig1 <- ggplot(fig1_data, aes(x = Score, y = Scheme, fill = MeetsThreshold)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 10, linetype = "dashed",
             colour = PALETTE$dark, linewidth = 0.4) +
  facet_wrap(~ Dimension, nrow = 1) +
  scale_fill_manual(
    values = c("TRUE" = PALETTE$governance, "FALSE" = PALETTE$highlight),
    labels = c("TRUE" = "Threshold met (\u2265 10)",
               "FALSE" = "Below threshold (< 10)"),
    name   = NULL
  ) +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, 5),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Dimension score (sum of 5 criteria, 0\u201315)", y = NULL) +
  theme_manuscript(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines")
  )

# ---- Save -------------------------------------------------------------------
save_figure(fig1, "Figure1_dimension_scores.png",
            width_mm = 290, height_mm = 210)
save_figure(fig1, "Figure1_dimension_scores.pdf",
            width_mm = 290, height_mm = 210)

# Console summary
cat("\nNumber of schemes below threshold per dimension:\n")
cat(sprintf("  Governance:  %d of 20\n", sum(dim_scores$Governance < 10)))
cat(sprintf("  Integrity:   %d of 20\n", sum(dim_scores$Integrity  < 10)))
cat(sprintf("  Monitoring:  %d of 20\n", sum(dim_scores$Monitoring < 10)))

message("02_figure1_dimensions.R complete.")
