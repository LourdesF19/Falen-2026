# =============================================================================
# 07_bootstrap_means.R — Bootstrap confidence intervals and Wilcoxon tests
#
# Addresses Reviewer 1 comment R1.4 by quantifying whether the three
# dimension-level means (Governance, Credit Integrity, Biodiversity
# Monitoring) are statistically distinguishable, given the small sample size
# (N = 20) and the large standard deviations relative to the differences in
# means.
#
# Two procedures:
#   - Percentile bootstrap 95% CIs around each dimension mean, with 10,000
#     resamples (Efron & Tibshirani 1993).
#   - Paired Wilcoxon signed-rank tests between dimensions (Wilcoxon 1945),
#     with Benjamini-Hochberg FDR correction across the three comparisons.
# =============================================================================

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_load_and_score.R"))

section_header("07 - Bootstrap confidence intervals and Wilcoxon tests")

set.seed(42)
n_boot <- 10000

# ---- Bootstrap CIs ---------------------------------------------------------
boot_ci <- function(x, n_boot = 10000) {
  n <- length(x)
  boot_means <- replicate(n_boot, mean(sample(x, n, replace = TRUE)))
  c(
    mean  = mean(x),
    sd    = sd(x),
    lower = unname(quantile(boot_means, 0.025)),
    upper = unname(quantile(boot_means, 0.975))
  )
}

ci_governance <- boot_ci(dim_scores$Governance, n_boot)
ci_integrity  <- boot_ci(dim_scores$Integrity,  n_boot)
ci_monitoring <- boot_ci(dim_scores$Monitoring, n_boot)

ci_table <- data.frame(
  Dimension = c("Governance, Rights & Equity",
                "Credit Integrity",
                "Biodiversity Monitoring"),
  Mean      = round(c(ci_governance["mean"],
                      ci_integrity["mean"],
                      ci_monitoring["mean"]), 2),
  SD        = round(c(ci_governance["sd"],
                      ci_integrity["sd"],
                      ci_monitoring["sd"]), 2),
  CI_lower  = round(c(ci_governance["lower"],
                      ci_integrity["lower"],
                      ci_monitoring["lower"]), 2),
  CI_upper  = round(c(ci_governance["upper"],
                      ci_integrity["upper"],
                      ci_monitoring["upper"]), 2),
  n         = 20,
  stringsAsFactors = FALSE
)

cat("Bootstrap 95% confidence intervals (10,000 resamples):\n")
print(ci_table, row.names = FALSE)
cat("\n")
write.csv(ci_table, file.path(OUTPUT_DIR, "table_bootstrap_CI_dimensions.csv"),
          row.names = FALSE)

# ---- Paired Wilcoxon signed-rank tests -------------------------------------
w_gi <- wilcox.test(dim_scores$Governance, dim_scores$Integrity,  paired = TRUE)
w_gm <- wilcox.test(dim_scores$Governance, dim_scores$Monitoring, paired = TRUE)
w_im <- wilcox.test(dim_scores$Integrity,  dim_scores$Monitoring, paired = TRUE)

wilcox_table <- data.frame(
  Comparison = c("Governance vs Integrity",
                 "Governance vs Monitoring",
                 "Integrity vs Monitoring"),
  Mean_diff  = round(c(
    mean(dim_scores$Governance - dim_scores$Integrity),
    mean(dim_scores$Governance - dim_scores$Monitoring),
    mean(dim_scores$Integrity  - dim_scores$Monitoring)
  ), 2),
  W          = c(w_gi$statistic, w_gm$statistic, w_im$statistic),
  p_raw      = round(c(w_gi$p.value, w_gm$p.value, w_im$p.value), 4),
  stringsAsFactors = FALSE
)
wilcox_table$p_FDR <- round(p.adjust(wilcox_table$p_raw, method = "BH"), 4)
wilcox_table$Significant <- wilcox_table$p_FDR < 0.05

cat("Paired Wilcoxon signed-rank tests with BH-FDR correction:\n")
print(wilcox_table, row.names = FALSE)
cat("\nInterpretation: no comparison is statistically significant after FDR.\n")
write.csv(wilcox_table,
          file.path(OUTPUT_DIR, "table_wilcoxon_dimensions.csv"),
          row.names = FALSE)

message("07_bootstrap_means.R complete.")
