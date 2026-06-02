# =============================================================================
# 06_doc_gradient.R — Documentation-gradient diagnostic
#
# Tests whether NMDS1 in the N = 20 ordination is dominated by an overall
# documentation-level gradient (sum of all 15 criterion scores per scheme).
# This diagnostic empirically justifies the 10-point documentation threshold
# applied in the manuscript and addresses Reviewer 1 comment R1.5 and
# Reviewer 5 comment R5.5.
#
# Three tests are performed:
#   Test 1: Pearson correlation between NMDS1 and total score, for N=13 and
#           N=20.
#   Test 2: envfit using total score as a single variable in the N=20 NMDS.
#   Test 3: Residualise each criterion against the total score and re-run
#           the NMDS + envfit on the residualised data.
# =============================================================================

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_load_and_score.R"))

section_header("06 — Documentation-gradient diagnostic")

set.seed(42)

# ---- Recompute NMDS for both subsets ---------------------------------------
M13 <- score_matrix[n13_schemes, , drop = FALSE]
nmds_n13 <- metaMDS(M13, distance = "bray", k = 2, trymax = 200,
                    trace = FALSE, autotransform = FALSE)
nmds_n20 <- metaMDS(score_matrix, distance = "bray", k = 2, trymax = 200,
                    trace = FALSE, autotransform = FALSE)

scores_n13 <- as.data.frame(scores(nmds_n13, display = "sites"))
scores_n13$Scheme <- rownames(scores_n13)
scores_n13$Total  <- rowSums(M13)

scores_n20 <- as.data.frame(scores(nmds_n20, display = "sites"))
scores_n20$Scheme <- rownames(scores_n20)
scores_n20$Total  <- rowSums(score_matrix)

# ---- Test 1: Pearson correlation NMDS1 vs Total ----------------------------
cor13 <- cor.test(scores_n13$NMDS1, scores_n13$Total)
cor20 <- cor.test(scores_n20$NMDS1, scores_n20$Total)

cat("Test 1 - Pearson correlation NMDS1 vs total score:\n")
cat(sprintf("  N = 13:  r = %.3f, r\u00b2 = %.3f, p = %.4f\n",
            cor13$estimate, cor13$estimate^2, cor13$p.value))
cat(sprintf("  N = 20:  r = %.3f, r\u00b2 = %.3f, p = %.4f\n",
            cor20$estimate, cor20$estimate^2, cor20$p.value))
cat("\n")

# ---- Test 2: envfit with total score as a single variable in N=20 ----------
env_total <- envfit(nmds_n20, scores_n20$Total, permutations = 9999)
cat("Test 2 - envfit with total score as a single variable in N=20:\n")
cat(sprintf("  r\u00b2 = %.3f, p = %.4f\n",
            env_total$vectors$r, env_total$vectors$pvals))
cat("\n")

# ---- Test 3: Partial out the total-score gradient --------------------------
# Residualise each criterion against the total score, then re-run NMDS
residualise <- function(M) {
  total <- rowSums(M)
  apply(M, 2, function(x) resid(lm(x ~ total)))
}
# Add a constant to keep residuals non-negative (required for Bray-Curtis)
M20_resid <- residualise(score_matrix)
M20_resid <- M20_resid - min(M20_resid) + 0.01

nmds_resid <- metaMDS(M20_resid, distance = "bray", k = 2, trymax = 200,
                      trace = FALSE, autotransform = FALSE)
env_resid <- envfit(nmds_resid, score_matrix, permutations = 9999)

vec_resid <- data.frame(
  Criterion = rownames(scores(env_resid, display = "vectors")),
  r2        = env_resid$vectors$r,
  pval      = env_resid$vectors$pvals
)
vec_resid$p_adj <- p.adjust(vec_resid$pval, method = "BH")
vec_resid$Significant <- vec_resid$p_adj < 0.05
vec_resid <- vec_resid[order(-vec_resid$r2), ]

cat("Test 3 - envfit on the residualised N=20 matrix (after partialing out total):\n")
print(vec_resid, row.names = FALSE)
write.csv(vec_resid,
          file.path(OUTPUT_DIR, "table_envfit_N20_residualised.csv"),
          row.names = FALSE)

# ---- Save summary diagnostic table -----------------------------------------
diag_summary <- data.frame(
  Test     = c("NMDS1 ~ Total (N=13)",
               "NMDS1 ~ Total (N=20)",
               "envfit Total (N=20)"),
  r        = c(cor13$estimate, cor20$estimate, NA),
  r_squared = c(cor13$estimate^2, cor20$estimate^2, env_total$vectors$r),
  p_value  = c(cor13$p.value, cor20$p.value, env_total$vectors$pvals)
)
write.csv(diag_summary,
          file.path(OUTPUT_DIR, "table_documentation_gradient_diagnostic.csv"),
          row.names = FALSE)

message("06_doc_gradient.R complete.")
