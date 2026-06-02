# =============================================================================
# 01_load_and_score.R — Load scoring data and compute inter-rater agreement
#
# This script loads the four sheets of the scoring workbook (three independent
# evaluators + their mean) and computes Kendall's coefficient of concordance
# (W) for each of the 15 evaluation criteria. The output reproduces
# Supplementary Table 2 of the manuscript.
# =============================================================================

source(file.path("R", "00_setup.R"))

section_header("01 — Loading scoring data and computing Kendall's W")

# ---- Load all four sheets ---------------------------------------------------

if (!file.exists(DATA_FILE)) {
  stop("Data file not found at ", DATA_FILE,
       ". The repository must be cloned with the data/ folder intact.")
}

sheets <- excel_sheets(DATA_FILE)
stopifnot(all(c("Mean", "Evaluator_1", "Evaluator_2", "Evaluator_3") %in% sheets))

data_mean  <- read_excel(DATA_FILE, sheet = "Mean")        %>% as.data.frame()
data_ev1   <- read_excel(DATA_FILE, sheet = "Evaluator_1") %>% as.data.frame()
data_ev2   <- read_excel(DATA_FILE, sheet = "Evaluator_2") %>% as.data.frame()
data_ev3   <- read_excel(DATA_FILE, sheet = "Evaluator_3") %>% as.data.frame()

# Harmonise column names across sheets to the canonical names used in the
# "Mean" sheet (the per-evaluator sheets have slightly different spellings)
canonical_names <- colnames(data_mean)
colnames(data_ev1) <- canonical_names
colnames(data_ev2) <- canonical_names
colnames(data_ev3) <- canonical_names

# Sanity checks: same schemes, same dimensions
stopifnot(nrow(data_mean) == 20)
stopifnot(ncol(data_mean) == 16)  # Schemes + 15 criteria
stopifnot(identical(data_mean$Schemes, data_ev1$Schemes))
stopifnot(identical(data_mean$Schemes, data_ev2$Schemes))
stopifnot(identical(data_mean$Schemes, data_ev3$Schemes))

cat("Loaded scoring data:\n")
cat("  - 20 schemes\n")
cat("  - 15 evaluation criteria\n")
cat("  - 3 independent evaluators + arithmetic mean\n\n")

# ---- Build the analytical matrix -------------------------------------------
# Rows = schemes, columns = criteria, values = mean across 3 evaluators
schemes <- data_mean$Schemes
score_matrix <- as.matrix(data_mean[, -1])
rownames(score_matrix) <- schemes
mode(score_matrix) <- "numeric"

# Aggregate dimension scores (sums of 5 criteria each)
dim_scores <- data.frame(
  Scheme     = schemes,
  Governance = rowSums(score_matrix[, 1:5]),
  Integrity  = rowSums(score_matrix[, 6:10]),
  Monitoring = rowSums(score_matrix[, 11:15]),
  stringsAsFactors = FALSE
)
dim_scores$Total <- dim_scores$Governance + dim_scores$Integrity +
                    dim_scores$Monitoring

# ---- Documentation threshold (>= 10 in at least 2 of 3 dimensions) ----------
dim_scores$NumAbove10 <- with(dim_scores,
  (Governance >= 10) + (Integrity >= 10) + (Monitoring >= 10)
)
dim_scores$ThresholdMet <- dim_scores$NumAbove10 >= 2

n13_schemes <- dim_scores$Scheme[dim_scores$ThresholdMet]
cat("Schemes meeting the documentation threshold (N = ",
    length(n13_schemes), "):\n", sep = "")
cat(paste("  -", n13_schemes), sep = "\n")
cat("\n")

# ---- Kendall's W per criterion (Supplementary Table 2) ----------------------
# For each criterion, build a 20 x 3 matrix of scores (rows = schemes,
# columns = evaluators). Kendall's W is the standard non-parametric
# concordance statistic for K judges ranking N items.

kendall_w_per_criterion <- function(crit_col) {
  mat <- cbind(
    data_ev1[[crit_col]],
    data_ev2[[crit_col]],
    data_ev3[[crit_col]]
  )
  # Convert to ranks within each evaluator
  ranks <- apply(mat, 2, rank, ties.method = "average")
  n <- nrow(ranks); k <- ncol(ranks)
  R <- rowSums(ranks)
  S <- sum((R - mean(R))^2)
  # Correction for ties
  T_corr <- sum(apply(mat, 2, function(x) {
    t <- table(x)
    sum(t^3 - t)
  }))
  W <- (12 * S) / (k^2 * (n^3 - n) - k * T_corr)
  W
}

criterion_cols <- canonical_names[-1]
kendall_w <- sapply(criterion_cols, kendall_w_per_criterion)

kendall_table <- data.frame(
  Criterion_ID = seq_along(criterion_cols),
  Criterion    = unname(CRITERION_LABELS[criterion_cols]),
  Kendall_W    = round(kendall_w, 2),
  stringsAsFactors = FALSE
)

cat("Kendall's W per criterion (Supplementary Table 2):\n")
print(kendall_table, row.names = FALSE)
cat("\n")

stopifnot(all(kendall_w >= 0.4))  # minimum agreement reported in the paper

# ---- Save outputs -----------------------------------------------------------
write.csv(dim_scores,
          file.path(OUTPUT_DIR, "table_dimension_scores.csv"),
          row.names = FALSE)
write.csv(kendall_table,
          file.path(OUTPUT_DIR, "tableS2_kendall_w.csv"),
          row.names = FALSE)
write.csv(score_matrix,
          file.path(OUTPUT_DIR, "tableS3_mean_score_matrix.csv"),
          row.names = TRUE)

message("01_load_and_score.R complete.")
