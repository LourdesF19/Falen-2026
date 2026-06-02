# =============================================================================
# run_all.R - Reproduce the entire analytical pipeline
#
# Source this script from the repository root to reproduce all results,
# figures, and tables reported in Falen, Aide & Alonso (2026).
#
# Usage from the command line:
#   Rscript run_all.R
#
# Usage from an interactive R session:
#   setwd("path/to/falen-2026")
#   source("run_all.R")
#
# Outputs are written to the outputs/ directory.
# =============================================================================

cat("\n")
cat("============================================================\n")
cat(" Falen, Aide & Alonso (2026)\n")
cat(" Reproducing the full analytical pipeline\n")
cat("============================================================\n\n")

scripts <- c(
  "R/00_setup.R",
  "R/01_load_and_score.R",
  "R/02_figure1_dimensions.R",
  "R/03_figure2_nmds_n13.R",
  "R/04_figS2_nmds_n20.R",
  "R/05_figS3_clusters.R",
  "R/06_doc_gradient.R",
  "R/07_bootstrap_means.R"
)

for (s in scripts) {
  cat("\n>>> Running:", s, "\n")
  source(s)
}

cat("\n")
cat("============================================================\n")
cat(" Pipeline complete. See outputs/ for figures and tables.\n")
cat("============================================================\n")
