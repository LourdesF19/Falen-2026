# =============================================================================
# 00_setup.R — Packages, palette, theme, and shared helpers
#
# Falen, L., Aide, T.M. & Alonso, A. (2026)
# Ecological baselines and co-governance as foundational design choices
# for the voluntary biodiversity credit market.
# Communications Sustainability.
#
# This script loads all required R packages, defines the Okabe-Ito colour
# palette used throughout the manuscript, defines the shared ggplot theme
# (theme_manuscript), and sets a global random seed for reproducibility.
#
# It is sourced by every other script in the R/ directory.
# =============================================================================

# ---- Packages ---------------------------------------------------------------

required_packages <- c(
  "readxl",       # read Excel scoring matrix
  "dplyr",        # data manipulation
  "tidyr",        # reshape
  "vegan",        # NMDS, envfit, metaMDS
  "cluster",      # silhouette
  "ggplot2",      # plotting
  "ggrepel",      # non-overlapping point labels
  "patchwork",    # combine plots
  "scales"        # axis formatting
)

missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  stop(
    "The following packages are required but not installed: ",
    paste(missing, collapse = ", "),
    "\nInstall them with: install.packages(c('",
    paste(missing, collapse = "', '"), "'))"
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(vegan)
  library(cluster)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(scales)
})

# ---- Reproducibility --------------------------------------------------------

set.seed(42)

# ---- Paths ------------------------------------------------------------------

# Resolve paths relative to the repository root, regardless of where the
# script is sourced from. Assumes the working directory is the repo root.
DATA_FILE <- file.path("data", "scoring_data.xlsx")
OUTPUT_DIR <- "outputs"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---- Colour palette (Okabe-Ito, colour-blind friendly) ----------------------

# Okabe, M. & Ito, K. (2008). Color Universal Design.
# https://jfly.uni-koeln.de/color/
PALETTE <- list(
  governance  = "#0072B2",   # blue
  integrity   = "#009E73",   # green
  monitoring  = "#E69F00",   # orange
  highlight   = "#D55E00",   # vermillion (FDR-significant criteria)
  neutral     = "#999999",   # grey (non-significant)
  dark        = "#222222"    # axis lines, text
)

# ---- Shared ggplot theme ----------------------------------------------------

theme_manuscript <- function(base_size = 9, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid       = element_blank(),
      axis.line        = element_line(colour = PALETTE$dark, linewidth = 0.4),
      axis.ticks       = element_line(colour = PALETTE$dark, linewidth = 0.3),
      axis.text        = element_text(colour = PALETTE$dark),
      axis.title       = element_text(colour = PALETTE$dark),
      plot.title       = element_text(face = "bold", hjust = 0),
      plot.subtitle    = element_text(colour = PALETTE$dark),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", colour = PALETTE$dark),
      legend.position  = "right",
      legend.title     = element_text(face = "bold")
    )
}

# ---- Criterion labels (unified across figures and tables) -------------------

CRITERION_LABELS <- c(
  "1.State oversight"               = "State oversight",
  "2.Rights & FPIC"                 = "Rights & FPIC",
  "3.Co-governance"                 = "Co-governance",
  "4.Safeguards"                    = "Safeguards",
  "5.Accountability"                = "Accountability",
  "6.Eligibility"                   = "Eligibility",
  "7.Additionality & durability"    = "Additionality & durability",
  "8.Credit calculation"            = "Credit calculation",
  "9.MRV registry"                  = "MRV registry",
  "10.Risk management"              = "Risk management",
  "11.Site selection"               = "Site selection",
  "12.Baseline sampling"            = "Baseline sampling",
  "13.Integrated biodiv. monitoring"= "Integrated monitoring",
  "14.Data transparency"            = "Data transparency",
  "15.Independent validation"       = "Independent validation"
)

# Dimension assignment for each criterion (1-5: Governance; 6-10: Integrity;
# 11-15: Monitoring)
CRITERION_DIMENSION <- c(
  rep("Governance, Rights & Equity", 5),
  rep("Credit Integrity",            5),
  rep("Biodiversity Monitoring",     5)
)
names(CRITERION_DIMENSION) <- names(CRITERION_LABELS)

# ---- Helpers ----------------------------------------------------------------

#' Save a ggplot at manuscript-ready dimensions (290 x 210 mm, 300 dpi)
save_figure <- function(plot, filename,
                        width_mm = 290, height_mm = 210, dpi = 300) {
  ggsave(
    filename = file.path(OUTPUT_DIR, filename),
    plot     = plot,
    width    = width_mm,
    height   = height_mm,
    units    = "mm",
    dpi      = dpi,
    bg       = "white"
  )
  message("Saved: ", file.path(OUTPUT_DIR, filename))
}

#' Print a tidy header to the console for log readability
section_header <- function(title) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(" ", title, "\n", sep = "")
  cat(strrep("=", 70), "\n\n", sep = "")
}

message("00_setup.R sourced: packages loaded, palette and theme defined.")
