################################################################################
# 25_rebuild_publication_tables_v23.R
# Rebuild `Publication_Tables_v23.xlsx` in the first-mention order dictated by
# Methods_v23.md.  17 supplementary sheets (Table S1-S17) plus Tables 1-6 main.
#
# Design rules:
#   1. Sheet order = first-mention order in Methods_v23.md
#   2. No gaps (unlike Publication_Tables_v3.xlsx which had v8 legacy labels)
#   3. Tables and Notes are separate artefacts (Notes live in
#      Methods_Supplementary_Notes_v23.md, NOT in the xlsx)
#   4. Some v23 tables consolidate content from 2-5 v8 tables (e.g. Table S11
#      = moderation families combined); these are built here from the existing
#      canonical CSVs in `Analysis/tables/`.
#
# Output: v3_stress_process/Analysis/Publication_Tables_v23.xlsx
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
})

proj    <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
tab_dir <- file.path(proj, "Analysis/tables")
out_f   <- file.path(proj, "Analysis/Publication_Tables_v23.xlsx")

# ── Common styles (match prior scheme) ───────────────────────────────────
hdrStyle <- createStyle(
  fontSize = 11, fontColour = "#FFFFFF", fgFill = "#3C5488",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "bottom", borderColour = "#333333", wrapText = TRUE)
bodyStyle <- createStyle(fontSize = 10, halign = "center",
                          valign = "center")
leftStyle <- createStyle(fontSize = 10, halign = "left",
                          valign = "center")
noteStyle <- createStyle(fontSize = 9, fontColour = "#666666",
                          wrapText = TRUE)

wb <- createWorkbook()

# ── Helper: write CSV into a sheet with standard styling ─────────────────
add_sheet <- function(sheet_name, title, csv_name, col_widths = NULL) {
  path <- file.path(tab_dir, csv_name)
  if (!file.exists(path)) {
    cat("  SKIP  ", sheet_name, " - missing ", csv_name, "\n", sep = "")
    return(invisible())
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, title, startRow = 1)
  writeData(wb, sheet_name, df, startRow = 2)
  addStyle(wb, sheet_name, hdrStyle, rows = 2,
           cols = seq_len(ncol(df)), gridExpand = TRUE)
  if (is.null(col_widths)) {
    setColWidths(wb, sheet_name, cols = seq_len(ncol(df)),
                 widths = "auto")
  } else {
    setColWidths(wb, sheet_name, cols = seq_len(length(col_widths)),
                 widths = col_widths)
  }
  cat("  ADDED ", sheet_name, "  (", nrow(df), " x ", ncol(df), ")\n",
      sep = "")
}

# ══════════════════════════════════════════════════════════════════════════
# MAIN TABLES (6) — re-use current content from v3.xlsx via CSV
# ══════════════════════════════════════════════════════════════════════════

add_sheet("Table 1",
  "Table 1. Sample characteristics by country",
  "Table1_Sample_Characteristics.csv")

add_sheet("Table 2",
  "Table 2. Psychometric properties of composite measures",
  "Table2_psychometric_properties.csv")

add_sheet("Table 3",
  "Table 3. Correlation matrix (Spearman) among key constructs",
  "Table3_correlation_matrix.csv")

add_sheet("Table 4",
  "Table 4. Primary multilevel results — MI Rubin-pooled (n = 1,462)",
  "Table4_Primary_Results_MI.csv")

add_sheet("Table 5",
  "Table 5. Mediation: indirect effects (complete-case bootstrap comparator; see Table S9 for Rubin-pooled primary)",
  "Table5_Mediation_Results.csv")

add_sheet("Table 6",
  "Table 6. Country-stratified regression coefficients (complete-case multilevel; within-country k = 3/6/4)",
  "Table6_Country_Stratified_Multilevel.csv")

# ══════════════════════════════════════════════════════════════════════════
# SUPPLEMENTARY TABLES (S1-S17) — first-mention order per Methods_v23.md
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== Supplementary tables (first-mention order) ===\n")

# S1 — Variable construction reference (all derived measures)
add_sheet("Table S1",
  "Table S1. Variable construction — items, scoring rules, scale ranges, missing-data handling, and internal consistency for all derived measures",
  "TableS8_Variable_Construction.csv")

# S2 — EFA loadings + CFA fit (split-sample worry validation)
add_sheet("Table S2",
  "Table S2. Split-sample worry inventory validation — EFA loadings (n = 660) and hold-out CFA fit indices (n = 660)",
  "TableS1_EFA_worry_loadings.csv")

# S3 — Measurement invariance DASS-21 (plus partial invariance for DASS)
add_sheet("Table S3",
  "Table S3. Measurement invariance — DASS-21 three-factor model (configural, metric, scalar, partial scalar across Indonesia, Nepal, Vietnam)",
  "TableS13_Measurement_Invariance.csv")

# S4 — Measurement invariance worry 5-factor (incl. partial)
add_sheet("Table S4",
  "Table S4. Measurement invariance — worry five-factor model with sequential intercept-freeing for partial scalar invariance",
  "TableS13b_Partial_Invariance.csv")

# S5 — Missing data pattern / FMI diagnostics
add_sheet("Table S5",
  "Table S5. Missing data pattern — complete vs incomplete comparison and per-coefficient fraction of missing information",
  "TableS4_Missingness_Comparison.csv")

# S6 — MI congeniality (site dummies retained)
add_sheet("Table S6",
  "Table S6. MI congeniality sensitivity — primary coefficients with site dummies retained in the imputation predictor matrix",
  "TableS14_MI_Congeniality_Sensitivity.csv")

# S7 — VIF
add_sheet("Table S7",
  "Table S7. Variance inflation factors for the M3 fixed-effects block",
  "TableS18_VIF.csv")

# S8 — Cohen's f-squared
add_sheet("Table S8",
  "Table S8. Cohen's f-squared for the worry block (M3 over M2)",
  "TableS20_fsq_worry.csv")

# S9 — Mediation Rubin-pooled
add_sheet("Table S9",
  "Table S9. Mediation — Rubin-pooled indirect effects across m = 30 imputations (Schomaker-Heumann bootstrap-after-imputation)",
  "TableS17_Mediation_Rubin_Pooled.csv")

# S10 — SEM fit indices + structural paths (combined from multiple CSVs)
# v8 had these in separate CSVs; consolidate into a single sheet.
s10_name <- "Table S10"
addWorksheet(wb, s10_name)
writeData(wb, s10_name,
  "Table S10. Structural equation model — fit indices and standardised structural paths for 5-factor, 4-factor (merged material deprivation), and 5-factor MLR pure-CFA specifications",
  startRow = 1)
s10_row <- 2
for (sem_part in c("TableS2b_SEM_fit_indices.csv",
                    "TableS2b_SEM_structural_paths.csv",
                    "TableS2b_SEM_5factor_bounded.csv",
                    "TableS23_SEM_fit_verification.csv")) {
  p <- file.path(tab_dir, sem_part)
  if (!file.exists(p)) next
  writeData(wb, s10_name,
            paste("Panel:", sub("\\.csv$", "", sem_part)),
            startRow = s10_row)
  s10_row <- s10_row + 1
  d <- readr::read_csv(p, show_col_types = FALSE)
  writeData(wb, s10_name, d, startRow = s10_row)
  addStyle(wb, s10_name, hdrStyle, rows = s10_row,
           cols = seq_len(ncol(d)), gridExpand = TRUE)
  s10_row <- s10_row + nrow(d) + 3
}
setColWidths(wb, s10_name, cols = 1:6, widths = "auto")
cat("  ADDED  Table S10  (merged SEM panels)\n")

# S11 — Moderation families consolidated
# Merge: support × worry, stigma contextual, stigma × HCW, worry × location,
# gender × worry, age × worry (from v8 S5, S6, S9, S10, S25)
s11_name <- "Table S11"
addWorksheet(wb, s11_name)
writeData(wb, s11_name,
  "Table S11. Exploratory moderation families — community support x worry, stigma contextual (within- vs between-site) and HCW interaction, worry x location type, and gender / age x worry",
  startRow = 1)
s11_row <- 2
mod_parts <- list(
  list(file = "TableS5_Support_Interactions.csv",
       label = "Panel A — Community support x worry (15 tests)"),
  list(file = "TableS6_Stigma_Contextual.csv",
       label = "Panel B — Stigma contextual decomposition (within-site vs between-site)"),
  list(file = "TableS9_Stigma_HCW_Interaction.csv",
       label = "Panel C — Stigma x HCW-in-household interaction"),
  list(file = "TableS10_Worry_Location_Interactions.csv",
       label = "Panel D — Worry x location type (45 tests)"),
  list(file = "TableS25_Individual_Moderators.csv",
       label = "Panel E — Gender x worry + Age x worry (30 tests)")
)
for (mp in mod_parts) {
  p <- file.path(tab_dir, mp$file)
  if (!file.exists(p)) next
  writeData(wb, s11_name, mp$label, startRow = s11_row)
  s11_row <- s11_row + 1
  d <- readr::read_csv(p, show_col_types = FALSE)
  writeData(wb, s11_name, d, startRow = s11_row)
  addStyle(wb, s11_name, hdrStyle, rows = s11_row,
           cols = seq_len(ncol(d)), gridExpand = TRUE)
  s11_row <- s11_row + nrow(d) + 3
}
setColWidths(wb, s11_name, cols = 1:12, widths = "auto")
cat("  ADDED  Table S11  (merged moderation panels)\n")

# S12 — Binary DASS multilevel
add_sheet("Table S12",
  "Table S12. Binary DASS-21 caseness — multilevel logistic regression (glmer with site random intercept)",
  "TableS26_Binary_DASS_Multilevel.csv")

# ── Post-hoc diagnostics (S13-S17) ──────────────────────────────────────

# S13 — Attenuation-corrected beta
add_sheet("Table S13",
  "Table S13. Attenuation-corrected worry coefficients (beta_obs / sqrt(reliability))",
  "TableS21_Attenuation_Corrected.csv")

# S14 — Leave-one-site
add_sheet("Table S14",
  "Table S14. Site-level leave-one-out — worry coefficient max |delta-beta| summary across 13 drops",
  "TableS24_LeaveOneSite_Summary.csv")

# S15 — Harman CMV
add_sheet("Table S15",
  "Table S15. Harman's single-factor test (common-method variance diagnostic)",
  "TableS22_Harman.csv")

# S16 — Age non-linearity spline
add_sheet("Table S16",
  "Table S16. Age non-linearity — natural cubic spline (df = 4) versus linear LR test",
  "TableS19_Age_Spline.csv")

# S17 — 14-site inclusion sensitivity
add_sheet("Table S17",
  "Table S17. Protocol-fidelity sensitivity — coefficient stability when 38 off-protocol Vietnamese cases are retained (14-site n = 1,500)",
  "TableS16_13sites_vs_14sites.csv")

# ══════════════════════════════════════════════════════════════════════════
# SAVE
# ══════════════════════════════════════════════════════════════════════════

saveWorkbook(wb, out_f, overwrite = TRUE)
cat("\n=== Wrote:", out_f, "===\n")
cat("Sheet order:\n")
print(names(wb))
