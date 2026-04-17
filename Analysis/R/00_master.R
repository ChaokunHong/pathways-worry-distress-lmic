################################################################################
# 00_master.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities.
#
# Master script: orchestrates ALL analysis scripts in the canonical order
# required to reproduce Methods_FINAL_v7.md numbers from raw data.
#
# Target: Social Science & Medicine
# Framework: Stress Process Model (Pearlin et al. 1981) as orienting framework
# Data: SPEAR study, primary analytic sample n = 1,462
#       (13 protocol-specified sites; 607 Indonesia / 460 Nepal / 395 Vietnam)
#
# Canonical execution order (produces every table/figure cited in Methods v7):
#   01  construct development (loads raw data; derives all variables;
#                              outputs spear_analysis_augmented.rds)
#   02  descriptives (Table 1, Table 3, EFA/CFA psychometric development)
#   03b primary multilevel (complete-case comparator)
#   03c primary MI         (MI on original 14-site sample — ARCHIVAL)
#   17  primary MI 13-site (CANONICAL primary analysis, n = 1,462)
#   18  consolidation      (rewrites every downstream canonical table
#                           on the 13-site sample; de-duplicates)
#   04  mediation          (5 pathways on 13-site complete cases)
#   05  SEM
#   06  sensitivity        (5 sensitivity analyses incl. binary DASS & CR2)
#   10  context effects    (stigma contextual, support × worry interactions)
#   12  supervisor v2      (HCW, location, country-FE, Nepal-ref tables)
#   15  measurement invariance  (retained on complete-DASS sample of n = 1,527
#                                for maximum power; 13-site restriction is
#                                not enforced here — metric-level parameters
#                                are the quantity of interest)
#   16  methodological fixes   (MI congeniality sensitivity, SEM Heywood
#                               diagnostic; runs on canonical 13-site data)
#   07  figures            (Figures 3–7 from canonical tables)
#   08  publication tables (Publication_Tables.xlsx)
#   11  Figure S3          (support × worry buffering)
#   13  Figure S4          (worry × location interactions)
#   14  Figure 1 Panel B
#   19  Figure 1 Panel A + combined
################################################################################

cat("============================================================\n")
cat("MASTER ANALYSIS PIPELINE (v7, canonical 13-site primary)\n")
cat("============================================================\n\n")

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
script_dir <- file.path(base_dir, "v3_stress_process", "Analysis", "R")
start_time <- Sys.time()

run_step <- function(n, label, script) {
  cat(sprintf("\n>>> STEP %02d: %s  [%s] <<<\n", n, label, script))
  source(file.path(script_dir, script))
}

# CRITICAL ORDERING RULE:
#   18_rerun_all_on_13sites.R must run AFTER every other script that writes
#   any canonical CSV. 10 and 12 write overlapping tables (stigma contextual,
#   HCW interaction, country FE necessity, Nepal reference, worry × location);
#   if they run after 18, they silently overwrite canonical 13-site CSVs with
#   14-site content. 18 is therefore placed last among the writers.

run_step( 1, "Construct development",          "01_construct_development.R")
run_step( 2, "Descriptives & EFA/CFA",         "02_descriptives.R")
run_step( 3, "Multilevel CC comparator",       "03b_primary_multilevel.R")
run_step( 4, "Primary MI (archival 14-site)",  "03c_primary_mi.R")
run_step( 5, "PRIMARY MI 13-site (canonical)", "17_primary_13sites.R")
run_step( 6, "Mediation CC (bootstrap, arch.)","04_mediation.R")
run_step( 7, "SEM",                            "05_sem_analysis.R")
run_step( 8, "Sensitivity analyses",           "06_sensitivity.R")
run_step( 9, "Context effects (arch.)",        "10_context_effects.R")
run_step(10, "Supervisor v2 responses (arch.)","12_supervisor_v2_responses.R")
run_step(11, "Measurement invariance",         "15_measurement_invariance.R")
run_step(12, "Partial invariance (v8 add)",    "15b_partial_invariance.R")
run_step(13, "Methodological-fix diagnostics", "16_methodological_fixes.R")
run_step(14, "CANONICAL CONSOLIDATION (last)", "18_rerun_all_on_13sites.R")
# ── v8 audit-response analyses (run AFTER 18 so canonical tables exist) ────
run_step(15, "Mediation Rubin-pooled (v8)",    "04b_mediation_rubin_pooled.R")
run_step(16, "Audit-response quick (v8)",      "21_audit_response_quick.R")
run_step(17, "Leave-one-site M3 (v8)",         "22_leave_one_site_out.R")
run_step(18, "Individual moderators (v8)",     "23_moderator_exploratory.R")
run_step(19, "Binary DASS multilevel (v8)",    "24_binary_dass_multilevel.R")
# ── Figure and publication-table generation ───────────────────────────────
run_step(20, "Figures 3–7 + Figure S1 MICE",  "07_figures.R")
run_step(21, "Publication tables v23 xlsx",    "25_rebuild_publication_tables_v23.R")
run_step(22, "Figure S3 (support buffering)",  "11_figure_S3_support.R")
run_step(23, "Figure S4 (worry × location)",   "13_figure_S4_location.R")
run_step(24, "Figure 1 Panel B",               "14_figure1_flow.R")
run_step(25, "Figure 1 Panel A + combined",    "19_figure1_panel_a.R")
run_step(26, "Figure 2 Methods overview (v8)", "20_figure2_methods_overview.R")

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

cat("\n============================================================\n")
cat("PIPELINE COMPLETE\n")
cat("Total time:", round(as.numeric(duration), 1), "minutes\n")
cat("Primary sample: n = 1,462 across 13 protocol-specified sites\n")
cat("============================================================\n\n")

out_dir <- file.path(base_dir, "v3_stress_process", "Analysis")
cat("Outputs in:\n")
cat("  ", out_dir, "\n")
cat("Canonical artifacts:\n")
cat("  - Table4_Primary_Results_MI.csv      (n = 1,462 primary MI)\n")
cat("  - R2_decomposition_MI.csv            (marginal R^2 on 13 sites)\n")
cat("  - Publication_Tables.xlsx            (all publication tables)\n")
cat("  - Figure1_StudySites_Flow.png        (combined Panel A + B)\n")
cat("  - Figure2_Methods_Overview.pdf       (analytical-pipeline schematic, v8)\n")
cat("v8 audit-response supplementary outputs:\n")
cat("  - TableS13b_Partial_Invariance.csv\n")
cat("  - TableS17_Mediation_Rubin_Pooled.csv\n")
cat("  - TableS18_VIF.csv .. TableS26_Binary_DASS_Multilevel.csv\n")
cat("  - FigureS1_MICE_Convergence.pdf, FigureS2_M3_Residuals.pdf,\n")
cat("    FigureS3_Support_Buffering.pdf, FigureS4_Location_Interactions.pdf,\n")
cat("    FigureS5_LeaveOneSite_Forest.pdf\n")
