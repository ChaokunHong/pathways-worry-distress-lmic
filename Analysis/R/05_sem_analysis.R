################################################################################
# 05_sem_analysis.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities
#
# Purpose: Structural equation modeling with lavaan
#          Two SEM models: (A) 5-factor worry, (B) 4-factor (merged material)
# Input:   spear_analysis_augmented.rds
# Output:  Table S2 (SEM results), SEM results RDS
################################################################################

# ── 0. Setup ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(lavaan)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)

# Convert items to numeric (targeted, not everything)
sem_data <- analytic %>%
  mutate(across(starts_with("q47_"), as.numeric),
         across(starts_with("q51_"), as.numeric),
         across(c(perceived_risk, livelihood_disruption, comorbidity,
                  age, female, site), as.numeric))

cat("SEM data:", nrow(sem_data), "observations\n")

# Item lists
dass_items <- c("q47_3", "q47_5", "q47_10", "q47_13", "q47_16", "q47_17", "q47_21",
                "q47_2", "q47_4", "q47_7", "q47_9", "q47_15", "q47_19", "q47_20",
                "q47_1", "q47_6", "q47_8", "q47_11", "q47_12", "q47_14", "q47_18")

worry_items <- c("q51_2_quarantine", "q51_3_infected", "q51_4_severe",
                 "q51_5_death", "q51_6_health_system",
                 "q51_10_income", "q51_14_rent", "q51_15_debt",
                 "q51_16_agri", "q51_17_harvest", "q51_18_produce", "q51_21_recession",
                 "q51_11_food", "q51_12_water", "q51_13_medicine",
                 "q51_1_lonely", "q51_7_education", "q51_8_teaching", "q51_9_children_home",
                 "q51_19_stigma", "q51_20_abuse")

all_items <- c(dass_items, worry_items)

# ══════════════════════════════════════════════════════════════════════════════
# MODEL A: 5-Factor Worry (original specification — may have Heywood case)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n=== MODEL A: 5-FACTOR WORRY SEM ===\n")

sem_5f <- '
  # Measurement
  DEPR =~ q47_3 + q47_5 + q47_10 + q47_13 + q47_16 + q47_17 + q47_21
  ANX  =~ q47_2 + q47_4 + q47_7 + q47_9 + q47_15 + q47_19 + q47_20
  STR  =~ q47_1 + q47_6 + q47_8 + q47_11 + q47_12 + q47_14 + q47_18
  HFEAR  =~ q51_2_quarantine + q51_3_infected + q51_4_severe + q51_5_death + q51_6_health_system
  ECON   =~ q51_10_income + q51_14_rent + q51_15_debt + q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession
  BNEEDS =~ q51_11_food + q51_12_water + q51_13_medicine
  SOCIAL =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIGMA =~ q51_19_stigma + q51_20_abuse

  # Structural
  HFEAR  ~ perceived_risk
  ECON   ~ livelihood_disruption
  BNEEDS ~ livelihood_disruption
  STIGMA ~ perceived_risk
  DEPR ~ HFEAR + ECON + BNEEDS + SOCIAL + STIGMA + perceived_risk + comorbidity + age + female
  ANX  ~ HFEAR + ECON + BNEEDS + SOCIAL + STIGMA + perceived_risk + comorbidity + age + female
  STR  ~ HFEAR + ECON + BNEEDS + SOCIAL + STIGMA + perceived_risk + comorbidity + age + female

  HFEAR ~~ ECON + BNEEDS + SOCIAL + STIGMA
  ECON ~~ BNEEDS + SOCIAL + STIGMA
  BNEEDS ~~ SOCIAL + STIGMA
  SOCIAL ~~ STIGMA
  DEPR ~~ ANX + STR
  ANX ~~ STR
'

fit_5f <- tryCatch(
  sem(sem_5f, data = sem_data, ordered = all_items,
      estimator = "WLSMV", std.lv = TRUE),
  error = function(e) { cat("5-factor SEM error:", e$message, "\n"); NULL }
)

if (!is.null(fit_5f)) {
  post_ok_5f <- lavInspect(fit_5f, "post.check")
  cat("5-factor post-check:", post_ok_5f, "\n")
  if (!post_ok_5f) cat("WARNING: Heywood case in 5-factor model (expected with BNEEDS 3 items)\n")
  fit_5f_idx <- fitMeasures(fit_5f, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))
  cat("Fit: CFI=", round(fit_5f_idx["cfi"], 3), " RMSEA=", round(fit_5f_idx["rmsea"], 3), "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# MODEL B: 4-Factor Worry (ECON + BNEEDS merged → MATERIAL) — PRIMARY SEM
# Resolves Heywood case by providing 10 indicators for material hardship
# ══════════════════════════════════════════════════════════════════════════════

cat("\n=== MODEL B: 4-FACTOR WORRY SEM (PRIMARY — HEYWOOD FIX) ===\n")

sem_4f <- '
  # === Measurement Model ===
  DEPR =~ q47_3 + q47_5 + q47_10 + q47_13 + q47_16 + q47_17 + q47_21
  ANX  =~ q47_2 + q47_4 + q47_7 + q47_9 + q47_15 + q47_19 + q47_20
  STR  =~ q47_1 + q47_6 + q47_8 + q47_11 + q47_12 + q47_14 + q47_18

  # Merged: Economic concerns + Basic needs → Material hardship (10 items)
  MATERIAL =~ q51_10_income + q51_14_rent + q51_15_debt +
              q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession +
              q51_11_food + q51_12_water + q51_13_medicine
  HFEAR    =~ q51_2_quarantine + q51_3_infected + q51_4_severe +
              q51_5_death + q51_6_health_system
  SOCIAL   =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIGMA   =~ q51_19_stigma + q51_20_abuse

  # === Structural Paths ===
  # Layer 2 → Layer 3
  HFEAR    ~ a1*perceived_risk
  MATERIAL ~ a2*livelihood_disruption
  STIGMA   ~ a3*perceived_risk
  SOCIAL   ~ livelihood_disruption

  # Layer 3 → Outcomes
  DEPR ~ b1*HFEAR + b2*MATERIAL + b3*SOCIAL + b4*STIGMA + c1*perceived_risk + comorbidity + age + female
  ANX  ~ b5*HFEAR + b6*MATERIAL + b7*SOCIAL + b8*STIGMA + c2*perceived_risk + comorbidity + age + female
  STR  ~ b9*HFEAR + b10*MATERIAL + b11*SOCIAL + b12*STIGMA + c3*perceived_risk + comorbidity + age + female

  # Residual covariances
  HFEAR ~~ MATERIAL + SOCIAL + STIGMA
  MATERIAL ~~ SOCIAL + STIGMA
  SOCIAL ~~ STIGMA
  DEPR ~~ ANX + STR
  ANX ~~ STR

  # === Indirect Effects ===
  # Perceived risk → Health fear → Outcomes
  ind_risk_hfear_depr := a1*b1
  ind_risk_hfear_anx  := a1*b5
  ind_risk_hfear_str  := a1*b9
  # Livelihood → Material hardship → Outcomes
  ind_live_mat_depr   := a2*b2
  ind_live_mat_anx    := a2*b6
  ind_live_mat_str    := a2*b10
  # Perceived risk → Stigma → Outcomes
  ind_risk_stig_depr  := a3*b4
  ind_risk_stig_anx   := a3*b8
  ind_risk_stig_str   := a3*b12
'

fit_4f <- tryCatch(
  sem(sem_4f, data = sem_data, ordered = all_items,
      estimator = "WLSMV", std.lv = TRUE, cluster = "site"),
  error = function(e) {
    cat("4-factor SEM with clustering failed:", e$message, "\n")
    cat("Trying without clustering...\n")
    sem(sem_4f, data = sem_data, ordered = all_items,
        estimator = "WLSMV", std.lv = TRUE)
  }
)

# ── Check & Report ──────────────────────────────────────────────────────────

post_ok_4f <- lavInspect(fit_4f, "post.check")
cat("4-factor post-check:", post_ok_4f, "\n")

fit_4f_idx <- fitMeasures(fit_4f, c("chisq", "df", "pvalue",
                                     "cfi", "tli", "rmsea",
                                     "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
cat("\n4-Factor SEM Fit:\n")
print(round(fit_4f_idx, 3))

# Extract structural paths
std_sol <- standardizedSolution(fit_4f)

structural_paths <- std_sol %>%
  filter(op == "~") %>%
  dplyr::select(lhs, op, rhs, est.std, se, pvalue, ci.lower, ci.upper) %>%
  mutate(
    across(c(est.std, se, ci.lower, ci.upper), ~ round(.x, 3)),
    pvalue = round(pvalue, 4),
    sig = case_when(pvalue < 0.001 ~ "***", pvalue < 0.01 ~ "**",
                    pvalue < 0.05 ~ "*", TRUE ~ "")
  )

cat("\n--- Paths: Exposure → Worry Domains ---\n")
structural_paths %>%
  filter(rhs %in% c("perceived_risk", "livelihood_disruption"),
         lhs %in% c("HFEAR", "MATERIAL", "STIGMA", "SOCIAL")) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Paths: Worry Domains → Outcomes ---\n")
structural_paths %>%
  filter(lhs %in% c("DEPR", "ANX", "STR"),
         rhs %in% c("HFEAR", "MATERIAL", "SOCIAL", "STIGMA")) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Direct Paths: Covariates → Outcomes ---\n")
structural_paths %>%
  filter(lhs %in% c("DEPR", "ANX", "STR"),
         rhs %in% c("perceived_risk", "comorbidity", "age", "female")) %>%
  as.data.frame() %>% print(row.names = FALSE)

# Extract indirect effects
indirect_eff <- parameterEstimates(fit_4f) %>%
  filter(op == ":=") %>%
  dplyr::select(lhs, est, se, pvalue, ci.lower, ci.upper) %>%
  mutate(across(c(est, se, ci.lower, ci.upper), ~ round(.x, 4)),
         pvalue = round(pvalue, 4))

cat("\n--- Indirect Effects ---\n")
print(as.data.frame(indirect_eff), row.names = FALSE)

# ── Save Results ────────────────────────────────────────────────────────────

# Fit comparison table
fit_comparison <- tibble(
  model = c("5-Factor Worry (original)", "4-Factor Worry (merged material)"),
  post_check = c(ifelse(!is.null(fit_5f), post_ok_5f, NA), post_ok_4f),
  CFI   = c(ifelse(!is.null(fit_5f), round(fit_5f_idx["cfi"], 3), NA),
            round(fit_4f_idx["cfi"], 3)),
  TLI   = c(ifelse(!is.null(fit_5f), round(fit_5f_idx["tli"], 3), NA),
            round(fit_4f_idx["tli"], 3)),
  RMSEA = c(ifelse(!is.null(fit_5f), round(fit_5f_idx["rmsea"], 3), NA),
            round(fit_4f_idx["rmsea"], 3)),
  SRMR  = c(ifelse(!is.null(fit_5f), round(fit_5f_idx["srmr"], 3), NA),
            round(fit_4f_idx["srmr"], 3))
)

cat("\n=== MODEL COMPARISON ===\n")
print(as.data.frame(fit_comparison), row.names = FALSE)

write_csv(fit_comparison, file.path(tab_dir, "TableS2b_SEM_fit_indices.csv"))
write_csv(structural_paths, file.path(tab_dir, "TableS2b_SEM_structural_paths.csv"))

if (nrow(indirect_eff) > 0) {
  write_csv(indirect_eff, file.path(tab_dir, "TableS2b_SEM_indirect_effects.csv"))
}

saveRDS(list(
  fit_4f = fit_4f, fit_5f = fit_5f,
  structural_paths = structural_paths,
  indirect_effects = indirect_eff,
  fit_comparison = fit_comparison,
  std_solution = std_sol
), file.path(out_dir, "sem_results.rds"))

cat("\n=== 05_sem_analysis.R COMPLETE ===\n")
