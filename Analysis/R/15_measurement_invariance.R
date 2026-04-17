################################################################################
# 15_measurement_invariance.R
# Tests configural / metric / scalar invariance of DASS-21 and the five-factor
# worry model across Indonesia, Nepal, and Vietnam.
# Addresses reviewer concern: pooled n=1,527 across three linguistic contexts
# requires at least metric invariance before country-stratified coefficients
# (Table 6) can be compared.
################################################################################

library(lavaan)
library(dplyr)
library(readr)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")
out_dir  <- file.path(proj_dir, "Analysis", "output")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
d <- d %>% filter(complete_dass)
cat("N:", nrow(d), " | by country:\n"); print(table(d$country))

# ── 1. DASS-21 three-factor (Dep/Anx/Stress) ────────────────────────────────
dass_mod <- '
  DEP =~ q47_3 + q47_5 + q47_10 + q47_13 + q47_16 + q47_17 + q47_21
  ANX =~ q47_2 + q47_4 + q47_7 + q47_9 + q47_15 + q47_19 + q47_20
  STR =~ q47_1 + q47_6 + q47_8 + q47_11 + q47_12 + q47_14 + q47_18
'

# ── 2. Worry five-factor ────────────────────────────────────────────────────
worry_mod <- '
  HFEAR =~ q51_2_quarantine + q51_3_infected + q51_4_severe + q51_5_death + q51_6_health_system
  ECON  =~ q51_10_income + q51_14_rent + q51_15_debt + q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession
  BNEED =~ q51_11_food + q51_12_water + q51_13_medicine
  SOC   =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIG  =~ q51_19_stigma + q51_20_abuse
'

run_invariance <- function(model, label) {
  cfg <- tryCatch(cfa(model, data = d, group = "country",
                      estimator = "MLR"), error = function(e) NULL)
  met <- tryCatch(cfa(model, data = d, group = "country",
                      estimator = "MLR",
                      group.equal = "loadings"), error = function(e) NULL)
  sca <- tryCatch(cfa(model, data = d, group = "country",
                      estimator = "MLR",
                      group.equal = c("loadings","intercepts")), error = function(e) NULL)

  extract_fit <- function(m, lbl) {
    if (is.null(m)) return(data.frame(level = lbl, cfi = NA, tli = NA,
                                       rmsea = NA, srmr = NA, chisq = NA, df = NA))
    fm <- fitMeasures(m, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr","chisq","df"))
    data.frame(level = lbl,
               cfi = round(fm[["cfi.scaled"]], 3),
               tli = round(fm[["tli.scaled"]], 3),
               rmsea = round(fm[["rmsea.scaled"]], 3),
               srmr = round(fm[["srmr"]], 3),
               chisq = round(fm[["chisq"]], 1),
               df = fm[["df"]])
  }

  res <- bind_rows(
    extract_fit(cfg, "Configural"),
    extract_fit(met, "Metric (equal loadings)"),
    extract_fit(sca, "Scalar (equal loadings + thresholds)")
  ) %>% mutate(model = label, .before = 1)

  # Delta CFI between nested levels
  res$delta_cfi <- c(NA, res$cfi[2] - res$cfi[1], res$cfi[3] - res$cfi[2])
  res$delta_rmsea <- c(NA, res$rmsea[2] - res$rmsea[1], res$rmsea[3] - res$rmsea[2])
  res
}

cat("\n── DASS-21 invariance ──\n")
dass_res  <- run_invariance(dass_mod,  "DASS-21 (3-factor)")
print(dass_res)

cat("\n── Worry 5-factor invariance ──\n")
worry_res <- run_invariance(worry_mod, "Worry (5-factor)")
print(worry_res)

out <- bind_rows(dass_res, worry_res)
write_csv(out, file.path(tab_dir, "TableS_Measurement_Invariance.csv"))
cat("\nWrote: TableS_Measurement_Invariance.csv\n")
cat("Interpretation rule (Chen 2007): ΔCFI ≤ .010 and ΔRMSEA ≤ .015 support invariance.\n")
