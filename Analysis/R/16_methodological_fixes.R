################################################################################
# 16_methodological_fixes.R
#
# Address reviewer-identified methodological issues from v5 critique:
#   (A) MI congeniality — include site dummies as predictors
#   (B) Diagnose n=1,527 vs n=1,500 R² discrepancy
#   (C) SEM Heywood — refit with bounded loadings, preserving 5-factor structure
#   (D) Mediation — document CC vs MI divergence on the mediator path
#
# Output: updated tables, diagnostic CSVs, and a summary log for Methods v6.
################################################################################

library(haven)
library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
library(mitml)
library(MuMIn)
library(lavaan)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- d %>% filter(complete_dass, !is.na(site), site %in% 1:13)
cat("N analytic:", nrow(analytic), "\n")

################################################################################
# (A) MI congeniality fix — include site as factor predictor
################################################################################

imp_vars <- c("z_depression","z_anxiety","z_stress",
              "age","female","ethnicity_minority","education","employment",
              "comorbidity","gen_health",
              "perceived_risk","risk_self","health_trust",
              "worry_health_fear","worry_economic","worry_basic_needs",
              "worry_social","worry_safety_stigma",
              "livelihood_disruption",
              "country","site")

imp_data <- analytic %>%
  dplyr::select(all_of(imp_vars)) %>%
  mutate(
    female = as.factor(female),
    ethnicity_minority = as.factor(ethnicity_minority),
    education = as.integer(education),
    employment = as.factor(employment),
    country = as.factor(country),
    site = as.factor(site)  # CHANGED: factor, not integer
  )

ini <- mice(imp_data, maxit = 0, print = FALSE)
meth <- ini$method
meth[c("z_depression","z_anxiety","z_stress","country","site")] <- ""
meth["female"] <- "logreg"
meth["ethnicity_minority"] <- "logreg"
meth["employment"] <- "polyreg"

pred <- ini$predictorMatrix
# CHANGE vs 03c_primary_mi.R: retain site as predictor (dummy-expanded internally)
pred[, "site"] <- 1L
diag(pred) <- 0L

cat("\n=== MI RE-RUN (site retained in predictor matrix) ===\n")
set.seed(2026)
mi_obj_v2 <- mice(imp_data, m = 30, maxit = 10,
                  method = meth, predictorMatrix = pred, print = FALSE)
saveRDS(mi_obj_v2, file.path(out_dir, "mi_object_v2_site_predictor.rds"))
cat("Saved: mi_object_v2_site_predictor.rds\n")

# Fit M3 on the updated MI object
f_m3 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust +
          worry_health_fear + worry_economic + worry_basic_needs +
          worry_social + worry_safety_stigma +
          livelihood_disruption + (1|site)"

fit_and_pool <- function(mi_obj, outcome) {
  fits <- lapply(1:mi_obj$m, function(i) {
    dat_i <- complete(mi_obj, i)
    lmer(as.formula(paste(outcome, f_m3)), data = dat_i, REML = TRUE)
  })
  est <- testEstimates(fits, extra.pars = FALSE)
  mat <- est$estimates
  # Column positions: 1=Estimate 2=SE 3=t 4=df 5=p 6=RIV 7=FMI
  res <- data.frame(term = rownames(mat),
                    outcome = outcome,
                    estimate = mat[,1],
                    se = mat[,2],
                    t_value = mat[,3],
                    df = mat[,4],
                    p_value = mat[,5],
                    riv = mat[,6],
                    fmi = mat[,7],
                    row.names = NULL)
  res
}

outcomes <- c("z_depression","z_anxiety","z_stress")
pooled_v2 <- bind_rows(lapply(outcomes, fit_and_pool, mi_obj = mi_obj_v2))
write_csv(pooled_v2, file.path(tab_dir, "Table4_Primary_Results_MI_v2_site.csv"))
cat("Saved: Table4_Primary_Results_MI_v2_site.csv\n")

# Compare worry coefficients between v1 (original) and v2 (site in pred)
old <- read_csv(file.path(tab_dir, "Table4_Primary_Results_MI.csv"),
                show_col_types = FALSE) %>%
  filter(grepl("^worry_", term)) %>%
  dplyr::select(term, outcome, estimate_v1 = estimate, p_v1 = p.value,
                fmi_v1 = fmi)

# Normalise outcome labels for join
old <- old %>% mutate(outcome = paste0("z_", tolower(outcome)))

new <- pooled_v2 %>%
  filter(grepl("^worry_", term)) %>%
  dplyr::select(term, outcome,
                estimate_v2 = estimate,
                p_v2 = p_value,
                fmi_v2 = fmi)

cmp <- left_join(old, new, by = c("term","outcome")) %>%
  mutate(delta_beta = round(estimate_v2 - estimate_v1, 4),
         delta_p    = round(p_v2 - p_v1, 4))
write_csv(cmp, file.path(tab_dir, "Diagnostic_MI_congeniality_comparison.csv"))
cat("Saved: Diagnostic_MI_congeniality_comparison.csv\n")
print(cmp)

################################################################################
# (B) Diagnose n=1,527 vs n=1,500 R² discrepancy
################################################################################
# Determine which variables contribute to the 27-case listwise drop
model_vars <- c("z_depression","z_anxiety","z_stress",
                "country","age","female","ethnicity_minority","education",
                "employment","comorbidity","gen_health",
                "perceived_risk","risk_self","health_trust",
                "worry_health_fear","worry_economic","worry_basic_needs",
                "worry_social","worry_safety_stigma",
                "livelihood_disruption","site")
cc_full <- complete.cases(analytic[, model_vars])
cat("\n=== n=1,527 vs R² subset diagnostic ===\n")
cat("Complete-case n on M3 model matrix:", sum(cc_full), "/ 1,527\n")
cat("Per-variable missingness among analytic sample:\n")
miss_tab <- sapply(model_vars, function(v) sum(is.na(analytic[[v]])))
print(miss_tab[miss_tab > 0])

################################################################################
# (C) SEM refit with bounded loading (preserves 5-factor structure)
################################################################################
worry_mod <- '
  HFEAR =~ q51_2_quarantine + q51_3_infected + q51_4_severe + q51_5_death + q51_6_health_system
  ECON  =~ q51_10_income + q51_14_rent + q51_15_debt + q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession
  BNEED =~ q51_11_food + q51_12_water + q51_13_medicine
  SOC   =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIG  =~ q51_19_stigma + q51_20_abuse
'

cat("\n=== SEM: 5-factor with default estimator ===\n")
sem5_unconstrained <- cfa(worry_mod, data = analytic, estimator = "MLR")
fm5 <- fitMeasures(sem5_unconstrained, c("cfi","tli","rmsea","srmr"))
cat("5-factor MLR fit:\n"); print(fm5)

# Check for Heywood: any |std loading| > 1
std <- standardizedSolution(sem5_unconstrained)
heywood <- std %>% filter(op == "=~", abs(est.std) > 1)
cat("\nHeywood cases (|std loading| > 1):\n")
print(heywood)

# Bounded refit: constrain residual variances to be positive using
# lavaan's built-in bounds (check.start / bounds = "standard")
sem5_bounded <- cfa(worry_mod, data = analytic, estimator = "MLR",
                    bounds = "standard")
fm5b <- fitMeasures(sem5_bounded, c("cfi","tli","rmsea","srmr"))
cat("\n5-factor MLR with bounded estimation:\n"); print(fm5b)

# Save standardised solution
write_csv(standardizedSolution(sem5_bounded),
          file.path(tab_dir, "TableS2b_SEM_5factor_bounded.csv"))
cat("Saved: TableS2b_SEM_5factor_bounded.csv\n")

################################################################################
# (D) Mediation: complete-case vs primary MI coefficient on worry path
################################################################################
cat("\n=== Mediation diagnostic (CC vs MI on worry→outcome path) ===\n")
# Use primary MI results (Table4_Primary_Results_MI.csv, which is MI-based)
# vs complete-case multilevel results (Table4_Primary_Results_Multilevel.csv)
mi_tab <- read_csv(file.path(tab_dir, "Table4_Primary_Results_MI.csv"),
                   show_col_types = FALSE) %>%
  filter(grepl("^worry_", term)) %>%
  dplyr::select(term, outcome, estimate_mi = estimate, p_mi = p.value)

cc_tab <- read_csv(file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv"),
                   show_col_types = FALSE) %>%
  filter(grepl("^worry_", term)) %>%
  dplyr::select(term, outcome,
                estimate_cc = estimate,
                p_cc = p.value)

cmp_med <- left_join(mi_tab, cc_tab, by = c("term","outcome")) %>%
  mutate(sign_agree = sign(estimate_mi) == sign(estimate_cc),
         both_sig05 = p_mi < 0.05 & p_cc < 0.05)
write_csv(cmp_med, file.path(tab_dir, "Diagnostic_Mediation_CC_vs_MI.csv"))
cat("Rows with sign disagreement:", sum(!cmp_med$sign_agree, na.rm=TRUE), "\n")
cat("Rows significant under both:", sum(cmp_med$both_sig05, na.rm=TRUE), "\n")
print(cmp_med)

cat("\n=== 16_methodological_fixes.R COMPLETE ===\n")
