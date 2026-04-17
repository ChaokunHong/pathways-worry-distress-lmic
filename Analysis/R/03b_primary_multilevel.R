# DEPRECATED (v7+ canonical): this script is preserved for historical traceability.
# The canonical 13-site sample writers for all downstream tables are
# 17_primary_13sites.R and 18_rerun_all_on_13sites.R. If you run THIS script
# standalone, its outputs may overwrite canonical CSVs with 14-site or old-filter
# content. Always re-run 17 and then 18 after running this script.
#
################################################################################
# 03b_primary_multilevel.R
# PRIMARY ANALYSIS: Multilevel models (lme4) with sequential building
#
# Rationale for lme4 over CR2:
# - CR2 with k=13 clusters produces overly conservative inference
#   (Satterthwaite df can be as low as 2-5)
# - lme4 REML is appropriate for k>=10 (Maas & Hox 2005; McNeish 2017)
# - CR2 results reported as sensitivity analysis in 06_sensitivity.R
#
# Output: Primary regression tables, forest plot data
################################################################################

library(tidyverse)
library(lme4)
library(lmerTest)  # Provides Satterthwaite p-values for lmer
library(broom.mixed)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)
cat("Analytic sample:", nrow(analytic), "\n")

outcomes <- c("z_depression", "z_anxiety", "z_stress")
outcome_labels <- c("Depression", "Anxiety", "Stress")

# ── 1. Sequential Model Formulas ────────────────────────────────────────────

formulas <- list(
  M0 = "~ country + (1|site)",

  M1 = "~ country + age + female + ethnicity_minority + education +
         employment + comorbidity + gen_health + (1|site)",

  M2 = "~ country + age + female + ethnicity_minority + education +
         employment + comorbidity + gen_health +
         perceived_risk + risk_self + health_trust + (1|site)",

  M3 = "~ country + age + female + ethnicity_minority + education +
         employment + comorbidity + gen_health +
         perceived_risk + risk_self + health_trust +
         worry_health_fear + worry_economic + worry_basic_needs +
         worry_social + worry_safety_stigma +
         livelihood_disruption + (1|site)",

  M4 = "~ country + age + female + ethnicity_minority + education +
         employment + comorbidity + gen_health +
         perceived_risk + risk_self + health_trust +
         worry_health_fear + worry_economic + worry_basic_needs +
         worry_social + worry_safety_stigma +
         livelihood_disruption +
         compliance + prevention_evidence + prevention_folk +
         support_close + support_community + support_government +
         (1|site)",

  # M4b: Alternative Layer 4 with help-seeking and media trust
  M4b = "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust +
          worry_health_fear + worry_economic + worry_basic_needs +
          worry_social + worry_safety_stigma +
          livelihood_disruption +
          support_close + support_community + support_government +
          help_close + help_community + help_government +
          helpseeking_formal + helpseeking_self +
          media_trust + (1|site)"
)

# ── 2. Fit All Models ───────────────────────────────────────────────────────

all_results <- list()
r2_table <- tibble()

for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  y_lab <- outcome_labels[oi]
  cat("\n=== Fitting models for", y_lab, "===\n")

  for (mname in names(formulas)) {
    f <- as.formula(paste(y, formulas[[mname]]))

    model <- tryCatch(
      lmer(f, data = analytic, REML = TRUE),
      error = function(e) { cat("  ", mname, "ERROR:", e$message, "\n"); NULL }
    )

    if (!is.null(model)) {
      coefs <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
        mutate(model = mname, outcome = y_lab)
      all_results[[paste(y_lab, mname)]] <- coefs

      # Marginal R² (fixed effects only) and conditional R² (fixed + random)
      r2_m <- tryCatch(MuMIn::r.squaredGLMM(model), error = function(e) c(R2m=NA, R2c=NA))
      n_obs <- nrow(model.frame(model))

      r2_table <- bind_rows(r2_table, tibble(
        outcome = y_lab, model = mname,
        R2_marginal = round(r2_m[1], 4),
        R2_conditional = round(r2_m[2], 4),
        n = n_obs
      ))

      cat("  ", mname, "done (R²m =", round(r2_m[1], 4),
          "R²c =", round(r2_m[2], 4), "n =", n_obs, ")\n")
    }
  }
}

# ── 3. R² Decomposition ────────────────────────────────────────────────────

cat("\n=== R² DECOMPOSITION ===\n")

r2_wide <- r2_table %>%
  pivot_wider(names_from = outcome, values_from = c(R2_marginal, R2_conditional, n))

print(as.data.frame(r2_wide), row.names = FALSE)
write_csv(r2_wide, file.path(tab_dir, "R2_decomposition_multilevel.csv"))

# ── 4. Primary Model (M3) Results ───────────────────────────────────────────

cat("\n=== PRIMARY MODEL (M3) — MULTILEVEL ===\n")

m3_results <- map_dfr(outcome_labels, function(ol) {
  res <- all_results[[paste(ol, "M3")]]
  if (!is.null(res)) {
    res %>%
      filter(term != "(Intercept)") %>%
      dplyr::select(term, estimate, std.error, statistic, p.value,
                     conf.low, conf.high, outcome) %>%
      mutate(
        across(c(estimate, std.error, statistic, conf.low, conf.high), ~ round(.x, 4)),
        p.value = round(p.value, 4),
        sig = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          p.value < 0.10  ~ "\u2020",
          TRUE ~ ""
        )
      )
  }
})

# Print worry sub-domain results
cat("\nWorry Sub-Domains (M3):\n")
m3_worry <- m3_results %>%
  filter(grepl("worry_", term)) %>%
  dplyr::select(outcome, term, estimate, conf.low, conf.high, p.value, sig) %>%
  as.data.frame()
print(m3_worry, row.names = FALSE)

# Print all key predictors
cat("\nAll Key Predictors (M3):\n")
key_terms <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
               "worry_social", "worry_safety_stigma",
               "perceived_risk", "livelihood_disruption",
               "health_trust", "risk_self",
               "age", "female", "comorbidity")

m3_key <- m3_results %>%
  filter(term %in% key_terms) %>%
  dplyr::select(outcome, term, estimate, conf.low, conf.high, p.value, sig) %>%
  as.data.frame()
print(m3_key, row.names = FALSE)

write_csv(m3_results, file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv"))

# ── 5. ICC from Empty Model ─────────────────────────────────────────────────

cat("\n=== INTRACLASS CORRELATIONS ===\n")
for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  m0 <- lmer(as.formula(paste(y, "~ 1 + (1|site)")), data = analytic, REML = TRUE)
  vc <- as.data.frame(VarCorr(m0))
  icc <- vc$vcov[1] / sum(vc$vcov)
  cat(outcome_labels[oi], ": ICC =", round(icc, 4), "\n")
}

# ── 6. Country-Stratified Models ────────────────────────────────────────────

cat("\n=== COUNTRY-STRATIFIED MODELS ===\n")

f_strat <- "~ age + female + ethnicity_minority + education +
             employment + comorbidity + gen_health +
             perceived_risk + risk_self + health_trust +
             worry_health_fear + worry_economic + worry_basic_needs +
             worry_social + worry_safety_stigma +
             livelihood_disruption + (1|site)"

strat_results <- list()

for (cntry in c("Indonesia", "Nepal", "Vietnam")) {
  cat("\n", cntry, ":\n")
  cdata <- analytic %>% filter(country == cntry)

  for (oi in seq_along(outcomes)) {
    model <- tryCatch(
      lmer(as.formula(paste(outcomes[oi], f_strat)), data = cdata, REML = TRUE),
      error = function(e) {
        # Fallback to lm if random effect fails
        lm(as.formula(paste(outcomes[oi], gsub("\\+ \\(1\\|site\\)", "", f_strat))),
           data = cdata)
      }
    )

    res <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
      mutate(country = cntry, outcome = outcome_labels[oi])
    strat_results[[paste(cntry, outcome_labels[oi])]] <- res

    r2 <- tryCatch(MuMIn::r.squaredGLMM(model)[1],
                    error = function(e) summary(model)$adj.r.squared)
    cat("  ", outcome_labels[oi], "R²m =", round(r2, 4),
        "n =", nrow(model.frame(model)), "\n")
  }
}

strat_df <- bind_rows(strat_results)
write_csv(strat_df, file.path(tab_dir, "Table6_Country_Stratified_Multilevel.csv"))

# ── 7. Save for Figures ─────────────────────────────────────────────────────

labels_map <- c(
  comorbidity = "Comorbidity count",
  female = "Female (vs male)",
  age = "Age (years)",
  risk_self = "Self-assessed risk",
  perceived_risk = "Perceived health threat",
  health_trust = "Health system trust",
  worry_safety_stigma = "Worry: Safety / stigma",
  worry_basic_needs   = "Worry: Basic needs",
  worry_social        = "Worry: Social / family",
  worry_economic      = "Worry: Economic concerns",
  livelihood_disruption = "Livelihood disruption",
  worry_health_fear   = "Worry: Health fears"
)

layer_map <- c(
  comorbidity = "Vulnerability", female = "Vulnerability", age = "Vulnerability",
  risk_self = "Appraisal", perceived_risk = "Appraisal", health_trust = "Appraisal",
  worry_safety_stigma = "Stressors", worry_basic_needs = "Stressors",
  worry_social = "Stressors", worry_economic = "Stressors",
  livelihood_disruption = "Stressors", worry_health_fear = "Stressors"
)

forest_data <- m3_results %>%
  filter(term %in% key_terms) %>%
  mutate(
    label = factor(labels_map[term], levels = rev(labels_map)),
    layer = factor(layer_map[term], levels = c("Vulnerability", "Appraisal", "Stressors"))
  )

saveRDS(forest_data, file.path(out_dir, "forest_plot_data.rds"))

# Also save all coefficients
all_coef <- bind_rows(all_results)
write_csv(all_coef, file.path(tab_dir, "all_model_coefficients_multilevel.csv"))

cat("\n=== 03b_primary_multilevel.R COMPLETE ===\n")
