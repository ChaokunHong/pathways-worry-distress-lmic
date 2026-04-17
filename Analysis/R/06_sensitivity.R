################################################################################
# 06_sensitivity.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities
#
# Purpose: Sensitivity analyses for robustness
# Input:   spear_analysis_augmented.rds
# Output:  Table S3, Table S4
################################################################################

# ── 0. Setup ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(estimatr)
library(lme4)
library(broom)
library(broom.mixed)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)

outcomes <- c("z_depression", "z_anxiety", "z_stress")
outcome_labels <- c("Depression", "Anxiety", "Stress")

# Primary model formula (M3 — without coping variables to maximize sample)
f_primary <- "~ country + age + female + ethnicity_minority + education +
               employment + comorbidity + gen_health +
               perceived_risk + risk_self + health_trust +
               worry_health_fear + worry_economic + worry_basic_needs +
               worry_social + worry_safety_stigma +
               livelihood_disruption"

all_sensitivity <- list()

# ── 1. Sensitivity 1: Aggregate Worry Score (Original Approach) ─────────────

cat("\n=== SENSITIVITY 1: AGGREGATE WORRY SCORE ===\n")

f_aggregate <- "~ country + age + female + ethnicity_minority + education +
                 employment + comorbidity + gen_health +
                 perceived_risk + risk_self + health_trust +
                 worry_total + livelihood_disruption"

for (i in seq_along(outcomes)) {
  model <- lm_robust(as.formula(paste(outcomes[i], f_aggregate)),
                     data = analytic, clusters = site, se_type = "CR2")
  res <- tidy(model) %>% mutate(outcome = outcome_labels[i], sensitivity = "Aggregate worry")
  all_sensitivity[[paste("agg", outcome_labels[i])]] <- res
  cat(outcome_labels[i], ": adj R² =", round(glance(model)$adj.r.squared, 4), "\n")
}

# ── 2. Sensitivity 2: Binary DASS Outcomes (Multilevel Logistic) ────────────
# Corrected from single-level glm() in v7 to multilevel glmer() with site
# random intercept, matching Methods v8 Section 2.3.8(iii).
# Canonical per-coefficient OR table is TableS26, produced by
# 24_binary_dass_multilevel.R; here we regenerate the raw-OR table for
# Table S3 alignment.

cat("\n=== SENSITIVITY 2: BINARY DASS OUTCOMES (MULTILEVEL LOGISTIC) ===\n")

binary_outcomes <- c("depr_any", "anx_any", "stress_any")

for (i in seq_along(binary_outcomes)) {
  full_f <- as.formula(paste(binary_outcomes[i], f_primary, "+ (1 | site)"))

  model <- tryCatch(
    glmer(full_f, data = analytic, family = binomial(link = "logit"),
          control = glmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 2e5))),
    error = function(e) { cat("Error:", e$message, "\n"); NULL }
  )

  if (!is.null(model)) {
    res <- broom.mixed::tidy(model, exponentiate = TRUE, conf.int = TRUE,
                              effects = "fixed") %>%
      mutate(outcome = outcome_labels[i],
             sensitivity = "Binary multilevel (OR)")
    all_sensitivity[[paste("bin", outcome_labels[i])]] <- res
    cat(outcome_labels[i], "- converged, n =", nobs(model), "\n")
  }
}

# ── 3. Sensitivity 3: Multilevel Model (for reviewer comparison) ────────────

cat("\n=== SENSITIVITY 3: MULTILEVEL MODEL (lme4) ===\n")

for (i in seq_along(outcomes)) {
  full_f <- as.formula(paste(outcomes[i], f_primary, "+ (1 | site)"))

  model <- tryCatch(
    lmer(full_f, data = analytic, REML = TRUE),
    error = function(e) { cat("Error:", e$message, "\n"); NULL }
  )

  if (!is.null(model)) {
    res <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
      mutate(outcome = outcome_labels[i], sensitivity = "Multilevel (lme4)")
    all_sensitivity[[paste("mlm", outcome_labels[i])]] <- res

    # ICC
    vc <- as.data.frame(VarCorr(model))
    icc <- vc$vcov[1] / sum(vc$vcov)
    cat(outcome_labels[i], "- ICC =", round(icc, 4), "\n")
  }
}

# ── 4. Sensitivity 4: Excluding Overlapping Worry Items ─────────────────────

cat("\n=== SENSITIVITY 4: EXCLUDING OVERLAPPING WORRY ITEMS ===\n")
cat("(Removing q51_1 lonely and q51_20 abuse which overlap with DASS content)\n")

# Recalculate worry sub-domains without overlapping items
analytic_excl <- analytic %>%
  mutate(
    # Social without lonely
    worry_social_excl = worry_social - q51_1_lonely,
    # Stigma without abuse
    worry_stigma_excl = worry_safety_stigma - q51_20_abuse
  )

f_excl <- "~ country + age + female + ethnicity_minority + education +
            employment + comorbidity + gen_health +
            perceived_risk + risk_self + health_trust +
            worry_health_fear + worry_economic + worry_basic_needs +
            worry_social_excl + worry_stigma_excl +
            livelihood_disruption"

for (i in seq_along(outcomes)) {
  model <- lm_robust(as.formula(paste(outcomes[i], f_excl)),
                     data = analytic_excl, clusters = site, se_type = "CR2")
  res <- tidy(model) %>%
    mutate(outcome = outcome_labels[i], sensitivity = "Excluding overlapping items")
  all_sensitivity[[paste("excl", outcome_labels[i])]] <- res
  cat(outcome_labels[i], ": adj R² =", round(glance(model)$adj.r.squared, 4), "\n")
}

# ── 5. Sensitivity 5: Complete Case (No MI/FIML comparison) ────────────────

cat("\n=== SENSITIVITY 5: COMPLETE CASE COMPARISON ===\n")

# Count complete cases for primary model variables
primary_vars <- c("country", "age", "female", "ethnicity_minority", "education",
                   "employment", "comorbidity", "gen_health",
                   "perceived_risk", "risk_self", "health_trust",
                   "worry_health_fear", "worry_economic", "worry_basic_needs",
                   "worry_social", "worry_safety_stigma",
                   "livelihood_disruption",
                   "z_depression", "z_anxiety", "z_stress", "site")

cc_data <- analytic %>% drop_na(all_of(primary_vars))
cat("Complete cases for primary model:", nrow(cc_data), "of", nrow(analytic), "\n")

for (i in seq_along(outcomes)) {
  model <- lm_robust(as.formula(paste(outcomes[i], f_primary)),
                     data = cc_data, clusters = site, se_type = "CR2")
  res <- tidy(model) %>%
    mutate(outcome = outcome_labels[i], sensitivity = "Complete case")
  all_sensitivity[[paste("cc", outcome_labels[i])]] <- res
}

# ── 6. Compile Comparison Table ────────────────────────────────────────────

cat("\n=== COMPILING SENSITIVITY COMPARISON ===\n")

# Combine all sensitivity results
sens_all <- bind_rows(all_sensitivity)

# Focus on key predictors for comparison
key_terms <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
               "worry_social", "worry_safety_stigma", "worry_total",
               "worry_social_excl", "worry_stigma_excl",
               "perceived_risk", "livelihood_disruption", "comorbidity")

sens_comparison <- sens_all %>%
  filter(term %in% key_terms) %>%
  select(sensitivity, outcome, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 4)),
         p.value = round(p.value, 4))

write_csv(sens_comparison, file.path(tab_dir, "TableS3_Sensitivity_Comparison.csv"))
write_csv(sens_all, file.path(tab_dir, "TableS3_Sensitivity_Full.csv"))

# Print summary
cat("\nKey coefficient comparison (worry sub-domains → Depression):\n")
sens_comparison %>%
  filter(outcome == "Depression",
         term %in% c("worry_basic_needs", "worry_safety_stigma",
                      "worry_total", "worry_stigma_excl")) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n=== 06_sensitivity.R COMPLETE ===\n")
