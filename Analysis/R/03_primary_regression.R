################################################################################
# 03_primary_regression.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities
#
# Purpose: Sequential regression analysis with cluster-robust SEs
#          Following the Stress Process Model layers
# Input:   spear_analysis_augmented.rds
# Output:  Table 4 (main results), country-stratified results
################################################################################

# ── 0. Setup ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(estimatr)
library(broom)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)
cat("Analytic sample:", nrow(analytic), "\n")

# ── 1. Define Model Formulas (Sequential) ───────────────────────────────────

# Common outcomes
outcomes <- c("z_depression", "z_anxiety", "z_stress")
outcome_labels <- c("Depression", "Anxiety", "Stress")

# M0: Country only (baseline)
f_m0 <- "~ country"

# M1: + Layer 1 (Structural vulnerability)
f_m1 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health"

# M2: + Layer 2 (Shock appraisal)
f_m2 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust"

# M3: + Layer 3 (Stressor proliferation / worry sub-domains)
f_m3 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust +
          worry_health_fear + worry_economic + worry_basic_needs +
          worry_social + worry_safety_stigma +
          livelihood_disruption"

# M4: + Layer 4 (Coping & behavioral adaptation) — FULL MODEL
f_m4 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust +
          worry_health_fear + worry_economic + worry_basic_needs +
          worry_social + worry_safety_stigma +
          livelihood_disruption +
          compliance + prevention_evidence + prevention_folk +
          support_score"

formulas <- list(M0 = f_m0, M1 = f_m1, M2 = f_m2, M3 = f_m3, M4 = f_m4)

# ── 2. Fit Sequential Models with Cluster-Robust SEs ────────────────────────

# Function to fit one model
fit_cluster_model <- function(outcome, formula_rhs, data, cluster_var = "site") {
  full_formula <- as.formula(paste(outcome, formula_rhs))
  lm_robust(full_formula, data = data, clusters = get(cluster_var),
            se_type = "CR2")
}

# Function to extract key results
extract_results <- function(model, model_name, outcome_name) {
  tidy_res <- tidy(model) %>%
    mutate(model = model_name, outcome = outcome_name)

  # R-squared
  r2 <- glance(model)

  list(
    coefficients = tidy_res,
    r2_adj = r2$adj.r.squared,
    nobs = r2$nobs
  )
}

# Fit all models
all_results <- list()
r2_table <- tibble()

for (out_idx in seq_along(outcomes)) {
  outcome <- outcomes[out_idx]
  outcome_label <- outcome_labels[out_idx]
  cat("\n=== Fitting models for", outcome_label, "===\n")

  for (model_name in names(formulas)) {
    cat("  ", model_name, "... ")

    model <- tryCatch(
      fit_cluster_model(outcome, formulas[[model_name]], analytic),
      error = function(e) {
        cat("ERROR:", e$message, "\n")
        return(NULL)
      }
    )

    if (!is.null(model)) {
      res <- extract_results(model, model_name, outcome_label)
      all_results[[paste(outcome_label, model_name)]] <- res

      r2_table <- bind_rows(r2_table, tibble(
        outcome = outcome_label,
        model = model_name,
        adj_r2 = round(res$r2_adj, 4),
        n = res$nobs
      ))
      cat("done (adj R² =", round(res$r2_adj, 4), ", n =", res$nobs, ")\n")
    }
  }
}

# ── 3. R-squared Decomposition Table ────────────────────────────────────────

cat("\n=== R-SQUARED DECOMPOSITION ===\n")

r2_wide <- r2_table %>%
  pivot_wider(names_from = outcome, values_from = c(adj_r2, n)) %>%
  mutate(
    delta_Depression = adj_r2_Depression - lag(adj_r2_Depression, default = 0),
    delta_Anxiety    = adj_r2_Anxiety - lag(adj_r2_Anxiety, default = 0),
    delta_Stress     = adj_r2_Stress - lag(adj_r2_Stress, default = 0)
  )

cat("\nR² progression:\n")
print(r2_wide %>% select(model, starts_with("adj_r2"), starts_with("delta")))

write_csv(r2_wide, file.path(tab_dir, "R2_decomposition.csv"))

# ── 4. Main Results Table (Full Model M4) ───────────────────────────────────

cat("\n=== FULL MODEL (M4) RESULTS ===\n")

# Extract M4 coefficients for all outcomes
m4_results <- map_dfr(outcome_labels, function(ol) {
  res <- all_results[[paste(ol, "M4")]]
  if (!is.null(res)) {
    res$coefficients %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate, std.error, p.value, conf.low, conf.high, outcome) %>%
      mutate(
        across(c(estimate, std.error, conf.low, conf.high), ~ round(.x, 4)),
        p.value = round(p.value, 4),
        sig = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ ""
        )
      )
  }
})

# Print key predictors
cat("\nKey predictors (M4):\n")
key_predictors <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
                     "worry_social", "worry_safety_stigma",
                     "perceived_risk", "livelihood_disruption",
                     "compliance", "prevention_evidence", "prevention_folk",
                     "health_trust", "support_score",
                     "age", "female", "comorbidity")

m4_key <- m4_results %>%
  filter(term %in% key_predictors) %>%
  select(outcome, term, estimate, conf.low, conf.high, p.value, sig) %>%
  arrange(term, outcome) %>%
  as.data.frame()
print(m4_key, row.names = FALSE)

write_csv(m4_results, file.path(tab_dir, "Table4_Main_Results_M4.csv"))

# NOTE: M4 has reduced sample due to missingness in compliance/prevention
# Also extract M3 as primary model (larger sample, includes worry decomposition)
cat("\n=== PRIMARY MODEL (M3) RESULTS ===\n")
cat("(M3 is primary because M4 loses ~40% of sample due to coping variable missingness)\n\n")

m3_results <- map_dfr(outcome_labels, function(ol) {
  res <- all_results[[paste(ol, "M3")]]
  if (!is.null(res)) {
    res$coefficients %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate, std.error, p.value, conf.low, conf.high, outcome) %>%
      mutate(
        across(c(estimate, std.error, conf.low, conf.high), ~ round(.x, 4)),
        p.value = round(p.value, 4),
        sig = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ ""
        )
      )
  }
})

m3_key <- m3_results %>%
  filter(term %in% c("worry_health_fear", "worry_economic", "worry_basic_needs",
                      "worry_social", "worry_safety_stigma",
                      "perceived_risk", "livelihood_disruption",
                      "health_trust", "risk_self",
                      "age", "female", "comorbidity")) %>%
  select(outcome, term, estimate, conf.low, conf.high, p.value, sig) %>%
  arrange(term, outcome) %>%
  as.data.frame()
print(m3_key, row.names = FALSE)

write_csv(m3_results, file.path(tab_dir, "Table4_Primary_Results_M3.csv"))

# ── 5. All Models Comparison Table ──────────────────────────────────────────

# Combine M1-M4 for publication table
all_coef <- map_dfr(names(all_results), function(key) {
  res <- all_results[[key]]
  if (!is.null(res)) res$coefficients
})

write_csv(all_coef, file.path(tab_dir, "all_model_coefficients.csv"))

# ── 6. Country-Stratified Models (M4) ──────────────────────────────────────

cat("\n=== COUNTRY-STRATIFIED MODELS ===\n")

# Formula without country
f_stratified <- "~ age + female + ethnicity_minority + education +
                  employment + comorbidity + gen_health +
                  perceived_risk + risk_self + health_trust +
                  worry_health_fear + worry_economic + worry_basic_needs +
                  worry_social + worry_safety_stigma +
                  livelihood_disruption +
                  compliance + prevention_evidence + prevention_folk +
                  support_score"

stratified_results <- list()

for (cntry in c("Indonesia", "Nepal", "Vietnam")) {
  cat("\n", cntry, ":\n")
  country_data <- analytic %>% filter(country == cntry)

  for (out_idx in seq_along(outcomes)) {
    outcome <- outcomes[out_idx]
    outcome_label <- outcome_labels[out_idx]

    model <- tryCatch(
      fit_cluster_model(outcome, f_stratified, country_data),
      error = function(e) {
        cat("  ", outcome_label, "- ERROR:", e$message, "\n")
        # Fallback to regular OLS if clustering fails (too few clusters)
        full_formula <- as.formula(paste(outcome, f_stratified))
        lm(full_formula, data = country_data)
      }
    )

    res <- tidy(model) %>%
      mutate(country = cntry, outcome = outcome_label,
             across(c(estimate, std.error), ~ round(.x, 4)))

    stratified_results[[paste(cntry, outcome_label)]] <- res
    r2 <- tryCatch(summary(model)$adj.r.squared,
                    error = function(e) glance(model)$adj.r.squared)
    cat("  ", outcome_label, "adj R² =", round(r2, 4),
        "n =", nrow(model.frame(model)), "\n")
  }
}

stratified_df <- bind_rows(stratified_results)
write_csv(stratified_df, file.path(tab_dir, "Table6_Country_Stratified.csv"))

# Print key predictors by country
cat("\n--- Key Worry Sub-Domains by Country ---\n")
strat_key <- stratified_df %>%
  filter(term %in% c("worry_health_fear", "worry_economic", "worry_basic_needs",
                      "worry_social", "worry_safety_stigma")) %>%
  dplyr::select(country, outcome, term, estimate, p.value) %>%
  mutate(estimate = round(estimate, 4), p.value = round(p.value, 4)) %>%
  arrange(term, outcome, country) %>%
  as.data.frame()
print(strat_key, row.names = FALSE)

# ── 7. Save Full Model Objects for Figures ──────────────────────────────────

# Re-fit M4 models and save for figure generation
m4_models <- list()
for (out_idx in seq_along(outcomes)) {
  outcome <- outcomes[out_idx]
  m4_models[[outcome_labels[out_idx]]] <- fit_cluster_model(
    outcome, formulas[["M4"]], analytic
  )
}
saveRDS(m4_models, file.path(out_dir, "m4_models.rds"))

# Save coefficient table formatted for forest plot
forest_data <- m4_results %>%
  filter(term %in% key_predictors) %>%
  mutate(
    term_label = recode(term,
      worry_health_fear = "Worry: Health fears",
      worry_economic = "Worry: Economic concerns",
      worry_basic_needs = "Worry: Basic needs insecurity",
      worry_social = "Worry: Social/family disruption",
      worry_safety_stigma = "Worry: Safety/stigma concerns",
      perceived_risk = "Perceived health threat",
      livelihood_disruption = "Livelihood disruption",
      compliance = "Behavioral compliance",
      prevention_evidence = "Evidence-based prevention",
      prevention_folk = "Folk/alternative prevention",
      health_trust = "Health system trust",
      support_score = "Social support",
      age = "Age (years)",
      female = "Female gender",
      comorbidity = "Comorbidity count"
    ),
    layer = case_when(
      term %in% c("age", "female", "comorbidity") ~ "Layer 1: Vulnerability",
      term %in% c("perceived_risk", "health_trust") ~ "Layer 2: Appraisal",
      grepl("worry_|livelihood", term) ~ "Layer 3: Stressor proliferation",
      TRUE ~ "Layer 4: Coping"
    )
  )

saveRDS(forest_data, file.path(out_dir, "forest_plot_data.rds"))

# ── 8. Extended Model: Additional Covariates ────────────────────────────────

cat("\n=== EXTENDED MODEL: M3+ (with location, marital, media_trust) ===\n")

f_m3plus <- "~ country + age + female + ethnicity_minority + education +
              employment + comorbidity + gen_health + location_type + marital +
              perceived_risk + risk_self + health_trust + media_trust +
              worry_health_fear + worry_economic + worry_basic_needs +
              worry_social + worry_safety_stigma +
              livelihood_disruption + missed_healthcare"

for (i in seq_along(outcomes)) {
  model <- tryCatch(
    fit_cluster_model(outcomes[i], f_m3plus, analytic),
    error = function(e) { cat("Error:", e$message, "\n"); NULL }
  )
  if (!is.null(model)) {
    cat(outcome_labels[i], ": adj R² =", round(glance(model)$adj.r.squared, 4),
        "n =", glance(model)$nobs, "\n")
    # Save key new predictors
    tidy(model) %>%
      filter(term %in% c("location_typePeri-urban", "location_typeRural",
                          "location_typeRemote", "media_trust",
                          "missed_healthcare", "maritalWidowed/divorced/separated",
                          "maritalSingle/unmarried")) %>%
      mutate(outcome = outcome_labels[i]) %>%
      dplyr::select(outcome, term, estimate, p.value) %>%
      mutate(across(c(estimate, p.value), ~ round(.x, 4))) %>%
      as.data.frame() %>% print(row.names = FALSE)
  }
}

# ── 9. Interaction Analyses ─────────────────────────────────────────────────

cat("\n=== INTERACTION ANALYSES ===\n")

interaction_results <- list()

# 9a. Health trust × worry sub-domains → does trust buffer worry effects?
cat("\n--- Health trust × Worry interactions ---\n")
for (i in seq_along(outcomes)) {
  f_int <- paste(outcomes[i], "~ country + age + female + ethnicity_minority +
    education + employment + comorbidity + gen_health +
    perceived_risk + risk_self + livelihood_disruption +
    worry_basic_needs * health_trust +
    worry_safety_stigma * health_trust +
    worry_health_fear + worry_economic + worry_social")

  model <- tryCatch(
    lm_robust(as.formula(f_int), data = analytic, clusters = site, se_type = "CR2"),
    error = function(e) NULL
  )
  if (!is.null(model)) {
    int_terms <- tidy(model) %>%
      filter(grepl(":", term)) %>%
      mutate(outcome = outcome_labels[i])
    interaction_results[[paste("trust", outcome_labels[i])]] <- int_terms
    if (nrow(int_terms) > 0) {
      cat(outcome_labels[i], ":\n")
      int_terms %>% dplyr::select(term, estimate, p.value) %>%
        mutate(across(everything(), ~ round(as.numeric(.x), 4))) %>%
        as.data.frame() %>% print(row.names = FALSE)
    }
  }
}

# 9b. Country × worry interactions (from v2, now with sub-domains)
cat("\n--- Country × Worry sub-domain interactions ---\n")
for (i in seq_along(outcomes)) {
  f_int2 <- paste(outcomes[i], "~ age + female + ethnicity_minority +
    education + employment + comorbidity + gen_health +
    perceived_risk + risk_self + health_trust + livelihood_disruption +
    worry_basic_needs * country +
    worry_safety_stigma * country +
    worry_health_fear + worry_economic + worry_social")

  model <- tryCatch(
    lm_robust(as.formula(f_int2), data = analytic, clusters = site, se_type = "CR2"),
    error = function(e) NULL
  )
  if (!is.null(model)) {
    int_terms <- tidy(model) %>%
      filter(grepl(":", term)) %>%
      mutate(outcome = outcome_labels[i])
    interaction_results[[paste("country", outcome_labels[i])]] <- int_terms
    if (any(int_terms$p.value < 0.05)) {
      cat(outcome_labels[i], "- SIGNIFICANT interactions:\n")
      int_terms %>% filter(p.value < 0.05) %>%
        dplyr::select(term, estimate, p.value) %>%
        mutate(across(everything(), ~ round(as.numeric(.x), 4))) %>%
        as.data.frame() %>% print(row.names = FALSE)
    }
  }
}

int_df <- bind_rows(interaction_results)
write_csv(int_df, file.path(tab_dir, "Table_Interaction_Results.csv"))

# ── 10. Non-Linearity Check ────────────────────────────────────────────────

cat("\n=== NON-LINEARITY CHECK (quadratic terms) ===\n")

for (i in seq_along(outcomes)) {
  f_quad <- paste(outcomes[i], "~ country + age + female + ethnicity_minority +
    education + employment + comorbidity + gen_health +
    perceived_risk + I(perceived_risk^2) +
    worry_basic_needs + I(worry_basic_needs^2) +
    worry_safety_stigma + I(worry_safety_stigma^2) +
    risk_self + health_trust +
    worry_health_fear + worry_economic + worry_social +
    livelihood_disruption")

  model <- tryCatch(
    lm_robust(as.formula(f_quad), data = analytic, clusters = site, se_type = "CR2"),
    error = function(e) NULL
  )
  if (!is.null(model)) {
    quad_terms <- tidy(model) %>% filter(grepl("\\^2", term))
    if (any(quad_terms$p.value < 0.05)) {
      cat(outcome_labels[i], "- significant quadratic terms:\n")
      quad_terms %>% dplyr::select(term, estimate, p.value) %>%
        mutate(across(everything(), ~ round(as.numeric(.x), 4))) %>%
        as.data.frame() %>% print(row.names = FALSE)
    } else {
      cat(outcome_labels[i], "- no significant non-linear effects\n")
    }
  }
}

# ── 11. Suicidality Supplementary Analysis ──────────────────────────────────

cat("\n=== SUPPLEMENTARY: SUICIDALITY ANALYSIS ===\n")

f_suicidal <- "~ country + age + female + ethnicity_minority + education +
                employment + comorbidity + gen_health +
                perceived_risk + risk_self + health_trust +
                worry_health_fear + worry_economic + worry_basic_needs +
                worry_social + worry_safety_stigma +
                livelihood_disruption"

suic_data <- analytic %>% filter(!is.na(any_suicidality))
cat("Suicidality data: n =", nrow(suic_data),
    "prevalence =", round(mean(suic_data$any_suicidality, na.rm = TRUE) * 100, 1), "%\n")

if (sum(suic_data$any_suicidality, na.rm = TRUE) >= 20) {
  suic_model <- tryCatch(
    glm(as.formula(paste("any_suicidality", f_suicidal)),
        data = suic_data, family = binomial),
    error = function(e) { cat("Error:", e$message, "\n"); NULL }
  )

  if (!is.null(suic_model)) {
    suic_results <- tidy(suic_model, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 3)),
             p.value = round(p.value, 4))

    cat("\nSignificant predictors of suicidality (OR):\n")
    suic_results %>%
      filter(p.value < 0.05) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      as.data.frame() %>% print(row.names = FALSE)

    write_csv(suic_results, file.path(tab_dir, "TableS_Suicidality.csv"))
  }
} else {
  cat("Too few suicidality cases for regression.\n")
}

# ── 12. Model Diagnostics ───────────────────────────────────────────────────

cat("\n=== MODEL DIAGNOSTICS (M3) ===\n")

# Re-fit M3 with standard lm for diagnostics
for (i in seq_along(outcomes)) {
  diag_model <- lm(as.formula(paste(outcomes[i], formulas[["M3"]])), data = analytic)

  # Cook's distance
  cooksd <- cooks.distance(diag_model)
  n_influential <- sum(cooksd > 4 / nrow(model.frame(diag_model)), na.rm = TRUE)
  cat(outcome_labels[i], ": influential observations (Cook's D > 4/n):", n_influential, "\n")

  # Shapiro-Wilk on residuals (subsample for large n)
  resids <- residuals(diag_model)
  sw <- shapiro.test(sample(resids, min(5000, length(resids))))
  cat("  Shapiro-Wilk p =", round(sw$p.value, 4),
      ifelse(sw$p.value < 0.05, "(non-normal — expected with large n)", "(normal)"), "\n")
}

cat("\n=== 03_primary_regression.R COMPLETE ===\n")
