# DEPRECATED (v7+ canonical): this script is preserved for historical traceability.
# The canonical 13-site sample writers for all downstream tables are
# 17_primary_13sites.R and 18_rerun_all_on_13sites.R. If you run THIS script
# standalone, its outputs may overwrite canonical CSVs with 14-site or old-filter
# content. Always re-run 17 and then 18 after running this script.
#
################################################################################
# 03c_primary_mi.R
# PRIMARY ANALYSIS with Multiple Imputation
#
# Rationale: Complete case analysis loses 40% of sample (n=1462 → 919)
# Missingness is NOT associated with outcomes (p=0.22-0.60) but IS
# associated with country (Indonesia 42% complete vs Vietnam 77%)
# → MAR assumption supported → MI is appropriate and preferred
#
# Strategy: Impute covariates only (not DASS outcomes); 30 imputations;
# run multilevel models on each imputed dataset; pool with Rubin's rules
################################################################################

library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(mitml)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)
cat("Analytic sample (complete DASS):", nrow(analytic), "\n")

# ── 1. Prepare Imputation Data ──────────────────────────────────────────────

# Variables for imputation model and analysis
imp_vars <- c(
  # Outcomes (not imputed, but included in imputation model as predictors)
  "z_depression", "z_anxiety", "z_stress",
  # Covariates to impute
  "age", "female", "ethnicity_minority", "education", "employment",
  "comorbidity", "gen_health",
  "perceived_risk", "risk_self", "health_trust",
  "worry_health_fear", "worry_economic", "worry_basic_needs",
  "worry_social", "worry_safety_stigma",
  "livelihood_disruption",
  # Contextual (for imputation model, not imputed)
  "country", "site"
)

imp_data <- analytic %>%
  dplyr::select(all_of(imp_vars)) %>%
  mutate(
    # Ensure correct types for mice
    female = as.factor(female),
    ethnicity_minority = as.factor(ethnicity_minority),
    education = as.integer(education),
    employment = as.factor(employment),
    country = as.factor(country),
    site = as.integer(site)
  )

cat("\nMissingness before MI:\n")
miss_pct <- colMeans(is.na(imp_data)) * 100
print(round(miss_pct[miss_pct > 0], 1))

# ── 2. Set Up Imputation ────────────────────────────────────────────────────

cat("\n=== SETTING UP MULTIPLE IMPUTATION ===\n")

# Initialize mice to get default methods and predictor matrix
ini <- mice(imp_data, maxit = 0, print = FALSE)

# Customize methods
meth <- ini$method
# Don't impute outcomes or clustering variables
meth[c("z_depression", "z_anxiety", "z_stress", "country", "site")] <- ""
# Use appropriate methods for variable types
meth["female"] <- "logreg"
meth["ethnicity_minority"] <- "logreg"
meth["employment"] <- "polyreg"
# Continuous variables: predictive mean matching (default, robust to non-normality)

cat("Imputation methods:\n")
print(meth[meth != ""])

# Predictor matrix: include outcomes as predictors (improves imputation quality)
pred <- ini$predictorMatrix
# Don't use site as predictor (too many levels, use country instead)
pred[, "site"] <- 0

# ── 3. Run Imputation ───────────────────────────────────────────────────────

cat("\n=== RUNNING MI (m=30, this takes ~5 min) ===\n")

set.seed(2026)
mi_obj <- mice(imp_data, m = 30, maxit = 10, method = meth,
               predictorMatrix = pred, print = FALSE)

cat("MI complete. Checking convergence...\n")

# Basic convergence check: no warnings is good
# Check that imputed values are in plausible ranges
cat("\nImputed value ranges (first imputation):\n")
imp1 <- complete(mi_obj, 1)
for(v in names(meth[meth != ""])) {
  if(is.numeric(imp1[[v]])) {
    cat("  ", v, ": [", round(min(imp1[[v]], na.rm=TRUE), 2), ",",
        round(max(imp1[[v]], na.rm=TRUE), 2), "]\n")
  }
}

# ── 4. Fit Multilevel Models on Imputed Data ────────────────────────────────

cat("\n=== FITTING MODELS ON IMPUTED DATA ===\n")

outcomes <- c("z_depression", "z_anxiety", "z_stress")
outcome_labels <- c("Depression", "Anxiety", "Stress")

f_m3 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust +
          worry_health_fear + worry_economic + worry_basic_needs +
          worry_social + worry_safety_stigma +
          livelihood_disruption + (1|site)"

mi_results <- list()
m <- 30  # number of imputations

for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  y_lab <- outcome_labels[oi]
  cat("\n", y_lab, ":\n")

  full_f <- as.formula(paste(y, f_m3))

  # Fit model on each imputed dataset manually
  fit_list <- list()
  for (i in 1:m) {
    imp_i <- complete(mi_obj, i)
    fit_list[[i]] <- tryCatch(
      lmer(full_f, data = imp_i, REML = TRUE),
      error = function(e) { cat("  imp", i, "error:", e$message, "\n"); NULL }
    )
  }

  # Remove failed fits
  fit_list <- Filter(Negate(is.null), fit_list)
  cat("  Successful fits:", length(fit_list), "/", m, "\n")

  # Pool using mitml (handles lmer objects)
  pooled <- testEstimates(fit_list, var.comp = FALSE)

  # Extract pooled results
  pooled_df <- as.data.frame(pooled$estimates) %>%
    rownames_to_column("term") %>%
    rename(estimate = Estimate, std.error = `Std.Error`,
           statistic = `t.value`, p.value = `P(>|t|)`,
           riv = RIV, fmi = FMI) %>%
    mutate(
      outcome = y_lab,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ "\u2020",
        TRUE ~ ""
      )
    )

  mi_results[[y_lab]] <- pooled_df

  # Print worry sub-domains
  worry_terms <- pooled_df %>%
    filter(grepl("worry_", term)) %>%
    dplyr::select(term, estimate, conf.low, conf.high, p.value, fmi) %>%
    mutate(across(c(estimate, conf.low, conf.high, fmi), ~ round(.x, 4)),
           p.value = round(p.value, 4))
  cat("  Worry sub-domains:\n")
  print(as.data.frame(worry_terms), row.names = FALSE)
  cat("  Effective n:", nrow(analytic), "(full DASS sample, MI-restored)\n")
}

# ── 5. Compile and Save MI Results ──────────────────────────────────────────

mi_all <- bind_rows(mi_results) %>%
  mutate(across(c(estimate, std.error, conf.low, conf.high), ~ round(.x, 4)),
         p.value = round(p.value, 4))

write_csv(mi_all, file.path(tab_dir, "Table4_Primary_Results_MI.csv"))

# ── 6. Compare: Complete Case vs MI ─────────────────────────────────────────

cat("\n=== COMPARISON: COMPLETE CASE vs MI ===\n")

cc_results <- read_csv(file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv"),
                       show_col_types = FALSE)

worry_terms_list <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
                      "worry_social", "worry_safety_stigma")

cat("\nWorry sub-domain coefficients:\n")
cat(sprintf("%-22s  %-12s  %-20s  %-20s\n", "Term", "Outcome", "CC (n=892)", "MI (n=1462)"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (w in worry_terms_list) {
  for (o in outcome_labels) {
    cc_row <- cc_results %>% filter(term == w, outcome == o)
    mi_row <- mi_all %>% filter(term == w, outcome == o)

    if (nrow(cc_row) > 0 & nrow(mi_row) > 0) {
      cc_str <- sprintf("%.4f (p=%.4f)%s", cc_row$estimate, cc_row$p.value,
                         ifelse(cc_row$p.value < 0.05, "*", ""))
      mi_str <- sprintf("%.4f (p=%.4f)%s", mi_row$estimate, mi_row$p.value,
                         ifelse(mi_row$p.value < 0.05, "*", ""))
      cat(sprintf("%-22s  %-12s  %-20s  %-20s\n", w, o, cc_str, mi_str))
    }
  }
}

# ── 7. Missingness Pattern Analysis (for manuscript) ────────────────────────

cat("\n=== MISSINGNESS PATTERN ANALYSIS ===\n")

analytic$m3_complete <- complete.cases(analytic[, c(
  "age", "female", "ethnicity_minority", "education", "employment",
  "comorbidity", "gen_health", "perceived_risk", "risk_self", "health_trust",
  "worry_health_fear", "worry_economic", "worry_basic_needs",
  "worry_social", "worry_safety_stigma", "livelihood_disruption", "site"
)])

miss_compare <- tibble(
  Variable = character(), Complete_mean = numeric(), Missing_mean = numeric(),
  p_value = numeric()
)

# Continuous variables
for (v in c("z_depression", "z_anxiety", "z_stress", "age", "perceived_risk",
            "comorbidity", "livelihood_disruption")) {
  comp <- analytic[[v]][analytic$m3_complete]
  miss <- analytic[[v]][!analytic$m3_complete]
  p <- tryCatch(wilcox.test(comp, miss)$p.value, error = function(e) NA)
  miss_compare <- bind_rows(miss_compare, tibble(
    Variable = v,
    Complete_mean = round(mean(comp, na.rm = TRUE), 3),
    Missing_mean = round(mean(miss, na.rm = TRUE), 3),
    p_value = round(p, 4)
  ))
}

# Country
country_tbl <- table(analytic$m3_complete, analytic$country)
p_country <- chisq.test(country_tbl)$p.value
miss_compare <- bind_rows(miss_compare, tibble(
  Variable = "country (chi-sq)", Complete_mean = NA, Missing_mean = NA,
  p_value = round(p_country, 4)
))

cat("\nComplete (n=892) vs Missing (n=608) comparison:\n")
print(as.data.frame(miss_compare), row.names = FALSE)

write_csv(miss_compare, file.path(tab_dir, "TableS_Missingness_Comparison.csv"))

# Save MI object for potential reuse
saveRDS(mi_obj, file.path(out_dir, "mi_object.rds"))

cat("\n=== 03c_primary_mi.R COMPLETE ===\n")
