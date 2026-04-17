# DEPRECATED (v7+ canonical): this script is preserved for historical traceability.
# The canonical 13-site sample writers for all downstream tables are
# 17_primary_13sites.R and 18_rerun_all_on_13sites.R. If you run THIS script
# standalone, its outputs may overwrite canonical CSVs with 14-site or old-filter
# content. Always re-run 17 and then 18 after running this script.
#
################################################################################
# 04_mediation.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities
#
# Purpose: Test mediation hypotheses using bootstrap CIs
# Note: mediate() has scoping issues with formulas in functions,
#       so each mediation is run inline at top-level scope.
################################################################################

library(tidyverse)
library(mediation)
# mediation loads MASS which masks dplyr::select
select <- dplyr::select

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)

set.seed(42)
N_SIMS <- 1000

covs <- c("age", "female", "ethnicity_minority", "education",
          "comorbidity", "gen_health", "country")

mediation_results <- list()

# Helper to extract results from a mediate() object
extract_med <- function(med, label) {
  tibble(
    label = label,
    acme = round(med$d0, 4),
    acme_ci_low = round(med$d0.ci[1], 4),
    acme_ci_high = round(med$d0.ci[2], 4),
    acme_p = round(med$d0.p, 4),
    ade = round(med$z0, 4),
    ade_ci_low = round(med$z0.ci[1], 4),
    ade_ci_high = round(med$z0.ci[2], 4),
    ade_p = round(med$z0.p, 4),
    total = round(med$tau.coef, 4),
    total_p = round(med$tau.p, 4),
    prop_mediated = round(med$n0, 4),
    prop_mediated_p = round(med$n0.p, 4)
  )
}

# ── Med 1: Livelihood → Economic Worry → Depression ─────────────────────────
cat("\n--- Med 1: Livelihood → Economic Worry → Depression ---\n")
d1 <- analytic %>%
  select(livelihood_disruption, worry_economic, z_depression, all_of(covs)) %>%
  drop_na()
cat("n =", nrow(d1), "\n")

m1_med <- lm(worry_economic ~ livelihood_disruption + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d1)
m1_out <- lm(z_depression ~ livelihood_disruption + worry_economic + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d1)
med1 <- mediate(m1_med, m1_out, treat = "livelihood_disruption",
                mediator = "worry_economic", boot = TRUE, sims = N_SIMS)
summary(med1)
mediation_results[["med1"]] <- extract_med(med1, "Livelihood → Econ Worry → Depression")

# ── Med 2: Livelihood → Basic Needs → Depression ────────────────────────────
cat("\n--- Med 2: Livelihood → Basic Needs → Depression ---\n")
d2 <- analytic %>%
  select(livelihood_disruption, worry_basic_needs, z_depression, all_of(covs)) %>%
  drop_na()
cat("n =", nrow(d2), "\n")

m2_med <- lm(worry_basic_needs ~ livelihood_disruption + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d2)
m2_out <- lm(z_depression ~ livelihood_disruption + worry_basic_needs + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d2)
med2 <- mediate(m2_med, m2_out, treat = "livelihood_disruption",
                mediator = "worry_basic_needs", boot = TRUE, sims = N_SIMS)
summary(med2)
mediation_results[["med2"]] <- extract_med(med2, "Livelihood → Basic Needs → Depression")

# ── Med 3: Perceived Risk → Health Fear → Anxiety ───────────────────────────
cat("\n--- Med 3: Perceived Risk → Health Fear → Anxiety ---\n")
d3 <- analytic %>%
  select(perceived_risk, worry_health_fear, z_anxiety, all_of(covs)) %>%
  drop_na()
cat("n =", nrow(d3), "\n")

m3_med <- lm(worry_health_fear ~ perceived_risk + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d3)
m3_out <- lm(z_anxiety ~ perceived_risk + worry_health_fear + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d3)
med3 <- mediate(m3_med, m3_out, treat = "perceived_risk",
                mediator = "worry_health_fear", boot = TRUE, sims = N_SIMS)
summary(med3)
mediation_results[["med3"]] <- extract_med(med3, "Perceived Risk → Health Fear → Anxiety")

# ── Med 4: Perceived Risk → Health Fear → Stress ───────────────────────────
cat("\n--- Med 4: Perceived Risk → Health Fear → Stress ---\n")
d4 <- analytic %>%
  select(perceived_risk, worry_health_fear, z_stress, all_of(covs)) %>%
  drop_na()
cat("n =", nrow(d4), "\n")

m4_med <- lm(worry_health_fear ~ perceived_risk + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d4)
m4_out <- lm(z_stress ~ perceived_risk + worry_health_fear + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d4)
med4 <- mediate(m4_med, m4_out, treat = "perceived_risk",
                mediator = "worry_health_fear", boot = TRUE, sims = N_SIMS)
summary(med4)
mediation_results[["med4"]] <- extract_med(med4, "Perceived Risk → Health Fear → Stress")

# ── Med 5: Perceived Risk → Safety/Stigma → Depression ─────────────────────
cat("\n--- Med 5: Perceived Risk → Safety/Stigma → Depression ---\n")
d5 <- analytic %>%
  select(perceived_risk, worry_safety_stigma, z_depression, all_of(covs)) %>%
  drop_na()
cat("n =", nrow(d5), "\n")

m5_med <- lm(worry_safety_stigma ~ perceived_risk + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d5)
m5_out <- lm(z_depression ~ perceived_risk + worry_safety_stigma + age + female +
               ethnicity_minority + education + comorbidity + gen_health + country,
             data = d5)
med5 <- mediate(m5_med, m5_out, treat = "perceived_risk",
                mediator = "worry_safety_stigma", boot = TRUE, sims = N_SIMS)
summary(med5)
mediation_results[["med5"]] <- extract_med(med5, "Perceived Risk → Safety/Stigma → Depression")

# ── Compile Results ─────────────────────────────────────────────────────────
results_df <- bind_rows(mediation_results)

cat("\n=== MEDIATION RESULTS SUMMARY ===\n")
print(as.data.frame(results_df), row.names = FALSE)

write_csv(results_df, file.path(tab_dir, "Table5_Mediation_Results.csv"))
saveRDS(results_df, file.path(out_dir, "mediation_results.rds"))

cat("\n=== 04_mediation.R COMPLETE ===\n")
