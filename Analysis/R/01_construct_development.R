################################################################################
# 01_construct_development.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities
#
# Purpose: Load raw data, check missingness, decompose worry score,
#          create new constructs, validate with EFA/CFA
# Output:  Augmented analysis dataset + psychometric reports
################################################################################

# ── 0. Setup ─────────────────────────────────────────────────────────────────
library(haven)
library(tidyverse)
library(psych)
library(lavaan)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
data_dir <- file.path(base_dir, "Data")
out_dir  <- file.path(proj_dir, "Analysis", "output")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load Raw Data ─────────────────────────────────────────────────────────
raw <- read_dta(file.path(data_dir, "spear_community.dta"))
cat("Raw data dimensions:", nrow(raw), "x", ncol(raw), "\n")

# ── 1b. Recode Special Values to NA ──────────────────────────────────────────
# Many items use 96/97/98/99 for "don't know"/"prefer not to answer"/"other"
# These must be recoded to NA before creating composite scores

# q22 compliance items: 0-4 Likert, 96 = "don't know" -> NA
compliance_items_raw <- c("q22_1_lockdown", "q22_2_travel", "q22_3_gatherings",
                          "q22_4_work_home", "q22_5_self_isolate", "q22_6_isolate_sick",
                          "q22_7_distance", "q22_8_masks", "q22_9_hands")
raw <- raw %>%
  mutate(across(all_of(compliance_items_raw), ~ ifelse(.x >= 90, NA_real_, .x)))

# q21 govt awareness items: 1-3 scale (1=yes, 2=no, 3=don't know)
# Recode: 1=aware (1), 2=not aware (0), 3=don't know (NA)
govt_items_raw <- c("q21_1_lockdown", "q21_2_travel", "q21_3_gatherings",
                    "q21_4_schools", "q21_5_workplace", "q21_6_shops",
                    "q21_7_public", "q21_8_borders", "q21_9_international",
                    "q21_10_domestic", "q21_11_quarantine", "q21_12_self_isolate",
                    "q21_13_distance", "q21_14_masks")
raw <- raw %>%
  mutate(across(all_of(govt_items_raw), ~ case_when(
    .x == 1 ~ 1L,    # Yes, aware
    .x == 2 ~ 0L,    # No, not aware
    TRUE ~ NA_integer_ # Don't know or missing
  )))

# q23 prevention items: 1-4 Likert (1=yes definitely, 4=no definitely)
# Keep as-is but reverse-code so higher = more prevention
prevention_items_raw <- c("q23_1_pray", "q23_2_herbal", "q23_3_vitaminc",
                          "q23_4_saltwater", "q23_5_drugs", "q23_6_hands",
                          "q23_7_eyes", "q23_8_disinfect", "q23_9_mouth",
                          "q23_10_symptom_advice", "q23_11_hotline", "q23_12_travel",
                          "q23_13_stay_home_sick", "q23_14_stay_home", "q23_15_flu_jab",
                          "q23_16_avoid_hf", "q23_17_avoid_crowds",
                          "q23_18_avoid_transport", "q23_19_avoid_hcw",
                          "q23_20_avoid_eat_out")
# Reverse code: 1->4, 2->3, 3->2, 4->1 (so higher = more engagement)
raw <- raw %>%
  mutate(across(all_of(prevention_items_raw), ~ ifelse(.x >= 1 & .x <= 4, 5L - .x, NA_integer_)))

# q26 lifestyle items: 0-4 Likert, keep as-is (higher = more change)
# No special codes observed

# q28, q29 health trust: 0-4 Likert, 98 = "prefer not to answer" -> NA
raw <- raw %>%
  mutate(across(c(q28_prevention, q29_health_system), ~ ifelse(.x >= 90, NA_real_, .x)))

# q31, q32 self-assessed risk: check for special codes
raw <- raw %>%
  mutate(across(c(q31_risk_self, q32_risk_country), ~ ifelse(.x >= 90, NA_real_, .x)))

cat("Special value recoding complete.\n")

# ── 2. Basic Cleaning & Sample Definition ────────────────────────────────────

# Country variable
d <- raw %>%
  mutate(
    country = case_when(
      correct_DAG == "Indonesia" ~ "Indonesia",
      correct_DAG == "Nepal"     ~ "Nepal",
      correct_DAG == "Vietnam"   ~ "Vietnam",
      TRUE ~ NA_character_
    ),
    country = factor(country, levels = c("Indonesia", "Nepal", "Vietnam"))
  ) %>%
  filter(!is.na(country))

cat("After country filter:", nrow(d), "\n")

# Site ID (for clustering)
d <- d %>%
  mutate(site = as.integer(site_all_countries_n))

# Age variable (already computed in raw data)
d <- d %>%
  mutate(age = as.numeric(q2_age))

# Gender (0=Male, 1=Female from q1_sex_bin)
d <- d %>%
  mutate(
    gender = factor(q1_sex_bin, levels = c(1, 2), labels = c("Male", "Female")),
    female = as.integer(q1_sex_bin == 2)
  )

# Ethnicity (minority = 1)
d <- d %>%
  mutate(ethnicity_minority = as.integer(q4_min_ethnicity))

# Education (ordinal 0-7)
d <- d %>%
  mutate(education = as.integer(q5_education))

# Employment (categorical)
d <- d %>%
  mutate(
    employment = factor(q6_employment_cat,
                        levels = 1:4,
                        labels = c("Full-time", "Part-time/casual",
                                   "Student", "Not working"))
  )

# Marital status
d <- d %>%
  mutate(
    marital = factor(q3_married_3,
                     levels = 1:3,
                     labels = c("Married/cohabiting",
                                "Widowed/divorced/separated",
                                "Single/unmarried"))
  )

# Location type (urban/peri-urban/rural/remote)
d <- d %>%
  mutate(
    location_type = factor(q12_urban,
                           levels = 1:4,
                           labels = c("Urban", "Peri-urban", "Rural", "Remote"))
  )

# ── 3. DASS-21 Outcomes ──────────────────────────────────────────────────────

# Individual DASS items (0-3 scale)
dass_depr_items <- c("q47_3", "q47_5", "q47_10", "q47_13", "q47_16", "q47_17", "q47_21")
dass_anx_items  <- c("q47_2", "q47_4", "q47_7", "q47_9", "q47_15", "q47_19", "q47_20")
dass_stress_items <- c("q47_1", "q47_6", "q47_8", "q47_11", "q47_12", "q47_14", "q47_18")

# Compute subscale scores (sum of 7 items × 2 to match DASS-42 convention)
# Use existing continuous scores if available; recalculate for transparency
d <- d %>%
  rowwise() %>%
  mutate(
    # Count non-missing items per subscale
    depr_n = sum(!is.na(c_across(all_of(dass_depr_items)))),
    anx_n  = sum(!is.na(c_across(all_of(dass_anx_items)))),
    stress_n = sum(!is.na(c_across(all_of(dass_stress_items)))),
    # Sum scores (prorated if >= 6 of 7 items answered)
    depr_sum = ifelse(depr_n >= 6,
                      sum(c_across(all_of(dass_depr_items)), na.rm = TRUE) / depr_n * 7,
                      NA_real_),
    anx_sum  = ifelse(anx_n >= 6,
                      sum(c_across(all_of(dass_anx_items)), na.rm = TRUE) / anx_n * 7,
                      NA_real_),
    stress_sum = ifelse(stress_n >= 6,
                        sum(c_across(all_of(dass_stress_items)), na.rm = TRUE) / stress_n * 7,
                        NA_real_)
  ) %>%
  ungroup() %>%
  # DASS-42 equivalent (multiply by 2)
  mutate(
    depression = depr_sum * 2,
    anxiety    = anx_sum * 2,
    stress     = stress_sum * 2
  )

# Z-standardize for analysis
d <- d %>%
  mutate(
    z_depression = as.numeric(scale(depression)),
    z_anxiety    = as.numeric(scale(anxiety)),
    z_stress     = as.numeric(scale(stress))
  )

# Binary DASS outcomes (for sensitivity)
d <- d %>%
  mutate(
    depr_any  = as.integer(depression >= 10),
    anx_any   = as.integer(anxiety >= 8),
    stress_any = as.integer(stress >= 15)
  )

# Define analytic sample: complete DASS outcomes
d <- d %>%
  mutate(complete_dass = !is.na(z_depression) & !is.na(z_anxiety) & !is.na(z_stress))

cat("Complete DASS outcomes:", sum(d$complete_dass), "of", nrow(d), "\n")

# ── 4. Missingness Check for All Candidate Variables ─────────────────────────

# Variables we want to use (check missingness within analytic sample)
analytic <- d %>% filter(complete_dass)

candidate_vars <- list(
  # Layer 2: Shock appraisal
  perceived_risk = c("q27_1_infected", "q27_2_infect_others", "q27_3_family_infect",
                     "q27_4_severe", "q27_5_quarantine", "q27_6_die"),
  self_risk      = c("q31_risk_self", "q32_risk_country"),
  general_health = c("q33_gen_health", "q34_gen_health_now"),
  govt_awareness = c("q21_1_lockdown", "q21_2_travel", "q21_3_gatherings",
                     "q21_4_schools", "q21_5_workplace", "q21_6_shops",
                     "q21_7_public", "q21_8_borders", "q21_9_international",
                     "q21_10_domestic", "q21_11_quarantine", "q21_12_self_isolate",
                     "q21_13_distance", "q21_14_masks"),
  # Layer 3: Stressor proliferation
  worry_items    = c("q51_1_lonely", "q51_2_quarantine", "q51_3_infected",
                     "q51_4_severe", "q51_5_death", "q51_6_health_system",
                     "q51_7_education", "q51_8_teaching", "q51_9_children_home",
                     "q51_10_income", "q51_11_food", "q51_12_water",
                     "q51_13_medicine", "q51_14_rent", "q51_15_debt",
                     "q51_16_agri", "q51_17_harvest", "q51_18_produce",
                     "q51_19_stigma", "q51_20_abuse", "q51_21_recession"),
  livelihood     = c("q8_1_reduced_hours", "q8_3_less_income",
                     "q8_6_lost_job", "q8_8_quit_job"),
  lifestyle      = c("q26_1_food", "q26_2_disinfect", "q26_3_medicine",
                     "q26_4_supplies", "q26_5_healthy_food", "q26_6_vitamins",
                     "q26_7_sleep", "q26_8_smoking", "q26_9_alcohol",
                     "q26_10_exercise", "q26_11_socialise", "q26_12_family",
                     "q26_13_neighbours", "q26_14_spend_time"),
  # Layer 4: Coping
  compliance     = c("q22_1_lockdown", "q22_2_travel", "q22_3_gatherings",
                     "q22_4_work_home", "q22_5_self_isolate", "q22_6_isolate_sick",
                     "q22_7_distance", "q22_8_masks", "q22_9_hands"),
  prevention     = c("q23_1_pray", "q23_2_herbal", "q23_3_vitaminc",
                     "q23_4_saltwater", "q23_5_drugs", "q23_6_hands",
                     "q23_7_eyes", "q23_8_disinfect", "q23_9_mouth",
                     "q23_10_symptom_advice", "q23_11_hotline", "q23_12_travel",
                     "q23_13_stay_home_sick", "q23_14_stay_home", "q23_15_flu_jab",
                     "q23_16_avoid_hf", "q23_17_avoid_crowds",
                     "q23_18_avoid_transport", "q23_19_avoid_hcw",
                     "q23_20_avoid_eat_out"),
  health_trust   = c("q28_prevention", "q29_health_system"),
  social_support = c("q52___1", "q52___2", "q52___3", "q52___4", "q52___5",
                     "q52___6", "q52___7", "q52___8", "q52___9", "q52___10",
                     "q52___11", "q52___12", "q52___13"),
  community_help = c("q53___1", "q53___2", "q53___3", "q53___4", "q53___5",
                     "q53___6", "q53___7", "q53___8", "q53___9", "q53___10",
                     "q53___11", "q53___12", "q53___13")
)

# Compute missingness rates
miss_report <- map_dfr(candidate_vars, function(vars) {
  tibble(
    variable = vars,
    n_miss   = map_int(vars, ~ sum(is.na(analytic[[.x]]))),
    pct_miss = round(n_miss / nrow(analytic) * 100, 1)
  )
}, .id = "construct")

cat("\n=== MISSINGNESS REPORT ===\n")
miss_summary <- miss_report %>%
  group_by(construct) %>%
  summarise(
    n_items  = n(),
    max_miss = max(pct_miss),
    mean_miss = round(mean(pct_miss), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(max_miss))

print(miss_summary, n = 20)

# Save missingness report
write_csv(miss_report, file.path(tab_dir, "missingness_report.csv"))

# Decision: exclude constructs with max missingness > 30%
usable_constructs <- miss_summary %>% filter(max_miss <= 30) %>% pull(construct)
cat("\nUsable constructs (<=30% missing):", paste(usable_constructs, collapse = ", "), "\n")

# ── 5. Create Layer 1: Structural Vulnerability Variables ────────────────────

d <- d %>%
  mutate(
    # Comorbidity score (already in raw data)
    comorbidity = as.numeric(comorbidity_score),
    # General health (higher = better, 1-5 scale)
    gen_health = as.numeric(q33_gen_health),
    gen_health_now = as.numeric(q34_gen_health_now),
    # Household crowding (already computed)
    crowding = as.numeric(crowding),
    # Income category
    income_cat = as.numeric(q9_income)
  )

# ── 6. Create Layer 2: Shock Appraisal Variables ─────────────────────────────

# Perceived health threat (sum of 6 items, range 0-30)
d <- d %>%
  mutate(perceived_risk = as.numeric(perceived_risk_score))

# Self-assessed risk
d <- d %>%
  mutate(
    risk_self    = as.numeric(q31_risk_self),
    risk_country = as.numeric(q32_risk_country)
  )

# Government response awareness (sum of 14 binary items)
govt_items <- c("q21_1_lockdown", "q21_2_travel", "q21_3_gatherings",
                "q21_4_schools", "q21_5_workplace", "q21_6_shops",
                "q21_7_public", "q21_8_borders", "q21_9_international",
                "q21_10_domestic", "q21_11_quarantine", "q21_12_self_isolate",
                "q21_13_distance", "q21_14_masks")

d <- d %>%
  rowwise() %>%
  mutate(
    govt_awareness = sum(c_across(all_of(govt_items)), na.rm = FALSE)
  ) %>%
  ungroup()

# Health system trust (2 items)
d <- d %>%
  mutate(
    health_trust = as.numeric(q28_prevention) + as.numeric(q29_health_system)
  )

# ── 7. Create Layer 3: Worry Sub-Domains (Theory-Guided) ────────────────────

# Health fears (5 items)
health_fear_items <- c("q51_2_quarantine", "q51_3_infected", "q51_4_severe",
                       "q51_5_death", "q51_6_health_system")

# Economic concerns (7 items)
econ_worry_items <- c("q51_10_income", "q51_14_rent", "q51_15_debt",
                      "q51_16_agri", "q51_17_harvest", "q51_18_produce",
                      "q51_21_recession")

# Basic needs insecurity (3 items)
basic_needs_items <- c("q51_11_food", "q51_12_water", "q51_13_medicine")

# Social/family disruption (4 items)
social_disruption_items <- c("q51_1_lonely", "q51_7_education",
                             "q51_8_teaching", "q51_9_children_home")

# Safety/stigma (2 items)
safety_stigma_items <- c("q51_19_stigma", "q51_20_abuse")

# All worry items for EFA
all_worry_items <- c(health_fear_items, econ_worry_items, basic_needs_items,
                     social_disruption_items, safety_stigma_items)

# Create sub-domain sum scores
d <- d %>%
  rowwise() %>%
  mutate(
    worry_health_fear    = sum(c_across(all_of(health_fear_items)), na.rm = FALSE),
    worry_economic       = sum(c_across(all_of(econ_worry_items)), na.rm = FALSE),
    worry_basic_needs    = sum(c_across(all_of(basic_needs_items)), na.rm = FALSE),
    worry_social         = sum(c_across(all_of(social_disruption_items)), na.rm = FALSE),
    worry_safety_stigma  = sum(c_across(all_of(safety_stigma_items)), na.rm = FALSE),
    # Keep original aggregate for comparison
    worry_total          = sum(c_across(all_of(all_worry_items)), na.rm = FALSE)
  ) %>%
  ungroup()

# Livelihood disruption
# q8 items are 0-3 Likert (not binary): 0=no, 1=a little, 2=somewhat, 3=a lot
# Sum of 4 items: reduced hours, less income, lost job, quit job → range 0-12
# Using pre-computed Stata variable (verified: identical to rowSums of 4 items)
d <- d %>%
  mutate(livelihood_disruption = as.numeric(q8_reduced_income))

# ── 8. Create Layer 3: Lifestyle Disruption (q26) ───────────────────────────

lifestyle_items <- c("q26_1_food", "q26_2_disinfect", "q26_3_medicine",
                     "q26_4_supplies", "q26_5_healthy_food", "q26_6_vitamins",
                     "q26_7_sleep", "q26_8_smoking", "q26_9_alcohol",
                     "q26_10_exercise", "q26_11_socialise", "q26_12_family",
                     "q26_13_neighbours", "q26_14_spend_time")

# Theory-based sub-grouping for lifestyle disruption
lifestyle_access_items  <- c("q26_1_food", "q26_3_medicine", "q26_4_supplies")
lifestyle_health_items  <- c("q26_5_healthy_food", "q26_6_vitamins",
                             "q26_7_sleep", "q26_10_exercise")
lifestyle_social_items  <- c("q26_11_socialise", "q26_12_family",
                             "q26_13_neighbours", "q26_14_spend_time")
lifestyle_substance_items <- c("q26_8_smoking", "q26_9_alcohol")
lifestyle_hygiene_items <- c("q26_2_disinfect")

d <- d %>%
  rowwise() %>%
  mutate(
    lifestyle_access    = sum(c_across(all_of(lifestyle_access_items)), na.rm = FALSE),
    lifestyle_health    = sum(c_across(all_of(lifestyle_health_items)), na.rm = FALSE),
    lifestyle_social    = sum(c_across(all_of(lifestyle_social_items)), na.rm = FALSE),
    lifestyle_total     = sum(c_across(all_of(lifestyle_items)), na.rm = FALSE)
  ) %>%
  ungroup()

# ── 9. Create Layer 4: Coping & Behavioral Adaptation ───────────────────────

# Behavioral compliance (sum of 9 items)
compliance_items <- c("q22_1_lockdown", "q22_2_travel", "q22_3_gatherings",
                      "q22_4_work_home", "q22_5_self_isolate", "q22_6_isolate_sick",
                      "q22_7_distance", "q22_8_masks", "q22_9_hands")

d <- d %>%
  rowwise() %>%
  mutate(
    compliance = sum(c_across(all_of(compliance_items)), na.rm = FALSE)
  ) %>%
  ungroup()

# Preventive behaviors: evidence-based vs folk
prevention_evidence_items <- c("q23_6_hands", "q23_7_eyes", "q23_8_disinfect",
                               "q23_9_mouth", "q23_10_symptom_advice",
                               "q23_12_travel", "q23_13_stay_home_sick",
                               "q23_14_stay_home", "q23_17_avoid_crowds",
                               "q23_18_avoid_transport")

prevention_folk_items <- c("q23_1_pray", "q23_2_herbal", "q23_3_vitaminc",
                           "q23_4_saltwater", "q23_5_drugs", "q23_15_flu_jab")

d <- d %>%
  rowwise() %>%
  mutate(
    prevention_evidence = sum(c_across(all_of(prevention_evidence_items)), na.rm = FALSE),
    prevention_folk     = sum(c_across(all_of(prevention_folk_items)), na.rm = FALSE)
  ) %>%
  ungroup()

# Social support (site-level aggregates already exist)
d <- d %>%
  mutate(
    supportive_community = as.numeric(supportive_community),
    helpful_community    = as.numeric(helpful_community),
    # Individual-level support score
    support_score = as.numeric(q52_support_score)
  )

# Media trust
d <- d %>%
  mutate(media_trust = as.numeric(q30_media))

# ── 9b. Additional Variables (previously underutilized) ─────────────────────

# Occupation (categorical, from pre-computed Stata variable)
d <- d %>%
  mutate(
    occupation = factor(q7_occupation_cat,
                        levels = 1:12,
                        labels = c("Farmer", "Manual/factory", "Office", "Shop/retail",
                                   "Hospitality", "Driver", "Healthcare", "Teacher",
                                   "Student", "Stay-at-home", "Unemployed", "Other"))
  )

# Healthcare access disruption (q44: missed appointments due to pandemic)
d <- d %>%
  mutate(missed_healthcare = as.integer(q44_missed_app == 1))  # 1=yes missed

# Household size (total members)
d <- d %>%
  mutate(household_size = as.numeric(hhmembers_tot))

# Suicidality indicators (for supplementary analysis)
# q48: suicidal thoughts (past 2 weeks), q49: plan, q50: attempt
d <- d %>%
  mutate(
    suicidal_thought = as.integer(q48_suicide_thought == 1),
    suicidal_plan    = as.integer(q49_suicide_plan == 1),
    suicidal_attempt = as.integer(q50_suicide_attempt == 1),
    any_suicidality  = as.integer(suicidal_thought == 1 | suicidal_plan == 1 |
                                    suicidal_attempt == 1)
  )

# Income per capita (from pre-computed Stata variable)
d <- d %>%
  mutate(income_per_capita = as.numeric(income_per_capita))

# Worry: Material hardship (merged BNEEDS + ECON for SEM robustness)
d <- d %>%
  mutate(worry_material = worry_economic + worry_basic_needs)

# Social support SOURCE decomposition (already computed in Stata, zero missing)
d <- d %>%
  mutate(
    support_close = as.integer(q52_support_close),  # Close family/friends
    support_community = as.integer(q52_support_comm), # Community/neighbors
    support_government = as.integer(q52_support_gov), # Government programs
    help_close = as.integer(q53_help_close),
    help_community = as.integer(q53_help_comm),
    help_government = as.integer(q53_help_gov)
  )

# q37: Illness management behaviors (help-seeking)
# 1=stayed home, 2=fluids, 3=herbal/vitamins, 4=antibiotics from pharmacy,
# 5=other pharmacy meds, 6=called hotline, 7=govt primary care, 8=govt hospital, 9=private hospital
d <- d %>%
  rowwise() %>%
  mutate(
    # Formal help-seeking (called provider, went to facility)
    helpseeking_formal = sum(c_across(c(q37___6, q37___7, q37___8, q37___9)), na.rm = FALSE),
    # Self-management (stayed home, fluids, herbal, pharmacy)
    helpseeking_self = sum(c_across(c(q37___1, q37___2, q37___3, q37___4, q37___5)), na.rm = FALSE)
  ) %>%
  ungroup()

# Healthcare workers in household (q17)
d <- d %>%
  mutate(hcw_in_household = as.integer(q17_hcws > 0 & !is.na(q17_hcws)))

# COVID testing (q38: 0=never, 1-4=tested at various times)
d <- d %>%
  mutate(covid_tested = as.integer(q38_test > 0 & !is.na(q38_test)))

cat("Additional variables added: occupation, missed_healthcare, household_size,\n")
cat("  suicidality, income_per_capita, worry_material,\n")
cat("  support 3-source (close/community/govt), help 3-source,\n")
cat("  help-seeking (formal/self), hcw_in_household, covid_tested\n")

# ── 10. EFA on Worry Items (q51) ────────────────────────────────────────────

cat("\n=== EFA ON WORRY ITEMS (q51) ===\n")

worry_data <- analytic %>%
  select(all_of(all_worry_items)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

cat("Complete cases for worry EFA/CFA:", nrow(worry_data), "\n")

# SPLIT SAMPLE: EFA on 50%, CFA on other 50% (avoids circular validation)
set.seed(123)
split_idx <- sample(seq_len(nrow(worry_data)), size = floor(nrow(worry_data) / 2))
worry_efa_data <- worry_data[split_idx, ]
worry_cfa_data <- worry_data[-split_idx, ]
cat("EFA sample:", nrow(worry_efa_data), "| CFA sample:", nrow(worry_cfa_data), "\n")

# Parallel analysis to determine number of factors (on EFA half)
cat("\nParallel analysis (EFA subsample):\n")
pa_result <- fa.parallel(worry_efa_data, fm = "ml", cor = "poly", fa = "fa",
                         plot = FALSE, n.iter = 100)
cat("Suggested number of factors:", pa_result$nfact, "\n")

# Run EFA with theoretical number (5 factors) and data-driven number
for (nf in c(pa_result$nfact, 5)) {
  cat("\n--- EFA with", nf, "factors (oblimin rotation, polychoric) ---\n")
  efa_result <- fa(worry_efa_data, nfactors = nf, rotate = "oblimin",
                   cor = "poly", fm = "ml")
  print(efa_result$loadings, cutoff = 0.30)
  cat("\nVariance explained:", round(sum(efa_result$Vaccounted["Proportion Var", ]) * 100, 1), "%\n")
  cat("RMSR:", round(efa_result$rms, 4), "\n")
  cat("TLI:", round(efa_result$TLI, 3), "\n")
}

# Save 5-factor EFA
efa_5f <- fa(worry_data, nfactors = 5, rotate = "oblimin", cor = "poly", fm = "ml")

# Extract and save loadings
loadings_df <- as.data.frame(unclass(efa_5f$loadings)) %>%
  rownames_to_column("item") %>%
  mutate(
    theoretical_domain = case_when(
      item %in% health_fear_items      ~ "Health fears",
      item %in% econ_worry_items       ~ "Economic concerns",
      item %in% basic_needs_items      ~ "Basic needs",
      item %in% social_disruption_items ~ "Social/family",
      item %in% safety_stigma_items    ~ "Safety/stigma"
    )
  )

write_csv(loadings_df, file.path(tab_dir, "TableS1_EFA_worry_loadings.csv"))

# ── 11. Reliability (Cronbach's Alpha & McDonald's Omega) ───────────────────

cat("\n=== RELIABILITY ANALYSIS ===\n")

# Function to compute alpha and omega safely
reliability_report <- function(data, items, label) {
  item_data <- data %>% select(all_of(items)) %>% mutate(across(everything(), as.numeric)) %>% drop_na()
  n_complete <- nrow(item_data)

  if (n_complete < 50 || ncol(item_data) < 2) {
    return(tibble(construct = label, n_items = length(items), n_complete = n_complete,
                  alpha = NA, omega_total = NA))
  }

  a <- tryCatch(psych::alpha(item_data)$total$raw_alpha, error = function(e) NA)
  o <- tryCatch(psych::omega(item_data, plot = FALSE, nfactors = 1)$omega.tot,
                error = function(e) NA)

  tibble(construct = label, n_items = length(items), n_complete = n_complete,
         alpha = round(a, 3), omega_total = round(o, 3))
}

reliability_results <- bind_rows(
  reliability_report(analytic, health_fear_items, "Worry: Health fears"),
  reliability_report(analytic, econ_worry_items, "Worry: Economic concerns"),
  reliability_report(analytic, basic_needs_items, "Worry: Basic needs"),
  reliability_report(analytic, social_disruption_items, "Worry: Social/family"),
  reliability_report(analytic, safety_stigma_items, "Worry: Safety/stigma"),
  reliability_report(analytic, all_worry_items, "Worry: Total (original)"),
  reliability_report(analytic, compliance_items, "Behavioral compliance"),
  reliability_report(analytic, prevention_evidence_items, "Prevention: Evidence-based"),
  reliability_report(analytic, prevention_folk_items, "Prevention: Folk/alternative"),
  reliability_report(analytic, lifestyle_items, "Lifestyle disruption: Total"),
  reliability_report(analytic, dass_depr_items, "DASS: Depression"),
  reliability_report(analytic, dass_anx_items, "DASS: Anxiety"),
  reliability_report(analytic, dass_stress_items, "DASS: Stress")
)

cat("\nReliability Summary:\n")
print(reliability_results, n = 20)

write_csv(reliability_results, file.path(tab_dir, "Table2_psychometric_properties.csv"))

# ── 12. CFA on Worry Sub-Domains (Confirmatory) ────────────────────────────

cat("\n=== CFA ON WORRY SUB-DOMAINS ===\n")

# CFA model specification
cfa_worry_model <- '
  health_fear   =~ q51_2_quarantine + q51_3_infected + q51_4_severe +
                    q51_5_death + q51_6_health_system
  econ_worry    =~ q51_10_income + q51_14_rent + q51_15_debt +
                    q51_16_agri + q51_17_harvest + q51_18_produce +
                    q51_21_recession
  basic_needs   =~ q51_11_food + q51_12_water + q51_13_medicine
  social_disrup =~ q51_1_lonely + q51_7_education + q51_8_teaching +
                    q51_9_children_home
  safety_stigma =~ q51_19_stigma + q51_20_abuse
'

# Prepare CFA data — use HOLD-OUT SAMPLE (not EFA sample) to avoid circular validation
cfa_data <- worry_cfa_data  # From split in Section 10
cat("CFA sample size:", nrow(cfa_data), "(independent from EFA sample)\n")

# Fit CFA (WLSMV for ordinal data)
cfa_fit <- tryCatch(
  cfa(cfa_worry_model, data = cfa_data, ordered = all_worry_items,
      estimator = "WLSMV", std.lv = TRUE),
  error = function(e) {
    cat("WLSMV CFA failed, trying MLR:\n", e$message, "\n")
    cfa(cfa_worry_model, data = cfa_data, estimator = "MLR",
        missing = "fiml", std.lv = TRUE)
  }
)

cat("\nCFA Fit Indices:\n")
fit_indices <- fitMeasures(cfa_fit, c("chisq", "df", "pvalue",
                                       "cfi", "tli", "rmsea",
                                       "rmsea.ci.lower", "rmsea.ci.upper",
                                       "srmr"))
print(round(fit_indices, 3))

cat("\nCFA Factor Loadings:\n")
print(standardizedSolution(cfa_fit) %>%
        filter(op == "=~") %>%
        select(lhs, rhs, est.std, se, pvalue) %>%
        mutate(across(c(est.std, se), ~ round(.x, 3))))

# Save CFA results
cfa_results <- list(
  fit = fit_indices,
  loadings = standardizedSolution(cfa_fit)
)
saveRDS(cfa_results, file.path(out_dir, "cfa_worry_results.rds"))

# ── 13. EFA on Other Item Batteries ─────────────────────────────────────────

cat("\n=== EFA ON COMPLIANCE (q22) ===\n")
comp_data <- analytic %>%
  select(all_of(compliance_items)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()
cat("N:", nrow(comp_data), "\n")
if (nrow(comp_data) > 100) {
  pa_comp <- fa.parallel(comp_data, fm = "ml", fa = "fa", plot = FALSE, n.iter = 50)
  cat("Suggested factors:", pa_comp$nfact, "\n")
}

cat("\n=== EFA ON LIFESTYLE (q26) ===\n")
life_data <- analytic %>%
  select(all_of(lifestyle_items)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()
cat("N:", nrow(life_data), "\n")
if (nrow(life_data) > 100) {
  pa_life <- fa.parallel(life_data, fm = "ml", fa = "fa", plot = FALSE, n.iter = 50)
  cat("Suggested factors:", pa_life$nfact, "\n")
  efa_life <- fa(life_data, nfactors = min(pa_life$nfact, 4), rotate = "oblimin", fm = "ml")
  print(efa_life$loadings, cutoff = 0.30)
}

cat("\n=== EFA ON PREVENTION (q23) ===\n")
prev_data <- analytic %>%
  select(all_of(c(prevention_evidence_items, prevention_folk_items))) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()
cat("N:", nrow(prev_data), "\n")
if (nrow(prev_data) > 100) {
  pa_prev <- fa.parallel(prev_data, fm = "ml", fa = "fa", plot = FALSE, n.iter = 50)
  cat("Suggested factors:", pa_prev$nfact, "\n")
  efa_prev <- fa(prev_data, nfactors = 2, rotate = "oblimin", fm = "ml")
  print(efa_prev$loadings, cutoff = 0.30)
}

# ── 14. Save Augmented Analysis Dataset ─────────────────────────────────────

# Select analysis variables
analysis_vars <- d %>%
  select(
    # Identifiers
    record_id, site, country, location_type,
    # Layer 1: Structural vulnerability
    age, female, gender, ethnicity_minority, education, employment, marital,
    comorbidity, gen_health, gen_health_now, crowding, income_cat,
    # Layer 2: Shock appraisal
    perceived_risk, risk_self, risk_country, govt_awareness, health_trust,
    # Layer 3: Stressor proliferation - worry sub-domains
    worry_health_fear, worry_economic, worry_basic_needs,
    worry_social, worry_safety_stigma, worry_total,
    # Layer 3: Other stressors
    livelihood_disruption,
    lifestyle_access, lifestyle_health, lifestyle_social, lifestyle_total,
    # Layer 3: Merged material hardship (for SEM without Heywood)
    worry_material,
    # Layer 4: Coping
    compliance, prevention_evidence, prevention_folk,
    supportive_community, helpful_community, support_score,
    media_trust,
    # Additional Layer 1 variables (newly included)
    occupation, household_size, income_per_capita,
    # Additional Layer 3/4 variables
    missed_healthcare, hcw_in_household, covid_tested,
    # Support source decomposition (Layer 4)
    support_close, support_community, support_government,
    help_close, help_community, help_government,
    # Help-seeking behavior (Layer 4)
    helpseeking_formal, helpseeking_self,
    # Outcomes (continuous z-standardized)
    z_depression, z_anxiety, z_stress,
    depression, anxiety, stress,
    # Outcomes (binary for sensitivity)
    depr_any, anx_any, stress_any,
    # Suicidality (supplementary outcome)
    suicidal_thought, suicidal_plan, suicidal_attempt, any_suicidality,
    # Sample indicator
    complete_dass,
    # DASS individual items (for SEM)
    all_of(dass_depr_items), all_of(dass_anx_items), all_of(dass_stress_items),
    # Worry individual items (for SEM)
    all_of(all_worry_items)
  )

# Save
saveRDS(analysis_vars, file.path(out_dir, "spear_analysis_augmented.rds"))
write_csv(analysis_vars, file.path(out_dir, "spear_analysis_augmented.csv"))

cat("\n=== AUGMENTED DATASET SAVED ===\n")
cat("Variables:", ncol(analysis_vars), "\n")
cat("Observations:", nrow(analysis_vars), "\n")
cat("Analytic sample (complete DASS):", sum(analysis_vars$complete_dass), "\n")

# ── 15. Summary Statistics of New Constructs ─────────────────────────────────

cat("\n=== CONSTRUCT SUMMARY (Analytic Sample) ===\n")
analysis_vars %>%
  filter(complete_dass) %>%
  select(worry_health_fear:worry_total,
         livelihood_disruption:lifestyle_total,
         compliance:prevention_folk,
         perceived_risk, govt_awareness, health_trust,
         risk_self, risk_country) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = round(mean(value, na.rm = TRUE), 2),
    sd   = round(sd(value, na.rm = TRUE), 2),
    min  = min(value, na.rm = TRUE),
    max  = max(value, na.rm = TRUE),
    pct_miss = round(sum(is.na(value)) / n() * 100, 1),
    .groups = "drop"
  ) %>%
  print(n = 30)

cat("\n=== 01_construct_development.R COMPLETE ===\n")
