# DEPRECATED (v7+ canonical): this script is preserved for historical traceability.
# The canonical 13-site sample writers for all downstream tables are
# 17_primary_13sites.R and 18_rerun_all_on_13sites.R. If you run THIS script
# standalone, its outputs may overwrite canonical CSVs with 14-site or old-filter
# content. Always re-run 17 and then 18 after running this script.
#
################################################################################
# 02_descriptives.R
# Pathways from public health shock exposure to psychological distress
# in vulnerable LMIC communities
#
# Purpose: Descriptive statistics, correlation matrix, distribution figures
# Input:   spear_analysis_augmented.rds (from 01_construct_development.R)
# Output:  Table 1, Table 3 (correlations), Figures 3
################################################################################

# ── 0. Setup ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(gtsummary)
library(corrplot)
library(patchwork)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

# Load augmented dataset
dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
analytic <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)
cat("Analytic sample:", nrow(analytic), "\n")

# ── 1. Table 1: Sample Characteristics by Country ───────────────────────────

table1 <- analytic %>%
  select(
    country,
    # Demographics
    age, gender, ethnicity_minority, education, employment, marital,
    location_type,
    # Health
    comorbidity, gen_health,
    # Pandemic exposures
    perceived_risk, livelihood_disruption,
    # Worry sub-domains
    worry_health_fear, worry_economic, worry_basic_needs,
    worry_social, worry_safety_stigma, worry_total,
    # Coping
    compliance, prevention_evidence, prevention_folk,
    health_trust, support_score,
    # Lifestyle
    lifestyle_access, lifestyle_health, lifestyle_social,
    # Outcomes
    z_depression, z_anxiety, z_stress,
    depression, anxiety, stress
  ) %>%
  mutate(
    ethnicity_minority = factor(ethnicity_minority, levels = c(0, 1),
                                labels = c("Majority", "Minority")),
    education = factor(education,
                       labels = c("No formal", "Primary incomplete",
                                  "Primary complete", "Secondary incomplete",
                                  "Secondary complete", "Tertiary incomplete",
                                  "Tertiary complete", "Postgraduate")[1:length(unique(education[!is.na(education)]))]),
    comorbidity_cat = factor(case_when(
      comorbidity == 0 ~ "None",
      comorbidity == 1 ~ "One",
      comorbidity >= 2 ~ "Two or more"
    ), levels = c("None", "One", "Two or more"))
  )

# Generate gtsummary table
tbl1 <- table1 %>%
  tbl_summary(
    by = country,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany",
    label = list(
      age ~ "Age (years)",
      gender ~ "Gender",
      ethnicity_minority ~ "Ethnicity",
      education ~ "Education level",
      employment ~ "Employment status",
      marital ~ "Marital status",
      location_type ~ "Location type",
      comorbidity ~ "Comorbidity count",
      gen_health ~ "General health (1-5)",
      perceived_risk ~ "Perceived health threat (0-30)",
      livelihood_disruption ~ "Livelihood disruption (0-4)",
      worry_health_fear ~ "Worry: Health fears",
      worry_economic ~ "Worry: Economic concerns",
      worry_basic_needs ~ "Worry: Basic needs insecurity",
      worry_social ~ "Worry: Social/family disruption",
      worry_safety_stigma ~ "Worry: Safety/stigma",
      worry_total ~ "Worry: Total score",
      compliance ~ "Behavioral compliance",
      prevention_evidence ~ "Evidence-based prevention",
      prevention_folk ~ "Folk/alternative prevention",
      health_trust ~ "Health system trust",
      support_score ~ "Social support score",
      lifestyle_access ~ "Lifestyle: Access disruption",
      lifestyle_health ~ "Lifestyle: Health behavior change",
      lifestyle_social ~ "Lifestyle: Social disruption",
      z_depression ~ "Depression (z-score)",
      z_anxiety ~ "Anxiety (z-score)",
      z_stress ~ "Stress (z-score)",
      depression ~ "Depression (raw DASS score)",
      anxiety ~ "Anxiety (raw DASS score)",
      stress ~ "Stress (raw DASS score)"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    )
  )

# Save as CSV
tbl1_df <- as_tibble(tbl1)
write_csv(tbl1_df, file.path(tab_dir, "Table1_Sample_Characteristics.csv"))

# Save as HTML for viewing (requires gt package)
if (requireNamespace("gt", quietly = TRUE)) {
  gt_tbl1 <- as_gt(tbl1)
  gt::gtsave(gt_tbl1, file.path(tab_dir, "Table1_Sample_Characteristics.html"))
}

cat("Table 1 saved.\n")

# ── 2. Table 3: Correlation Matrix of Key Constructs ────────────────────────

cor_vars <- analytic %>%
  select(
    # Layer 2
    perceived_risk, risk_self, health_trust,
    # Layer 3 - worry sub-domains
    worry_health_fear, worry_economic, worry_basic_needs,
    worry_social, worry_safety_stigma,
    # Layer 3 - other stressors
    livelihood_disruption,
    lifestyle_access, lifestyle_health, lifestyle_social,
    # Layer 4
    compliance, prevention_evidence, prevention_folk,
    support_score,
    # Outcomes
    z_depression, z_anxiety, z_stress
  )

# Pairwise complete correlation
cor_mat <- cor(cor_vars, use = "pairwise.complete.obs", method = "spearman")

# Save correlation matrix (Table 3 canonical; heatmap figure retired — the
# table representation is what the manuscript cites, and the heatmap was an
# unreferenced orphan.)
write_csv(as_tibble(cor_mat, rownames = "variable"),
          file.path(tab_dir, "Table3_correlation_matrix.csv"))

cat("Correlation matrix saved (Table 3 CSV).\n")

# ── 3. VIF Check (Multicollinearity) ────────────────────────────────────────

# Quick check using a simple linear model
library(car)

vif_model <- lm(z_depression ~
                   perceived_risk + risk_self + health_trust +
                   worry_health_fear + worry_economic + worry_basic_needs +
                   worry_social + worry_safety_stigma +
                   livelihood_disruption +
                   compliance + prevention_evidence + prevention_folk +
                   age + female + ethnicity_minority + comorbidity,
                 data = analytic)

vif_values <- vif(vif_model)
cat("\n=== VIF CHECK ===\n")
print(round(vif_values, 2))
cat("Max VIF:", max(vif_values), "\n")
if (max(vif_values) > 5) {
  cat("WARNING: VIF > 5 detected. Consider removing or combining predictors.\n")
} else {
  cat("All VIF < 5. No multicollinearity concerns.\n")
}

# ── 4. Figure 3: Distribution of Key Constructs by Country ──────────────────

# Theme for publication
theme_pub <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12)
  )

# 4a. Worry sub-domains by country
worry_long <- analytic %>%
  select(country, worry_health_fear, worry_economic, worry_basic_needs,
         worry_social, worry_safety_stigma) %>%
  pivot_longer(-country, names_to = "subdomain", values_to = "score") %>%
  mutate(subdomain = dplyr::recode(subdomain,
    worry_health_fear = "Health fears",
    worry_economic = "Economic concerns",
    worry_basic_needs = "Basic needs",
    worry_social = "Social/family",
    worry_safety_stigma = "Safety/stigma"
  ))

p_worry <- ggplot(worry_long, aes(x = country, y = score, fill = country)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "red") +
  facet_wrap(~subdomain, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("#4DAF4A", "#377EB8", "#E41A1C")) +
  labs(title = "A. Worry Sub-Domain Scores by Country",
       x = NULL, y = "Score", fill = "Country") +
  theme_pub

# 4b. DASS outcomes by country
dass_long <- analytic %>%
  select(country, z_depression, z_anxiety, z_stress) %>%
  pivot_longer(-country, names_to = "outcome", values_to = "score") %>%
  mutate(outcome = dplyr::recode(outcome,
    z_depression = "Depression",
    z_anxiety = "Anxiety",
    z_stress = "Stress"
  ))

p_dass <- ggplot(dass_long, aes(x = country, y = score, fill = country)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "red") +
  facet_wrap(~outcome, nrow = 1) +
  scale_fill_manual(values = c("#4DAF4A", "#377EB8", "#E41A1C")) +
  labs(title = "B. DASS-21 Z-Scores by Country",
       x = NULL, y = "Z-Score", fill = "Country") +
  theme_pub

# 4c. Coping by country
coping_long <- analytic %>%
  select(country, compliance, prevention_evidence, prevention_folk) %>%
  pivot_longer(-country, names_to = "measure", values_to = "score") %>%
  mutate(measure = dplyr::recode(measure,
    compliance = "Behavioral\nCompliance",
    prevention_evidence = "Evidence-Based\nPrevention",
    prevention_folk = "Folk/Alternative\nPrevention"
  ))

p_coping <- ggplot(coping_long, aes(x = country, y = score, fill = country)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "red") +
  facet_wrap(~measure, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("#4DAF4A", "#377EB8", "#E41A1C")) +
  labs(title = "C. Coping and Behavioral Adaptation by Country",
       x = NULL, y = "Score", fill = "Country") +
  theme_pub

# Combine. Apply the shared theme per-plot (instead of using patchwork's
# `&` operator) to avoid S4 generic-method clashes introduced by later
# package loads (e.g. `car` / Matrix-based packages override `&`).
p_worry  <- p_worry  + theme(legend.position = "bottom")
p_dass   <- p_dass   + theme(legend.position = "bottom")
p_coping <- p_coping + theme(legend.position = "bottom")
p_combined <- p_worry / p_dass / p_coping +
  plot_layout(guides = "collect")

ggsave(file.path(fig_dir, "Figure3_Constructs_by_Country.png"),
       p_combined, width = 14, height = 12, dpi = 300)
ggsave(file.path(fig_dir, "Figure3_Constructs_by_Country.pdf"),
       p_combined, width = 14, height = 12)

cat("Figure 3 saved.\n")

# ── 5. Prevalence of Mental Health Outcomes ─────────────────────────────────

cat("\n=== MENTAL HEALTH PREVALENCE ===\n")
prevalence <- analytic %>%
  group_by(country) %>%
  summarise(
    n = n(),
    depr_prev = round(mean(depr_any, na.rm = TRUE) * 100, 1),
    anx_prev  = round(mean(anx_any, na.rm = TRUE) * 100, 1),
    stress_prev = round(mean(stress_any, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(prevalence)

# Overall
cat("\nOverall prevalence:\n")
cat("Depression:", round(mean(analytic$depr_any, na.rm = TRUE) * 100, 1), "%\n")
cat("Anxiety:", round(mean(analytic$anx_any, na.rm = TRUE) * 100, 1), "%\n")
cat("Stress:", round(mean(analytic$stress_any, na.rm = TRUE) * 100, 1), "%\n")

# ── 6. Country-Level Summary of New Constructs ──────────────────────────────

cat("\n=== KEY CONSTRUCTS BY COUNTRY ===\n")
country_summary <- analytic %>%
  group_by(country) %>%
  summarise(
    across(c(worry_health_fear, worry_economic, worry_basic_needs,
             worry_social, worry_safety_stigma, worry_total,
             perceived_risk, livelihood_disruption,
             compliance, prevention_evidence, prevention_folk,
             health_trust, support_score),
           list(mean = ~ round(mean(.x, na.rm = TRUE), 2),
                sd   = ~ round(sd(.x, na.rm = TRUE), 2)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

# Save
write_csv(country_summary, file.path(tab_dir, "country_construct_summary.csv"))

# Print key constructs
cat("\nWorry sub-domains (mean):\n")
analytic %>%
  group_by(country) %>%
  summarise(
    health_fear = round(mean(worry_health_fear, na.rm = TRUE), 2),
    economic = round(mean(worry_economic, na.rm = TRUE), 2),
    basic_needs = round(mean(worry_basic_needs, na.rm = TRUE), 2),
    social = round(mean(worry_social, na.rm = TRUE), 2),
    safety = round(mean(worry_safety_stigma, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>% print()

cat("\nPrevention (mean):\n")
analytic %>%
  group_by(country) %>%
  summarise(
    evidence = round(mean(prevention_evidence, na.rm = TRUE), 2),
    folk = round(mean(prevention_folk, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>% print()

cat("\n=== 02_descriptives.R COMPLETE ===\n")
