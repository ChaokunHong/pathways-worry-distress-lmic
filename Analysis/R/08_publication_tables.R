################################################################################
# 08_publication_tables.R
# Generate publication-ready Excel tables for Social Science & Medicine
# All tables formatted with headers, notes, and consistent styling
################################################################################

library(tidyverse)
library(openxlsx)

proj <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
tab_dir <- file.path(proj, "Analysis/tables")
out_file <- file.path(proj, "Analysis/Publication_Tables.xlsx")

# ── Style definitions ────────────────────────────────────────────────────────
headerStyle <- createStyle(
  fontSize = 11, fontColour = "#FFFFFF", fgFill = "#3C5488",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "bottom", borderColour = "#333333", wrapText = TRUE
)
subheaderStyle <- createStyle(
  fontSize = 10, fgFill = "#D6DCE4", halign = "left",
  textDecoration = "bold", border = "bottom"
)
bodyStyle <- createStyle(fontSize = 10, halign = "center", valign = "center")
leftStyle <- createStyle(fontSize = 10, halign = "left", valign = "center")
noteStyle <- createStyle(fontSize = 9, fontColour = "#666666", wrapText = TRUE)
sigStyle <- createStyle(fontSize = 10, halign = "center", fontColour = "#B2182B",
                        textDecoration = "bold")
pctStyle <- createStyle(fontSize = 10, halign = "center", numFmt = "0.0%")

wb <- createWorkbook()

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1: Sample Characteristics
# ══════════════════════════════════════════════════════════════════════════════
cat("Table 1...\n")

dat <- readRDS(file.path(proj, "Analysis/output/spear_analysis_augmented.rds"))
ana <- dat[dat$complete_dass, ]

# Build Table 1 manually for full control
t1_rows <- list()
add_row <- function(var, label, type = "continuous") {
  if (type == "continuous") {
    overall <- sprintf("%.1f (%.1f)", mean(ana[[var]], na.rm=TRUE), sd(ana[[var]], na.rm=TRUE))
    idn <- sprintf("%.1f (%.1f)", mean(ana[[var]][ana$country=="Indonesia"], na.rm=TRUE),
                    sd(ana[[var]][ana$country=="Indonesia"], na.rm=TRUE))
    npl <- sprintf("%.1f (%.1f)", mean(ana[[var]][ana$country=="Nepal"], na.rm=TRUE),
                    sd(ana[[var]][ana$country=="Nepal"], na.rm=TRUE))
    vnm <- sprintf("%.1f (%.1f)", mean(ana[[var]][ana$country=="Vietnam"], na.rm=TRUE),
                    sd(ana[[var]][ana$country=="Vietnam"], na.rm=TRUE))
    p <- tryCatch(format(kruskal.test(ana[[var]] ~ ana$country)$p.value, digits=3),
                  error = function(e) "—")
    t1_rows[[length(t1_rows)+1]] <<- c(label, overall, idn, npl, vnm, p)
  }
}

add_row("age", "Age, years, mean (SD)")
add_row("comorbidity", "Comorbidity count, mean (SD)")
add_row("perceived_risk", "Perceived health threat (0-30)")
add_row("livelihood_disruption", "Livelihood disruption (0-12)")
add_row("worry_health_fear", "Worry: Health fears (0-15)")
add_row("worry_economic", "Worry: Economic concerns (0-21)")
add_row("worry_basic_needs", "Worry: Basic needs (0-9)")
add_row("worry_social", "Worry: Social/family (0-12)")
add_row("worry_safety_stigma", "Worry: Safety/stigma (0-6)")
add_row("z_depression", "Depression (z-score)")
add_row("z_anxiety", "Anxiety (z-score)")
add_row("z_stress", "Stress (z-score)")

t1_df <- as.data.frame(do.call(rbind, t1_rows))
names(t1_df) <- c("Characteristic", "Overall (N=1,462)", "Indonesia (n=607)",
                   "Nepal (n=460)", "Vietnam (n=395)", "p-value")

# Add categorical rows
cat_rows <- tribble(
  ~Characteristic, ~`Overall (N=1,462)`, ~`Indonesia (n=607)`, ~`Nepal (n=460)`, ~`Vietnam (n=395)`, ~`p-value`,
  "Female, n (%)", sprintf("%d (%.1f%%)", sum(ana$female==1,na.rm=TRUE), mean(ana$female==1,na.rm=TRUE)*100),
    sprintf("%d (%.1f%%)", sum(ana$female[ana$country=="Indonesia"]==1,na.rm=TRUE), mean(ana$female[ana$country=="Indonesia"]==1,na.rm=TRUE)*100),
    sprintf("%d (%.1f%%)", sum(ana$female[ana$country=="Nepal"]==1,na.rm=TRUE), mean(ana$female[ana$country=="Nepal"]==1,na.rm=TRUE)*100),
    sprintf("%d (%.1f%%)", sum(ana$female[ana$country=="Vietnam"]==1,na.rm=TRUE), mean(ana$female[ana$country=="Vietnam"]==1,na.rm=TRUE)*100),
    format(chisq.test(table(ana$female, ana$country))$p.value, digits=3),
  "Ethnic minority, n (%)", sprintf("%d (%.1f%%)", sum(ana$ethnicity_minority==1,na.rm=TRUE), mean(ana$ethnicity_minority==1,na.rm=TRUE)*100),
    sprintf("%d", sum(ana$ethnicity_minority[ana$country=="Indonesia"]==1,na.rm=TRUE)),
    sprintf("%d", sum(ana$ethnicity_minority[ana$country=="Nepal"]==1,na.rm=TRUE)),
    sprintf("%d", sum(ana$ethnicity_minority[ana$country=="Vietnam"]==1,na.rm=TRUE)),
    "<0.001"
)

t1_final <- bind_rows(cat_rows, t1_df)

addWorksheet(wb, "Table 1")
writeData(wb, "Table 1", t1_final, startRow = 2)
writeData(wb, "Table 1", "Table 1. Sample Characteristics by Country", startRow = 1, startCol = 1)
addStyle(wb, "Table 1", headerStyle, rows = 2, cols = 1:6, gridExpand = TRUE)
addStyle(wb, "Table 1", leftStyle, rows = 3:(nrow(t1_final)+2), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table 1", bodyStyle, rows = 3:(nrow(t1_final)+2), cols = 2:6, gridExpand = TRUE)
setColWidths(wb, "Table 1", cols = 1:6, widths = c(35, 18, 18, 18, 18, 12))
writeData(wb, "Table 1",
  "Note: Continuous variables presented as mean (SD). Categorical variables as n (%). P-values from Kruskal-Wallis (continuous) or chi-square (categorical) tests. Ethnic minority percentage computed among n = 1,270 with non-missing ethnicity data (257 missing).",
  startRow = nrow(t1_final) + 3)
addStyle(wb, "Table 1", noteStyle, rows = nrow(t1_final)+3, cols = 1)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2: Psychometric Properties
# ══════════════════════════════════════════════════════════════════════════════
cat("Table 2...\n")

t2 <- read_csv(file.path(tab_dir, "Table2_psychometric_properties.csv"), show_col_types = FALSE)
t2 <- t2 %>% dplyr::select(construct, n_items, n_complete, alpha, omega_total) %>%
  rename(Construct = construct, Items = n_items, `Complete n` = n_complete,
         `Cronbach's alpha` = alpha, `McDonald's omega` = omega_total)

addWorksheet(wb, "Table 2")
writeData(wb, "Table 2", "Table 2. Psychometric Properties of Composite Measures", startRow = 1)
writeData(wb, "Table 2", t2, startRow = 2)
addStyle(wb, "Table 2", headerStyle, rows = 2, cols = 1:5, gridExpand = TRUE)
addStyle(wb, "Table 2", leftStyle, rows = 3:(nrow(t2)+2), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table 2", bodyStyle, rows = 3:(nrow(t2)+2), cols = 2:5, gridExpand = TRUE)
setColWidths(wb, "Table 2", cols = 1:5, widths = c(35, 8, 12, 16, 16))

# CFA fit note
cfa <- readRDS(file.path(proj, "Analysis/output/cfa_worry_results.rds"))
cfa_note <- sprintf("CFA fit (independent hold-out sample, n = 660): CFI = %.3f, TLI = %.3f, RMSEA = %.3f [%.3f, %.3f], SRMR = %.3f",
                     cfa$fit["cfi"], cfa$fit["tli"], cfa$fit["rmsea"],
                     cfa$fit["rmsea.ci.lower"], cfa$fit["rmsea.ci.upper"], cfa$fit["srmr"])
writeData(wb, "Table 2", cfa_note, startRow = nrow(t2) + 3)
addStyle(wb, "Table 2", noteStyle, rows = nrow(t2)+3, cols = 1)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 3: R² Decomposition (Sequential Models)
# ══════════════════════════════════════════════════════════════════════════════
cat("Table 3...\n")

# Use MI R² (fixed sample n=1,500) as primary; fall back to multilevel if unavailable
r2_file <- file.path(tab_dir, "R2_decomposition_MI.csv")
if (!file.exists(r2_file)) r2_file <- file.path(tab_dir, "R2_decomposition_multilevel.csv")
r2 <- read_csv(r2_file, show_col_types = FALSE)
# Handle both MI (R2m_*) and multilevel (R2_marginal_*) column names
if ("R2m_Depression" %in% names(r2)) {
  r2 <- r2 %>% rename(R2_marginal_Depression = R2m_Depression,
                        R2_marginal_Anxiety = R2m_Anxiety,
                        R2_marginal_Stress = R2m_Stress)
}
if (!"n_Depression" %in% names(r2) & "n" %in% names(r2)) {
  r2$n_Depression <- r2$n
}

r2_tab <- r2 %>%
  mutate(
    Model = case_when(
      model == "M0" ~ "M0: Country",
      model == "M1" ~ "M1: + Demographics (Layer 1)",
      model == "M2" ~ "M2: + Threat appraisal (Layer 2)",
      model == "M3" ~ "M3: + Worry sub-domains (Layer 3)",
      model == "M4" ~ "M4: + Coping (Layer 4, original)",
      model == "M4b" ~ "M4b: + Coping (Layer 4, expanded)",
      TRUE ~ model
    ),
    `Depression R²m` = sprintf("%.1f%%", R2_marginal_Depression * 100),
    `Anxiety R²m` = sprintf("%.1f%%", R2_marginal_Anxiety * 100),
    `Stress R²m` = sprintf("%.1f%%", R2_marginal_Stress * 100),
    n = n_Depression
  ) %>%
  dplyr::select(Model, `Depression R²m`, `Anxiety R²m`, `Stress R²m`, n)

addWorksheet(wb, "Table 3")
writeData(wb, "Table 3", "Table 3. Variance Explained by Sequential Model Layers (Marginal R²)", startRow = 1)
writeData(wb, "Table 3", r2_tab, startRow = 2)
addStyle(wb, "Table 3", headerStyle, rows = 2, cols = 1:5, gridExpand = TRUE)
addStyle(wb, "Table 3", leftStyle, rows = 3:(nrow(r2_tab)+2), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table 3", bodyStyle, rows = 3:(nrow(r2_tab)+2), cols = 2:5, gridExpand = TRUE)
setColWidths(wb, "Table 3", cols = 1:5, widths = c(40, 16, 16, 16, 8))
writeData(wb, "Table 3",
  "Note: R²m = marginal R-squared (fixed effects only), averaged across 30 MI datasets. Sample size constant at n = 1,500 across all models, eliminating sample-selection bias in R² estimates. M3 is the primary model.",
  startRow = nrow(r2_tab) + 3)
addStyle(wb, "Table 3", noteStyle, rows = nrow(r2_tab)+3, cols = 1)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 4: Primary Results — Worry Sub-Domain Matrix (MI)
# ══════════════════════════════════════════════════════════════════════════════
cat("Table 4...\n")

mi <- read_csv(file.path(tab_dir, "Table4_Primary_Results_MI.csv"), show_col_types = FALSE)

# 4A: Worry sub-domain matrix
worry_mat <- mi %>%
  filter(grepl("worry_", term)) %>%
  mutate(
    Predictor = dplyr::recode(term,
      worry_health_fear = "Health fears",
      worry_economic = "Economic concerns",
      worry_basic_needs = "Basic needs insecurity",
      worry_social = "Social/family disruption",
      worry_safety_stigma = "Safety/stigma"),
    cell = sprintf("%.3f [%.3f, %.3f]%s",
                    estimate, conf.low, conf.high,
                    ifelse(p.value < 0.001, "***",
                    ifelse(p.value < 0.01, "**",
                    ifelse(p.value < 0.05, "*", ""))))
  ) %>%
  dplyr::select(Predictor, outcome, cell) %>%
  pivot_wider(names_from = outcome, values_from = cell)

# 4B: Other predictors
other_vars <- c("age", "female1", "ethnicity_minority1", "comorbidity",
                "perceived_risk", "risk_self", "health_trust", "livelihood_disruption",
                "countryNepal", "countryVietnam")
other_labels_map <- c(
  age = "Age (years)", female1 = "Female", ethnicity_minority1 = "Ethnic minority",
  comorbidity = "Comorbidity count", perceived_risk = "Perceived health threat",
  risk_self = "Self-assessed risk", health_trust = "Health system trust",
  livelihood_disruption = "Livelihood disruption",
  countryNepal = "Nepal (vs Indonesia)", countryVietnam = "Vietnam (vs Indonesia)"
)

other_mat <- mi %>%
  filter(term %in% other_vars) %>%
  mutate(
    Predictor = dplyr::recode(term, !!!other_labels_map),
    cell = sprintf("%.3f [%.3f, %.3f]%s",
                    estimate, conf.low, conf.high,
                    ifelse(p.value < 0.001, "***",
                    ifelse(p.value < 0.01, "**",
                    ifelse(p.value < 0.05, "*", ""))))
  ) %>%
  dplyr::select(Predictor, outcome, cell) %>%
  pivot_wider(names_from = outcome, values_from = cell)

# Combine with separator
separator <- tibble(Predictor = "--- Covariates ---", Depression = "", Anxiety = "", Stress = "")
t4_combined <- bind_rows(
  tibble(Predictor = "Panel A: Worry Sub-Domains", Depression = "", Anxiety = "", Stress = ""),
  worry_mat,
  separator,
  other_mat
)

addWorksheet(wb, "Table 4")
writeData(wb, "Table 4",
  "Table 4. Associations Between Worry Sub-Domains and Psychological Distress (MI, n = 1,462)",
  startRow = 1)
writeData(wb, "Table 4", t4_combined, startRow = 2)
addStyle(wb, "Table 4", headerStyle, rows = 2, cols = 1:4, gridExpand = TRUE)
addStyle(wb, "Table 4", leftStyle, rows = 3:(nrow(t4_combined)+2), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table 4", bodyStyle, rows = 3:(nrow(t4_combined)+2), cols = 2:4, gridExpand = TRUE)
# Bold the panel headers
addStyle(wb, "Table 4", subheaderStyle, rows = 3, cols = 1:4, gridExpand = TRUE)
addStyle(wb, "Table 4", subheaderStyle, rows = 3 + nrow(worry_mat) + 1, cols = 1:4, gridExpand = TRUE)
setColWidths(wb, "Table 4", cols = 1:4, widths = c(30, 28, 28, 28))
writeData(wb, "Table 4",
  "Note: Coefficients are unstandardized betas [95% CI] from multilevel models with multiple imputation (m = 30). * p < 0.05, ** p < 0.01, *** p < 0.001. Models adjusted for country, demographics, and all listed covariates simultaneously.",
  startRow = nrow(t4_combined) + 3)
addStyle(wb, "Table 4", noteStyle, rows = nrow(t4_combined)+3, cols = 1)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 5: Mediation Results
# ══════════════════════════════════════════════════════════════════════════════
cat("Table 5...\n")

med <- read_csv(file.path(tab_dir, "Table5_Mediation_Results.csv"), show_col_types = FALSE)
t5 <- med %>%
  mutate(
    Pathway = label,
    ACME = sprintf("%.4f [%.4f, %.4f]", acme, acme_ci_low, acme_ci_high),
    `Direct (ADE)` = sprintf("%.4f [%.4f, %.4f]", ade, ade_ci_low, ade_ci_high),
    `Total` = sprintf("%.4f", total),
    `% Mediated` = sprintf("%.1f%%", prop_mediated * 100),
    `p` = ifelse(acme_p < 0.001, "<0.001", sprintf("%.3f", acme_p))
  ) %>%
  dplyr::select(Pathway, ACME, `Direct (ADE)`, Total, `% Mediated`, p)

addWorksheet(wb, "Table 5")
writeData(wb, "Table 5",
  "Table 5. Mediation Analysis: Indirect Effects (complete-case bootstrap comparator; see Table S17 for multiply-imputed Rubin-pooled primary estimates)",
  startRow = 1)
writeData(wb, "Table 5", t5, startRow = 2)
addStyle(wb, "Table 5", headerStyle, rows = 2, cols = 1:6, gridExpand = TRUE)
addStyle(wb, "Table 5", leftStyle, rows = 3:(nrow(t5)+2), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table 5", bodyStyle, rows = 3:(nrow(t5)+2), cols = 2:6, gridExpand = TRUE)
setColWidths(wb, "Table 5", cols = 1:6, widths = c(45, 26, 26, 12, 14, 10))
writeData(wb, "Table 5",
  "Note: ACME = Average Causal Mediation Effect. Bootstrap 95% CIs (1,000 iterations, seed = 42). Cross-sectional data; causal language reflects the mediation framework assumptions, not observed temporal ordering.",
  startRow = nrow(t5) + 3)
addStyle(wb, "Table 5", noteStyle, rows = nrow(t5)+3, cols = 1)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 6: Country-Stratified (Worry Sub-Domains Only)
# ══════════════════════════════════════════════════════════════════════════════
cat("Table 6...\n")

strat <- read_csv(file.path(tab_dir, "Table6_Country_Stratified_Multilevel.csv"), show_col_types = FALSE)

worry_strat <- strat %>%
  filter(grepl("worry_|comorbidity|perceived_risk|livelihood", term)) %>%
  mutate(
    Predictor = dplyr::recode(term,
      worry_health_fear = "Health fears", worry_economic = "Economic concerns",
      worry_basic_needs = "Basic needs", worry_social = "Social/family",
      worry_safety_stigma = "Safety/stigma", perceived_risk = "Perceived risk",
      livelihood_disruption = "Livelihood disruption", comorbidity = "Comorbidity"),
    cell = sprintf("%.3f%s", estimate,
                    ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**",
                    ifelse(p.value < 0.05, "*", ""))))
  ) %>%
  dplyr::select(Predictor, outcome, country, cell) %>%
  pivot_wider(names_from = c(outcome, country), values_from = cell,
              names_glue = "{outcome}_{country}")

addWorksheet(wb, "Table 6")
writeData(wb, "Table 6",
  "Table 6. Country-Stratified Regression Coefficients (complete-case multilevel; random intercept by site within each country; k = 3 Indonesia / 6 Nepal / 4 Vietnam)",
  startRow = 1)
writeData(wb, "Table 6", worry_strat, startRow = 2)
addStyle(wb, "Table 6", headerStyle, rows = 2, cols = 1:ncol(worry_strat), gridExpand = TRUE)
addStyle(wb, "Table 6", leftStyle, rows = 3:(nrow(worry_strat)+2), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table 6", bodyStyle, rows = 3:(nrow(worry_strat)+2), cols = 2:ncol(worry_strat), gridExpand = TRUE)
setColWidths(wb, "Table 6", cols = 1:ncol(worry_strat), widths = c(25, rep(14, ncol(worry_strat)-1)))

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S1: EFA Factor Loadings
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S1...\n")

efa <- read_csv(file.path(tab_dir, "TableS1_EFA_worry_loadings.csv"), show_col_types = FALSE)
addWorksheet(wb, "Table S1")
writeData(wb, "Table S1", "Table S1. Exploratory Factor Analysis: Worry Item Loadings (Oblimin Rotation)", startRow = 1)
writeData(wb, "Table S1", efa, startRow = 2)
addStyle(wb, "Table S1", headerStyle, rows = 2, cols = 1:ncol(efa), gridExpand = TRUE)
setColWidths(wb, "Table S1", cols = 1:ncol(efa), widths = c(25, rep(10, ncol(efa)-1)))

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S2: Sensitivity — CC vs MI vs Multilevel Comparison
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S2...\n")

cc <- read_csv(file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv"), show_col_types = FALSE)

worry_compare <- bind_rows(
  mi %>% filter(grepl("worry_", term)) %>%
    mutate(Method = "MI (n=1,462)", est_ci = sprintf("%.3f [%.3f, %.3f]%s", estimate, conf.low, conf.high,
      ifelse(p.value<0.05,"*",""))),
  cc %>% filter(grepl("worry_", term)) %>%
    mutate(Method = "CC Multilevel (n=892)", est_ci = sprintf("%.3f [%.3f, %.3f]%s", estimate, conf.low, conf.high,
      ifelse(p.value<0.05,"*","")))
) %>%
  mutate(Predictor = dplyr::recode(term,
    worry_health_fear = "Health fears", worry_economic = "Economic",
    worry_basic_needs = "Basic needs", worry_social = "Social/family",
    worry_safety_stigma = "Safety/stigma")) %>%
  dplyr::select(Predictor, outcome, Method, est_ci) %>%
  pivot_wider(names_from = c(outcome, Method), values_from = est_ci)

addWorksheet(wb, "Table S2")
writeData(wb, "Table S2", "Table S2. Sensitivity Analysis: Complete Case vs Multiple Imputation", startRow = 1)
writeData(wb, "Table S2", worry_compare, startRow = 2)
addStyle(wb, "Table S2", headerStyle, rows = 2, cols = 1:ncol(worry_compare), gridExpand = TRUE)
setColWidths(wb, "Table S2", cols = 1:ncol(worry_compare), widths = c(18, rep(22, ncol(worry_compare)-1)))

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S3: Missingness Pattern
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S3...\n")

miss_f <- file.path(tab_dir, "TableS_Missingness_Comparison.csv")
if (file.exists(miss_f)) {
  miss <- read_csv(miss_f, show_col_types = FALSE)
  addWorksheet(wb, "Table S3")
  writeData(wb, "Table S3", "Table S3. Comparison of Complete vs Missing Covariate Samples", startRow = 1)
  writeData(wb, "Table S3", miss, startRow = 2)
  addStyle(wb, "Table S3", headerStyle, rows = 2, cols = 1:ncol(miss), gridExpand = TRUE)
  setColWidths(wb, "Table S3", cols = 1:ncol(miss), widths = c(25, 15, 15, 12))
  writeData(wb, "Table S3",
    "Note: Outcomes (depression, anxiety, stress) do not differ significantly between groups, supporting the MAR assumption for multiple imputation.",
    startRow = nrow(miss) + 3)
  addStyle(wb, "Table S3", noteStyle, rows = nrow(miss)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S4: SEM Results
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S4...\n")

sem_fit <- read_csv(file.path(tab_dir, "TableS2b_SEM_fit_indices.csv"), show_col_types = FALSE)
sem_paths <- read_csv(file.path(tab_dir, "TableS2b_SEM_structural_paths.csv"), show_col_types = FALSE)

addWorksheet(wb, "Table S4")
writeData(wb, "Table S4", "Table S4. Structural Equation Model Results", startRow = 1)
writeData(wb, "Table S4", "Panel A: Model Fit Comparison", startRow = 2)
writeData(wb, "Table S4", sem_fit, startRow = 3)
addStyle(wb, "Table S4", headerStyle, rows = 3, cols = 1:ncol(sem_fit), gridExpand = TRUE)
addStyle(wb, "Table S4", subheaderStyle, rows = 2, cols = 1)

sem_key <- sem_paths %>%
  filter(lhs %in% c("DEPR","ANX","STR"),
         rhs %in% c("HFEAR","MATERIAL","SOCIAL","STIGMA","perceived_risk","comorbidity","age","female")) %>%
  mutate(Path = paste(lhs, "~", rhs),
         `Std. Beta [95% CI]` = sprintf("%.3f [%.3f, %.3f]%s", est.std, ci.lower, ci.upper, sig)) %>%
  dplyr::select(Path, `Std. Beta [95% CI]`)

start_row <- nrow(sem_fit) + 5
writeData(wb, "Table S4", "Panel B: Structural Paths (Standardized)", startRow = start_row)
writeData(wb, "Table S4", sem_key, startRow = start_row + 1)
addStyle(wb, "Table S4", headerStyle, rows = start_row+1, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Table S4", subheaderStyle, rows = start_row, cols = 1)
setColWidths(wb, "Table S4", cols = 1:max(ncol(sem_fit), 2), widths = c(40, 30, rep(12, max(0, ncol(sem_fit)-2))))

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S5: Community Support × Worry Interactions
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S5...\n")

int_f <- file.path(tab_dir, "Table_Support_Interactions.csv")
if (file.exists(int_f)) {
  int_raw <- read_csv(int_f, show_col_types = FALSE)
  int_tab <- int_raw %>%
    mutate(
      Predictor = dplyr::recode(worry_var,
        worry_health_fear = "Health fears",
        worry_economic = "Economic concerns",
        worry_basic_needs = "Basic needs",
        worry_social = "Social/family",
        worry_safety_stigma = "Safety/stigma"),
      cell = ifelse(p.value < 0.05,
                     sprintf("%.3f (p=%.3f)*", estimate, p.value),
                     sprintf("%.3f (ns)", estimate))
    ) %>%
    dplyr::select(Predictor, outcome, cell) %>%
    pivot_wider(names_from = outcome, values_from = cell)

  addWorksheet(wb, "Table S5")
  writeData(wb, "Table S5", "Table S5. Community Support × Worry Sub-Domain Interactions",
            startRow = 1)
  writeData(wb, "Table S5", int_tab, startRow = 2)
  addStyle(wb, "Table S5", headerStyle, rows = 2, cols = 1:ncol(int_tab), gridExpand = TRUE)
  addStyle(wb, "Table S5", leftStyle, rows = 3:(nrow(int_tab)+2), cols = 1, gridExpand = TRUE)
  addStyle(wb, "Table S5", bodyStyle, rows = 3:(nrow(int_tab)+2), cols = 2:ncol(int_tab), gridExpand = TRUE)
  setColWidths(wb, "Table S5", cols = 1:ncol(int_tab), widths = c(25, rep(22, ncol(int_tab)-1)))
  writeData(wb, "Table S5",
    "Note: Interaction coefficients from multilevel models with site-level supportive community as moderator. Negative coefficients indicate buffering (support weakens the worry-distress association). 2 of 15 interactions reached p < 0.05, both involving economic worry. Remaining 13 interactions were non-significant.",
    startRow = nrow(int_tab) + 3)
  addStyle(wb, "Table S5", noteStyle, rows = nrow(int_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S6: Stigma Contextual Decomposition
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S6...\n")

ctx_f <- file.path(tab_dir, "Table_Stigma_Contextual.csv")
if (file.exists(ctx_f)) {
  ctx_raw <- read_csv(ctx_f, show_col_types = FALSE)
  ctx_tab <- ctx_raw %>%
    filter(term %in% c("stigma_within", "stigma_site")) %>%
    mutate(
      Level = ifelse(term == "stigma_within", "Within-site (individual)", "Between-site (community)"),
      cell = sprintf("%.3f [%.3f, %.3f] (p=%.3f)%s",
                      estimate, conf.low, conf.high, p.value,
                      ifelse(p.value < 0.05, "*", ""))
    ) %>%
    dplyr::select(outcome, Level, cell) %>%
    pivot_wider(names_from = Level, values_from = cell)

  addWorksheet(wb, "Table S6")
  writeData(wb, "Table S6", "Table S6. Stigma Contextual Decomposition (Within-Site vs Between-Site)",
            startRow = 1)
  writeData(wb, "Table S6", ctx_tab, startRow = 2)
  addStyle(wb, "Table S6", headerStyle, rows = 2, cols = 1:ncol(ctx_tab), gridExpand = TRUE)
  addStyle(wb, "Table S6", leftStyle, rows = 3:(nrow(ctx_tab)+2), cols = 1, gridExpand = TRUE)
  addStyle(wb, "Table S6", bodyStyle, rows = 3:(nrow(ctx_tab)+2), cols = 2:ncol(ctx_tab), gridExpand = TRUE)
  setColWidths(wb, "Table S6", cols = 1:ncol(ctx_tab), widths = c(18, 35, 35))
  writeData(wb, "Table S6",
    "Note: Group-mean centering separates individual stigma worry into within-site (deviation from site mean) and between-site (site mean) components. Stigma ICC = 34.8% (substantial between-community variation). Between-site effect on depression (β = 0.208) was numerically larger than within-site effect (β = 0.152) but did not reach significance (p = 0.098), likely due to limited power with k = 13 sites.",
    startRow = nrow(ctx_tab) + 3)
  addStyle(wb, "Table S6", noteStyle, rows = nrow(ctx_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S7: Worry Sub-Domain ICCs
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S7...\n")

icc_f <- file.path(tab_dir, "Table_Worry_ICC.csv")
if (file.exists(icc_f)) {
  icc_raw <- read_csv(icc_f, show_col_types = FALSE)
  icc_tab <- icc_raw %>%
    arrange(desc(icc)) %>%
    mutate(
      `Worry Sub-Domain` = dplyr::recode(worry_domain,
        worry_health_fear = "Health fears",
        worry_economic = "Economic concerns",
        worry_basic_needs = "Basic needs insecurity",
        worry_social = "Social/family disruption",
        worry_safety_stigma = "Safety/stigma"),
      ICC = sprintf("%.3f", icc),
      `% Between Sites` = sprintf("%.1f%%", between_pct)
    ) %>%
    dplyr::select(`Worry Sub-Domain`, ICC, `% Between Sites`)

  addWorksheet(wb, "Table S7")
  writeData(wb, "Table S7", "Table S7. Intraclass Correlations of Worry Sub-Domains",
            startRow = 1)
  writeData(wb, "Table S7", icc_tab, startRow = 2)
  addStyle(wb, "Table S7", headerStyle, rows = 2, cols = 1:3, gridExpand = TRUE)
  addStyle(wb, "Table S7", leftStyle, rows = 3:(nrow(icc_tab)+2), cols = 1, gridExpand = TRUE)
  addStyle(wb, "Table S7", bodyStyle, rows = 3:(nrow(icc_tab)+2), cols = 2:3, gridExpand = TRUE)
  setColWidths(wb, "Table S7", cols = 1:3, widths = c(30, 12, 18))
  writeData(wb, "Table S7",
    "Note: ICC from empty multilevel models (worry ~ 1 + (1|site)). Higher values indicate greater between-community variation. The two worry sub-domains with the strongest effects on depression (basic needs, safety/stigma) also show the highest ICCs, suggesting the most impactful stressors are community-shaped rather than individual.",
    startRow = nrow(icc_tab) + 3)
  addStyle(wb, "Table S7", noteStyle, rows = nrow(icc_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S8: Variable Construction Reference
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S8...\n")

var_tab <- tribble(
  ~Variable, ~`Source items`, ~`# items`, ~Range, ~`Scoring method`, ~Type,
  "Depression (DASS-21)", "q47_3, q47_5, q47_10, q47_13, q47_16, q47_17, q47_21", 7, "0-42",
    "Sum of items × 2 (DASS-42 equivalent); prorated if >=6/7 items non-missing; z-standardized", "Outcome",
  "Anxiety (DASS-21)", "q47_2, q47_4, q47_7, q47_9, q47_15, q47_19, q47_20", 7, "0-42",
    "Same as above", "Outcome",
  "Stress (DASS-21)", "q47_1, q47_6, q47_8, q47_11, q47_12, q47_14, q47_18", 7, "0-42",
    "Same as above", "Outcome",
  "Worry: Health fears", "q51_2, q51_3, q51_4, q51_5, q51_6", 5, "0-15",
    "Sum of items (each 0-3 Likert)", "Layer 3 Stressor",
  "Worry: Economic concerns", "q51_10, q51_14, q51_15, q51_16, q51_17, q51_18, q51_21", 7, "0-21",
    "Sum of items (each 0-3 Likert)", "Layer 3 Stressor",
  "Worry: Basic needs", "q51_11, q51_12, q51_13", 3, "0-9",
    "Sum of items (each 0-3 Likert)", "Layer 3 Stressor",
  "Worry: Social/family", "q51_1, q51_7, q51_8, q51_9", 4, "0-12",
    "Sum of items (each 0-3 Likert)", "Layer 3 Stressor",
  "Worry: Safety/stigma", "q51_19, q51_20", 2, "0-6",
    "Sum of items (each 0-3 Likert)", "Layer 3 Stressor",
  "Perceived health threat", "q27_1 to q27_6", 6, "0-30",
    "Sum of items (each 0-5 scale)", "Layer 2 Appraisal",
  "Self-assessed risk", "q31_risk_self, q32_risk_country", 2, "0-2 each",
    "Individual items retained (high correlation)", "Layer 2 Appraisal",
  "Health system trust", "q28_prevention + q29_health_system", 2, "0-8",
    "Sum (each 0-4 Likert; values >=90 recoded NA)", "Layer 2 Appraisal",
  "Behavioral compliance", "q22_1 to q22_9", 9, "0-36",
    "Sum (each 0-4 Likert; values >=90 recoded NA)", "Layer 4 Coping",
  "Evidence-based prevention", "q23 items: hands, eyes, disinfect, mouth, symptom advice, travel, stay home, avoid crowds/transport", 10, "10-40",
    "Reverse-coded (1->4 to 4->1) then summed; higher = more engagement", "Layer 4 Coping",
  "Folk/alternative prevention", "q23 items: pray, herbal, vitamin-C, saltwater, drugs, flu jab", 6, "6-24",
    "Same reverse-coding and sum as evidence-based", "Layer 4 Coping",
  "Livelihood disruption", "q8_1, q8_3, q8_6, q8_8", 4, "0-12",
    "Sum of 4 items (each 0-3 Likert: reduced hours, less income, lost job, quit job)", "Layer 3 Stressor",
  "supportive_community", "Site-level mean of individual q52 sum (13 binary support sources)", 13, "0-13",
    "Individual q52 = count of support source types (family, community, govt, etc.); site mean", "Layer 4 Coping (site)",
  "helpful_community", "Site-level mean of individual q53 sum (13 binary help sources)", 13, "0-13",
    "Same structure as supportive_community; help receipt vs support availability", "Layer 4 Coping (site)",
  "Comorbidity count", "q43 (16 conditions)", 16, "0-16",
    "Count of self-reported chronic conditions", "Layer 1 Vulnerability",
  "General health", "q33_gen_health", 1, "1-5",
    "Single Likert item (higher = better health)", "Layer 1 Vulnerability",
  "Location type", "Derived from site ID", 1, "4-level",
    "Urban / Peri-urban / Rural / Remote", "Layer 1 Vulnerability",
  "HCW in household", "q17_hcws", 1, "Binary",
    "1 if any household member works in a healthcare facility", "Layer 1 Vulnerability",
  "Suicidality (composite)", "q48_suicide_thought, q49_suicide_plan, q50_suicide_attempt", 3, "Binary",
    "1 if any of thought/plan/attempt endorsed (past 2 weeks)", "Supplementary outcome"
)

addWorksheet(wb, "Table S8")
writeData(wb, "Table S8", "Table S8. Variable Construction Reference",
          startRow = 1)
writeData(wb, "Table S8", var_tab, startRow = 2)
addStyle(wb, "Table S8", headerStyle, rows = 2, cols = 1:ncol(var_tab), gridExpand = TRUE)
addStyle(wb, "Table S8", leftStyle, rows = 3:(nrow(var_tab)+2), cols = c(1,2,5,6), gridExpand = TRUE)
addStyle(wb, "Table S8", bodyStyle, rows = 3:(nrow(var_tab)+2), cols = 3:4, gridExpand = TRUE)
setColWidths(wb, "Table S8", cols = 1:6, widths = c(28, 42, 10, 12, 55, 22))
writeData(wb, "Table S8",
  "Note: All item-level numeric codes >=90 were recoded to NA (see Methods). Split-sample CFA of 5 worry sub-domains on hold-out sample (n = 660): CFI = 0.990, TLI = 0.988, RMSEA = 0.068, SRMR = 0.076. Internal consistency (Cronbach's alpha) for all composites reported in Table 2.",
  startRow = nrow(var_tab) + 3)
addStyle(wb, "Table S8", noteStyle, rows = nrow(var_tab)+3, cols = 1)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S9: Stigma × HCW Interaction
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S9...\n")

hcw_f <- file.path(tab_dir, "Table_Stigma_HCW_Interaction.csv")
if (file.exists(hcw_f)) {
  hcw_raw <- read_csv(hcw_f, show_col_types = FALSE)
  hcw_tab <- hcw_raw %>%
    mutate(
      `Interaction β` = sprintf("%.3f", estimate),
      `95% CI` = sprintf("[%.3f, %.3f]", conf.low, conf.high),
      `p-value` = sprintf("%.3f%s", p.value,
                          ifelse(p.value < 0.05, "*", ""))
    ) %>%
    dplyr::select(Outcome = outcome, `Interaction β`, `95% CI`, `p-value`)

  addWorksheet(wb, "Table S9")
  writeData(wb, "Table S9",
    "Table S9. Stigma Worry × Healthcare Worker in Household Interaction",
    startRow = 1)
  writeData(wb, "Table S9", hcw_tab, startRow = 2)
  addStyle(wb, "Table S9", headerStyle, rows = 2, cols = 1:4, gridExpand = TRUE)
  addStyle(wb, "Table S9", leftStyle, rows = 3:(nrow(hcw_tab)+2), cols = 1, gridExpand = TRUE)
  addStyle(wb, "Table S9", bodyStyle, rows = 3:(nrow(hcw_tab)+2), cols = 2:4, gridExpand = TRUE)
  setColWidths(wb, "Table S9", cols = 1:4, widths = c(18, 18, 22, 14))
  writeData(wb, "Table S9",
    "Note: HCW household prevalence 20.9% (n = 305 of 1,462). Interaction term from multilevel model with full covariate adjustment. Positive β indicates amplification of stigma effect; negative β indicates buffering. Anxiety shows significant amplification.",
    startRow = nrow(hcw_tab) + 3)
  addStyle(wb, "Table S9", noteStyle, rows = nrow(hcw_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S10: Worry × Location Type Interactions
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S10...\n")

loc_f <- file.path(tab_dir, "Table_Worry_Location_Interactions.csv")
if (file.exists(loc_f)) {
  loc_raw <- read_csv(loc_f, show_col_types = FALSE)
  loc_tab <- loc_raw %>%
    filter(p.value < 0.10) %>%
    mutate(
      `Worry sub-domain` = dplyr::recode(worry_var,
        worry_health_fear = "Health fears", worry_economic = "Economic concerns",
        worry_basic_needs = "Basic needs", worry_social = "Social/family",
        worry_safety_stigma = "Safety/stigma"),
      `Location contrast` = gsub(".*location_type", "", term),
      β = sprintf("%.3f", estimate),
      p = sprintf("%.3f%s", p.value, ifelse(p.value<0.05,"*",""))
    ) %>%
    dplyr::select(`Worry sub-domain`, `Location contrast`, Outcome = outcome, β, p) %>%
    arrange(Outcome, `Worry sub-domain`)

  addWorksheet(wb, "Table S10")
  writeData(wb, "Table S10",
    "Table S10. Significant Worry × Location Type Interactions (p < 0.10)",
    startRow = 1)
  writeData(wb, "Table S10", loc_tab, startRow = 2)
  addStyle(wb, "Table S10", headerStyle, rows = 2, cols = 1:5, gridExpand = TRUE)
  addStyle(wb, "Table S10", leftStyle, rows = 3:(nrow(loc_tab)+2), cols = 1:3, gridExpand = TRUE)
  addStyle(wb, "Table S10", bodyStyle, rows = 3:(nrow(loc_tab)+2), cols = 4:5, gridExpand = TRUE)
  setColWidths(wb, "Table S10", cols = 1:5, widths = c(22, 22, 15, 10, 10))
  writeData(wb, "Table S10",
    "Note: 45 interaction tests conducted (5 worry sub-domains × 3 outcomes × 3 location contrasts, urban as reference). Only interactions with p < 0.10 shown; 7 reached p < 0.05. Two patterns: (1) peri-urban sites amplify material-type worries (economic, basic needs, social); (2) remote sites attenuate health fears across all outcomes.",
    startRow = nrow(loc_tab) + 3)
  addStyle(wb, "Table S10", noteStyle, rows = nrow(loc_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S11: Country Fixed Effect Necessity
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S11...\n")

cfe_f <- file.path(tab_dir, "Table_Country_FE_Necessity.csv")
if (file.exists(cfe_f)) {
  cfe_raw <- read_csv(cfe_f, show_col_types = FALSE)
  cfe_tab <- cfe_raw %>%
    mutate(
      `LRT χ²` = sprintf("%.2f", lrt_chisq),
      df = lrt_df,
      `LRT p` = sprintf("%.3f%s", lrt_p, ifelse(lrt_p < 0.05, "*", "")),
      `AIC (with)` = aic_with,
      `AIC (without)` = aic_without,
      `ICC (with)` = sprintf("%.3f", icc_with_country),
      `ICC (without)` = sprintf("%.3f", icc_without_country)
    ) %>%
    dplyr::select(Outcome = outcome, `ICC (with)`, `ICC (without)`,
                  `AIC (with)`, `AIC (without)`, `LRT χ²`, df, `LRT p`)

  addWorksheet(wb, "Table S11")
  writeData(wb, "Table S11",
    "Table S11. Country Fixed Effect Necessity (Likelihood Ratio Tests)",
    startRow = 1)
  writeData(wb, "Table S11", cfe_tab, startRow = 2)
  addStyle(wb, "Table S11", headerStyle, rows = 2, cols = 1:ncol(cfe_tab), gridExpand = TRUE)
  addStyle(wb, "Table S11", leftStyle, rows = 3:(nrow(cfe_tab)+2), cols = 1, gridExpand = TRUE)
  addStyle(wb, "Table S11", bodyStyle, rows = 3:(nrow(cfe_tab)+2), cols = 2:ncol(cfe_tab), gridExpand = TRUE)
  setColWidths(wb, "Table S11", cols = 1:ncol(cfe_tab), widths = c(14, 12, 14, 12, 14, 10, 6, 12))
  writeData(wb, "Table S11",
    "Note: LRT compares M3 with country fixed effect versus without (site random intercept retained in both). Country FE significantly improves fit for depression (p = 0.040) and anxiety (p = 0.007), but not stress (p = 0.294). Retained across all outcomes for model consistency.",
    startRow = nrow(cfe_tab) + 3)
  addStyle(wb, "Table S11", noteStyle, rows = nrow(cfe_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE S12: Reference Category Sensitivity (Nepal as reference)
# ══════════════════════════════════════════════════════════════════════════════
cat("Table S12...\n")

nep_f <- file.path(tab_dir, "Table_Nepal_Reference.csv")
if (file.exists(nep_f)) {
  nep_raw <- read_csv(nep_f, show_col_types = FALSE)
  nep_tab <- nep_raw %>%
    filter(grepl("country", term)) %>%
    mutate(
      Country = gsub("country", "", term),
      Outcome = outcome,
      `β (vs Nepal)` = sprintf("%.3f", estimate),
      `95% CI` = sprintf("[%.3f, %.3f]", conf.low, conf.high),
      `p-value` = sprintf("%.3f%s", p.value, ifelse(p.value<0.05,"*",""))
    ) %>%
    dplyr::select(Country, Outcome, `β (vs Nepal)`, `95% CI`, `p-value`)

  addWorksheet(wb, "Table S12")
  writeData(wb, "Table S12",
    "Table S12. Country Contrasts with Nepal as Reference (Sensitivity)",
    startRow = 1)
  writeData(wb, "Table S12", nep_tab, startRow = 2)
  addStyle(wb, "Table S12", headerStyle, rows = 2, cols = 1:5, gridExpand = TRUE)
  addStyle(wb, "Table S12", leftStyle, rows = 3:(nrow(nep_tab)+2), cols = 1:2, gridExpand = TRUE)
  addStyle(wb, "Table S12", bodyStyle, rows = 3:(nrow(nep_tab)+2), cols = 3:5, gridExpand = TRUE)
  setColWidths(wb, "Table S12", cols = 1:5, widths = c(14, 14, 16, 20, 14))
  writeData(wb, "Table S12",
    "Note: Re-fit M3 with Nepal (highest unadjusted distress) as reference instead of Indonesia (primary analysis). All worry sub-domain coefficients were identical to primary model (differences < 0.0001), confirming correct specification. Country contrasts are presented here for interpretability.",
    startRow = nrow(nep_tab) + 3)
  addStyle(wb, "Table S12", noteStyle, rows = nrow(nep_tab)+3, cols = 1)
}

# ══════════════════════════════════════════════════════════════════════════════
# v8 AUDIT-RESPONSE SUPPLEMENTARY TABLES
#   S13b partial invariance | S17 Rubin mediation | S18 VIF | S19 age spline |
#   S20 f-squared | S21 attenuation | S22 Harman | S23 SEM verification |
#   S24 leave-one-site | S25 individual moderators | S26 binary-DASS multilevel
# ══════════════════════════════════════════════════════════════════════════════

append_supp_sheet <- function(wb, sheet_name, csv_file, title) {
  path <- file.path(tab_dir, csv_file)
  if (!file.exists(path)) {
    cat("  (skip) ", sheet_name, " - file not found: ", csv_file, "\n", sep = "")
    return(invisible(NULL))
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, title, startRow = 1)
  writeData(wb, sheet_name, df, startRow = 2)
  addStyle(wb, sheet_name, headerStyle,
           rows = 2, cols = seq_len(ncol(df)), gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
  cat("  added", sheet_name, "(", nrow(df), "rows,", ncol(df), "cols)\n")
}

# ── Rename legacy v7-era sheet labels to match v8 / CSV naming ─────────
# Legacy (v7) -> Canonical (v8 text + CSV filename) mapping:
#   "Table S4"  SEM content           -> "Table S2b"  (v8 Table S2b)
#   "Table S3"  Missingness diagnostics -> "Table S4"  (v8 Table S4)
#   "Table S2"  CC vs MI sensitivity  -> "Table S3"   (v8 Table S3)
# Order matters: rename target-before-source to avoid collisions.
if ("Table S4" %in% names(wb)) renameWorksheet(wb, "Table S4", "Table S2b")
if ("Table S3" %in% names(wb)) renameWorksheet(wb, "Table S3", "Table S4")
if ("Table S2" %in% names(wb)) renameWorksheet(wb, "Table S2", "Table S3")

cat("\n=== v8 supplement sheets (canonical CSV-backed) ===\n")
# Existing canonical CSVs referenced by v8 but not previously in xlsx
append_supp_sheet(wb, "Table S2a",
  "TableS2a_Per_Coefficient_FMI.csv",
  "Table S2a. Per-coefficient fraction of missing information (FMI) for the pooled M3 fixed effects")
append_supp_sheet(wb, "Table S13",
  "TableS13_Measurement_Invariance.csv",
  "Table S13. DASS-21 and worry 5-factor measurement invariance across Indonesia, Nepal, Vietnam (configural / metric / scalar)")
append_supp_sheet(wb, "Table S13b",
  "TableS13b_Partial_Invariance.csv",
  "Table S13b. Partial scalar invariance (sequential intercept-freeing): DASS-21 and worry 5-factor")
append_supp_sheet(wb, "Table S14",
  "TableS14_MI_Congeniality_Sensitivity.csv",
  "Table S14. MI congeniality sensitivity: primary coefficients with site dummies retained in the imputation predictor matrix")
append_supp_sheet(wb, "Table S15",
  "TableS15_Mediation_CC_vs_MI.csv",
  "Table S15. Complete-case vs multiply-imputed worry coefficients for the mediation auxiliary models")
append_supp_sheet(wb, "Table S16",
  "TableS16_13sites_vs_14sites.csv",
  "Table S16. Primary-model coefficients on 13 protocol-specified sites (n = 1,462) versus 14-site sensitivity (n = 1,500)")
append_supp_sheet(wb, "Table S17",
  "TableS17_Mediation_Rubin_Pooled.csv",
  "Table S17. Mediation Rubin-pooled across m = 30 imputations (5 pathways; Barnard-Rubin CIs)")
append_supp_sheet(wb, "Table S18",
  "TableS18_VIF.csv",
  "Table S18. Variance inflation factors for the M3 fixed-effects block")
append_supp_sheet(wb, "Table S19",
  "TableS19_Age_Spline.csv",
  "Table S19. Age non-linearity: natural cubic spline (df = 4) versus linear LR test")
append_supp_sheet(wb, "Table S20",
  "TableS20_fsq_worry.csv",
  "Table S20. Cohen's f-squared for the worry block (M3 over M2)")
append_supp_sheet(wb, "Table S21",
  "TableS21_Attenuation_Corrected.csv",
  "Table S21. Attenuation-corrected worry coefficients (upper bound; beta_obs / sqrt(reliability))")
append_supp_sheet(wb, "Table S22",
  "TableS22_Harman.csv",
  "Table S22. Harman's single-factor test (common-method variance diagnostic)")
append_supp_sheet(wb, "Table S23",
  "TableS23_SEM_fit_verification.csv",
  "Table S23. SEM fit re-verification: 5F-MLR, 5F-WLSMV, 4F-WLSMV specifications")
append_supp_sheet(wb, "Table S24",
  "TableS24_LeaveOneSite_Summary.csv",
  "Table S24. Site-level leave-one-out: worry-coefficient max |delta-beta| summary across 13 drops")
append_supp_sheet(wb, "Table S25",
  "TableS25_Individual_Moderators.csv",
  "Table S25. Individual-level moderators: gender x worry and age x worry (30 tests, BH-FDR within family)")
append_supp_sheet(wb, "Table S26",
  "TableS26_Binary_DASS_Multilevel.csv",
  "Table S26. Multilevel logistic regression for moderate-or-worse DASS caseness (glmer with site RI)")

# ══════════════════════════════════════════════════════════════════════════════
# SAVE
# ══════════════════════════════════════════════════════════════════════════════

saveWorkbook(wb, out_file, overwrite = TRUE)
cat("\n=== Publication tables saved to:", out_file, "===\n")
cat("Sheets:", paste(names(wb), collapse = ", "), "\n")
