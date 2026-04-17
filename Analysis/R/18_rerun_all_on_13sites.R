################################################################################
# 18_rerun_all_on_13sites.R
# Consolidated re-run of every downstream analysis on the protocol-faithful
# 13-site primary sample (n = 1,462). Overwrites canonical table files so
# that all downstream figures and text refer to a single authoritative source.
#
# Sensitivity analyses that use the 14-site ("Vietnam other" retained) sample
# remain available in _v14sites.csv files produced earlier.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(broom.mixed)
  library(mice)
  library(mitml)
  library(MuMIn)
  library(mediation)
  library(gtsummary)
})

select <- dplyr::select

proj <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
out_dir <- file.path(proj, "Analysis/output")
tab_dir <- file.path(proj, "Analysis/tables")
fig_dir <- file.path(proj, "Analysis/figures")
archive_dir <- file.path(tab_dir, "archive_14sites")
dir.create(archive_dir, showWarnings = FALSE)

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))

# === PRIMARY ANALYTIC SAMPLE (13 protocol sites) =============================
ana <- dat %>% filter(complete_dass, !is.na(site), site %in% 1:13)
cat("Primary analytic sample: n =", nrow(ana), "| sites =",
    length(unique(ana$site)), "\n")

# Archive old 14-site canonical files before overwriting
to_archive <- c("Table1_Sample_Characteristics.csv",
                "Table1_Sample_Characteristics.html",
                "Table3_correlation_matrix.csv",
                "Table5_Mediation_Results.csv",
                "Table6_Country_Stratified_Multilevel.csv",
                "Table_Country_FE_Necessity.csv",
                "Table_Nepal_Reference.csv",
                "Table_Stigma_Contextual.csv",
                "Table_Stigma_HCW_Interaction.csv",
                "Table_Support_Interactions.csv",
                "Table_Worry_Location_Interactions.csv",
                "Table_Worry_ICC.csv",
                "Table4_Primary_Results_MI.csv",
                "R2_decomposition_MI.csv")
for (f in to_archive) {
  src <- file.path(tab_dir, f)
  if (file.exists(src)) file.copy(src, file.path(archive_dir, f), overwrite = TRUE)
}
cat("Archived 14-site outputs to:", archive_dir, "\n\n")

################################################################################
# (1) Table 1 — Sample characteristics by country (n = 1,462)
################################################################################
cat("== Table 1 ==\n")
tab1 <- ana %>%
  mutate(country = factor(country, levels = c("Indonesia","Nepal","Vietnam"))) %>%
  select(country, age, female, ethnicity_minority, education, employment,
         comorbidity, gen_health, livelihood_disruption, perceived_risk,
         risk_self, health_trust,
         worry_health_fear, worry_economic, worry_basic_needs,
         worry_social, worry_safety_stigma,
         depression, anxiety, stress) %>%
  tbl_summary(
    by = country,
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  add_p()

as_tibble(tab1$table_body) %>%
  write_csv(file.path(tab_dir, "Table1_Sample_Characteristics.csv"))
cat("  Saved Table1_Sample_Characteristics.csv\n")

################################################################################
# (2) Table 3 — Correlation matrix among key continuous variables
################################################################################
cat("== Table 3 ==\n")
cvars <- c("age","education","comorbidity",
           "worry_health_fear","worry_economic","worry_basic_needs",
           "worry_social","worry_safety_stigma",
           "livelihood_disruption","perceived_risk","risk_self",
           "health_trust",
           "z_depression","z_anxiety","z_stress")
cm <- cor(ana[, cvars], use = "pairwise.complete.obs")
write.csv(cm, file.path(tab_dir, "Table3_correlation_matrix.csv"))
cat("  Saved Table3_correlation_matrix.csv\n")

################################################################################
# (3) Worry ICC (unconditional)
################################################################################
cat("== Worry ICC ==\n")
icc_rows <- list()
for (v in c("worry_health_fear","worry_economic","worry_basic_needs",
            "worry_social","worry_safety_stigma")) {
  m <- lmer(as.formula(paste(v, "~ (1|site)")),
            data = ana, REML = TRUE)
  vc <- as.data.frame(VarCorr(m))
  tau2 <- vc$vcov[vc$grp == "site"]
  sig2 <- vc$vcov[vc$grp == "Residual"]
  icc_rows[[v]] <- data.frame(worry_domain = v,
                              icc = round(tau2 / (tau2 + sig2), 4),
                              between_pct = round(100 * tau2 / (tau2 + sig2), 1))
}
write_csv(bind_rows(icc_rows), file.path(tab_dir, "Table_Worry_ICC.csv"))
print(bind_rows(icc_rows))

################################################################################
# (4) Primary multilevel (complete-case comparator, used by many downstream)
################################################################################
cat("== Primary multilevel CC (n = 1,462) ==\n")
f_m3_cc <- "~ country + age + female + ethnicity_minority + education +
             employment + comorbidity + gen_health +
             perceived_risk + risk_self + health_trust +
             worry_health_fear + worry_economic + worry_basic_needs +
             worry_social + worry_safety_stigma +
             livelihood_disruption + (1|site)"

fit_cc <- function(y) {
  m <- lmer(as.formula(paste(y, f_m3_cc)), data = ana, REML = TRUE)
  broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed") %>%
    mutate(outcome = sub("^z_", "", y))
}
cc_all <- bind_rows(lapply(c("z_depression","z_anxiety","z_stress"), fit_cc))
cc_all$outcome <- tools::toTitleCase(cc_all$outcome)
write_csv(cc_all, file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv"))
cat("  Saved Table4_Primary_Results_Multilevel.csv (CC comparator)\n")

################################################################################
# (5) Table 6 — Country-stratified M3 (complete-case)
################################################################################
cat("== Table 6 country-stratified ==\n")
cs_rows <- list()
for (cc in c("Indonesia","Nepal","Vietnam")) {
  sub <- ana %>% filter(country == cc)
  for (y in c("z_depression","z_anxiety","z_stress")) {
    m <- lmer(as.formula(paste(y, sub("country \\+ ", "", f_m3_cc))),
              data = sub, REML = TRUE)
    td <- broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed") %>%
      mutate(country = cc, outcome = sub("^z_", "", y))
    cs_rows[[paste(cc, y)]] <- td
  }
}
cs_all <- bind_rows(cs_rows)
cs_all$outcome <- tools::toTitleCase(cs_all$outcome)
write_csv(cs_all, file.path(tab_dir, "Table6_Country_Stratified_Multilevel.csv"))
cat("  Saved Table6_Country_Stratified_Multilevel.csv\n")

################################################################################
# (6) Stigma contextual decomposition
################################################################################
cat("== Stigma contextual ==\n")
site_means <- ana %>%
  group_by(site) %>%
  summarise(stigma_site = mean(worry_safety_stigma, na.rm = TRUE),
            .groups = "drop")
ana2 <- ana %>%
  left_join(site_means, by = "site") %>%
  mutate(stigma_within = worry_safety_stigma - stigma_site)

stig_rows <- list()
base_fmla <- "country + age + female + ethnicity_minority + education +
              employment + comorbidity + gen_health +
              perceived_risk + risk_self + health_trust +
              worry_health_fear + worry_economic + worry_basic_needs +
              worry_social + livelihood_disruption + (1|site)"

for (y in c("z_depression","z_anxiety","z_stress")) {
  # Individual only
  f_ind <- paste(y, "~", base_fmla, "+ worry_safety_stigma")
  m_ind <- lmer(as.formula(f_ind), data = ana2, REML = TRUE)
  # Decomposed
  f_dec <- paste(y, "~", base_fmla, "+ stigma_within + stigma_site")
  m_dec <- lmer(as.formula(f_dec), data = ana2, REML = TRUE)

  for (term in c("worry_safety_stigma")) {
    td <- broom.mixed::tidy(m_ind, conf.int = TRUE, effects = "fixed") %>%
      filter(term == !!term) %>%
      mutate(model = "Individual only", outcome = sub("^z_","",y))
    stig_rows[[paste0(y,"_ind")]] <- td
  }
  for (term in c("stigma_within","stigma_site")) {
    td <- broom.mixed::tidy(m_dec, conf.int = TRUE, effects = "fixed") %>%
      filter(term == !!term) %>%
      mutate(model = "Decomposed", outcome = sub("^z_","",y))
    stig_rows[[paste0(y,"_dec_",term)]] <- td
  }
}
stig_out <- bind_rows(stig_rows)
stig_out$outcome <- tools::toTitleCase(stig_out$outcome)
write_csv(stig_out, file.path(tab_dir, "Table_Stigma_Contextual.csv"))
cat("  Saved Table_Stigma_Contextual.csv\n")

################################################################################
# (7) Stigma × HCW interaction
################################################################################
cat("== Stigma × HCW ==\n")
if ("hcw_in_household" %in% names(ana)) {
  hcw_rows <- list()
  for (y in c("z_depression","z_anxiety","z_stress")) {
    f <- paste(y, "~", base_fmla,
               "+ worry_safety_stigma * hcw_in_household")
    m <- lmer(as.formula(f), data = ana, REML = TRUE)
    td <- broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed") %>%
      filter(grepl("worry_safety_stigma:hcw_in_household", term)) %>%
      mutate(outcome = sub("^z_","",y))
    hcw_rows[[y]] <- td
  }
  hcw_out <- bind_rows(hcw_rows)
  hcw_out$outcome <- tools::toTitleCase(hcw_out$outcome)
  write_csv(hcw_out, file.path(tab_dir, "Table_Stigma_HCW_Interaction.csv"))
  cat("  Saved Table_Stigma_HCW_Interaction.csv\n")
}

################################################################################
# (8) Support × worry interactions (15 tests)
################################################################################
cat("== Support × worry interactions ==\n")
supp_rows <- list()
for (w in c("worry_health_fear","worry_economic","worry_basic_needs",
            "worry_social","worry_safety_stigma")) {
  for (y in c("z_depression","z_anxiety","z_stress")) {
    f <- paste(y, "~", base_fmla, "+ worry_safety_stigma +",
               w, "* supportive_community")
    # rebuild formula to avoid duplicating focal worry term
    base2 <- gsub(paste0("\\+ ", w, " "), "", base_fmla)
    f <- paste(y, "~", base2, "+ worry_safety_stigma +",
               w, "* supportive_community")
    m <- tryCatch(lmer(as.formula(f), data = ana, REML = TRUE),
                  error = function(e) NULL)
    if (is.null(m)) next
    td <- broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed") %>%
      filter(grepl(paste0(w, ":supportive_community"), term)) %>%
      mutate(worry_domain = w, worry_var = w, outcome = sub("^z_","",y))
    supp_rows[[paste(w,y)]] <- td
  }
}
supp_out <- bind_rows(supp_rows)
supp_out$outcome <- tools::toTitleCase(supp_out$outcome)
write_csv(supp_out, file.path(tab_dir, "Table_Support_Interactions.csv"))
cat("  Saved Table_Support_Interactions.csv (", nrow(supp_out), "tests)\n")

################################################################################
# (9) Worry × location interactions (45 tests)
################################################################################
cat("== Worry × location interactions ==\n")
loc_rows <- list()
for (w in c("worry_health_fear","worry_economic","worry_basic_needs",
            "worry_social","worry_safety_stigma")) {
  for (y in c("z_depression","z_anxiety","z_stress")) {
    base2 <- gsub(paste0("\\+ ", w, " "), "", base_fmla)
    f <- paste(y, "~", base2, "+ worry_safety_stigma +",
               w, "* location_type")
    m <- tryCatch(lmer(as.formula(f), data = ana, REML = TRUE),
                  error = function(e) NULL)
    if (is.null(m)) next
    td <- broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed") %>%
      filter(grepl(paste0(w, ":location_type"), term)) %>%
      mutate(worry_domain = w, worry_var = w, outcome = sub("^z_","",y))
    loc_rows[[paste(w,y)]] <- td
  }
}
loc_out <- bind_rows(loc_rows)
loc_out$outcome <- tools::toTitleCase(loc_out$outcome)
write_csv(loc_out, file.path(tab_dir, "Table_Worry_Location_Interactions.csv"))
cat("  Saved Table_Worry_Location_Interactions.csv (", nrow(loc_out), "tests)\n")

################################################################################
# (10) Country FE necessity — LRT
################################################################################
cat("== Country FE necessity LRT ==\n")
icc_from <- function(m) {
  v <- as.data.frame(VarCorr(m))
  tau <- v$vcov[v$grp == "site"]
  sig <- v$vcov[v$grp == "Residual"]
  tau / (tau + sig)
}
ctry_rows <- list()
for (y in c("z_depression","z_anxiety","z_stress")) {
  base_no_cty <- gsub("country \\+ ", "", f_m3_cc)
  m_full <- lmer(as.formula(paste(y, f_m3_cc)), data = ana, REML = FALSE)
  m_no   <- lmer(as.formula(paste(y, base_no_cty)),
                 data = ana, REML = FALSE)
  an <- anova(m_no, m_full)
  ctry_rows[[y]] <- data.frame(
    outcome = sub("^z_","",y),
    aic_with = round(AIC(m_full), 1),
    aic_without = round(AIC(m_no), 1),
    chisq = round(an$Chisq[2], 3),
    lrt_chisq = round(an$Chisq[2], 3),
    df = an$Df[2],
    lrt_df = an$Df[2],
    p = round(an[["Pr(>Chisq)"]][2], 4),
    p_value = round(an[["Pr(>Chisq)"]][2], 4),
    lrt_p = round(an[["Pr(>Chisq)"]][2], 4),
    icc_with_country = round(icc_from(m_full), 3),
    icc_without_country = round(icc_from(m_no), 3))
}
ctry_out <- bind_rows(ctry_rows)
ctry_out$outcome <- tools::toTitleCase(ctry_out$outcome)
write_csv(ctry_out, file.path(tab_dir, "Table_Country_FE_Necessity.csv"))
print(ctry_out)

################################################################################
# (11) Nepal as reference sensitivity
################################################################################
cat("== Nepal reference sensitivity ==\n")
ana_nep <- ana %>%
  mutate(country = relevel(factor(country), ref = "Nepal"))
nep_rows <- list()
for (y in c("z_depression","z_anxiety","z_stress")) {
  m <- lmer(as.formula(paste(y, f_m3_cc)), data = ana_nep, REML = TRUE)
  td <- broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed") %>%
    filter(grepl("^worry_|country", term)) %>%
    mutate(outcome = sub("^z_","",y))
  nep_rows[[y]] <- td
}
nep_out <- bind_rows(nep_rows)
nep_out$outcome <- tools::toTitleCase(nep_out$outcome)
write_csv(nep_out, file.path(tab_dir, "Table_Nepal_Reference.csv"))
cat("  Saved Table_Nepal_Reference.csv\n")

################################################################################
# (12) Mediation (complete case; 1,000 bootstrap)
################################################################################
cat("== Mediation ==\n")
set.seed(42)
mediation_out <- list()

covs_s <- "age + female + ethnicity_minority + education + comorbidity + gen_health + country"

# Inline mediation (package has scope issues with functions)
run_path <- function(iv, med, dv, lbl) {
  sub <<- ana %>% filter(!is.na(.data[[iv]]), !is.na(.data[[med]]),
                         !is.na(.data[[dv]]))
  f_med <<- as.formula(paste(med, "~", iv, "+", covs_s))
  f_out <<- as.formula(paste(dv, "~", iv, "+", med, "+", covs_s))
  m_mod <<- lm(f_med, data = sub)
  o_mod <<- lm(f_out, data = sub)
  me <- mediation::mediate(m_mod, o_mod, treat = iv, mediator = med,
                           boot = TRUE, sims = 1000)
  data.frame(pathway = lbl,
             label = lbl,
             ACME = round(me$d0, 4),
             ACME_lo = round(me$d0.ci[1], 4),
             ACME_hi = round(me$d0.ci[2], 4),
             ACME_p = round(me$d0.p, 4),
             ade = round(me$z0, 4),
             ade_ci_low = round(me$z0.ci[1], 4),
             ade_ci_high = round(me$z0.ci[2], 4),
             ade_p = round(me$z0.p, 4),
             prop_med = round(me$n0, 3),
             total = round(me$tau.coef, 4),
             n = nrow(sub))
}

mediation_out[[1]] <- run_path("livelihood_disruption", "worry_economic",
  "z_depression", "Livelihood -> Economic -> Depression")
mediation_out[[2]] <- run_path("livelihood_disruption", "worry_basic_needs",
  "z_depression", "Livelihood -> Basic needs -> Depression")
mediation_out[[3]] <- run_path("risk_self", "worry_health_fear",
  "z_anxiety", "Risk -> Health fear -> Anxiety")
mediation_out[[4]] <- run_path("risk_self", "worry_health_fear",
  "z_stress", "Risk -> Health fear -> Stress")
mediation_out[[5]] <- run_path("risk_self", "worry_safety_stigma",
  "z_depression", "Risk -> Stigma -> Depression")

med_tab <- bind_rows(mediation_out) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  mutate(acme = ACME,
         acme_ci_low = ACME_lo,
         acme_ci_high = ACME_hi,
         acme_p = ACME_p,
         pct_mediated = prop_med,
         prop_mediated = prop_med,
         pct_lab = sprintf("%.0f%%", 100 * prop_med))
write_csv(med_tab, file.path(tab_dir, "Table5_Mediation_Results.csv"))
cat("  Saved Table5_Mediation_Results.csv\n")
print(med_tab)

################################################################################
# (13) Canonicalise MI primary: copy _13sites outputs onto bare names
################################################################################
# Remap column names to canonical format expected by 07_figures.R and 08_publication_tables.R
tbl13 <- read_csv(file.path(tab_dir, "Table4_Primary_Results_MI_13sites.csv"),
                  show_col_types = FALSE)
names(tbl13)[names(tbl13) == "p_value"] <- "p.value"
names(tbl13)[names(tbl13) == "se"] <- "std.error"
# Recompute conf intervals (approximate using Satterthwaite df & t critical)
tbl13$conf.low <- tbl13$estimate - qt(0.975, df = pmax(tbl13$df, 2)) * tbl13$std.error
tbl13$conf.high <- tbl13$estimate + qt(0.975, df = pmax(tbl13$df, 2)) * tbl13$std.error
tbl13$statistic <- tbl13$estimate / tbl13$std.error
tbl13$sig <- ifelse(tbl13$p.value < 0.001, "***",
              ifelse(tbl13$p.value < 0.01, "**",
               ifelse(tbl13$p.value < 0.05, "*", "")))
# Strip z_ prefix so downstream scripts get "Depression" etc.
tbl13$outcome <- tools::toTitleCase(sub("^z_","", tbl13$outcome))
write_csv(tbl13, file.path(tab_dir, "Table4_Primary_Results_MI.csv"))

# R2 remap (08_publication_tables expects R2m_Depression etc.)
r2_13 <- read_csv(file.path(tab_dir, "R2_decomposition_MI_13sites.csv"),
                  show_col_types = FALSE)
r2_wide <- r2_13 %>%
  select(model, outcome, R2m, R2c) %>%
  mutate(outcome = tools::toTitleCase(sub("^z_","", outcome))) %>%
  pivot_wider(names_from = outcome, values_from = c(R2m, R2c), names_sep = "_") %>%
  rename_with(~ gsub("^R2m_","R2m_",.x)) %>%
  mutate(n = nrow(ana))
# Compute delta per outcome (Mi vs M{i-1})
for (o in c("Depression","Anxiety","Stress")) {
  col <- paste0("R2m_", o)
  r2_wide[[paste0("delta_", o)]] <- c(NA, diff(r2_wide[[col]]))
  r2_wide[[paste0("delta_", o)]][1] <- r2_wide[[col]][1]
}
write_csv(r2_wide, file.path(tab_dir, "R2_decomposition_MI.csv"))
cat("Canonicalised MI files: Table4_Primary_Results_MI.csv, R2_decomposition_MI.csv\n")

cat("\n=== 18_rerun_all_on_13sites.R COMPLETE ===\n")
