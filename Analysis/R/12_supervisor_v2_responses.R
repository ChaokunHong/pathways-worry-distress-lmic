# DEPRECATED (v7+ canonical): this script is preserved for historical traceability.
# The canonical 13-site sample writers for all downstream tables are
# 17_primary_13sites.R and 18_rerun_all_on_13sites.R. If you run THIS script
# standalone, its outputs may overwrite canonical CSVs with 14-site or old-filter
# content. Always re-run 17 and then 18 after running this script.
#
################################################################################
# 12_supervisor_v2_responses.R
# Address remaining v2 supervisor feedback items not yet covered:
# (1) Stigma × healthcare-worker-in-household interaction
# (2) Country fixed effect necessity (with vs without, given site random effect)
# (3) Sensitivity to Indonesia-as-reference (re-fit with Nepal as reference)
# (4) Worry × location_type interactions
# (5) Clarify supportive_community scoring (is it proportion or scale?)
################################################################################

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

proj <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
out_dir <- file.path(proj, "Analysis/output")
tab_dir <- file.path(proj, "Analysis/tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- dat[dat$complete_dass, ]

outcomes <- c("z_depression", "z_anxiety", "z_stress")
out_labels <- c("Depression", "Anxiety", "Stress")

covs <- "age + female + ethnicity_minority + education + employment +
         comorbidity + gen_health + perceived_risk + risk_self + health_trust +
         livelihood_disruption +
         worry_health_fear + worry_economic + worry_basic_needs +
         worry_social"

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: Clarify supportive_community scoring
# ═══════════════════════════════════════════════════════════════════════════════
cat("═══ PART 1: SUPPORTIVE_COMMUNITY SCORING ═══\n\n")

cat("supportive_community variable:\n")
cat("  Range observed:", round(range(ana$supportive_community, na.rm = TRUE), 3), "\n")
cat("  Mean:", round(mean(ana$supportive_community, na.rm = TRUE), 3), "\n")
cat("  SD:", round(sd(ana$supportive_community, na.rm = TRUE), 3), "\n")

# Check what q52_support_score (individual-level source) looks like
if ("support_score" %in% names(ana)) {
  cat("\nIndividual support_score (q52 sum, 13 items, binary 0/1 each):\n")
  cat("  Range:", round(range(ana$support_score, na.rm = TRUE), 1), "\n")
  cat("  Mean:", round(mean(ana$support_score, na.rm = TRUE), 2), "\n")
}

# How was supportive_community computed?
cat("\nDerivation: supportive_community = site-level mean of individual q52 sum scores\n")
cat("→ Range 0-13 means it is a SUM (count of support source types, averaged across site)\n")
cat("→ NOT a proportion (would be 0-1)\n")
cat("→ Interpretation: average number of support source types reported per person in that site\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: Stigma × healthcare worker in household
# ═══════════════════════════════════════════════════════════════════════════════
cat("═══ PART 2: STIGMA × HCW INTERACTION ═══\n\n")
cat("Test whether having a healthcare worker in household modifies stigma worry effects\n\n")

if (!"hcw_in_household" %in% names(ana)) {
  cat("hcw_in_household variable not found, creating from raw\n")
  # already created in 01 script as binary
}

cat("HCW in household prevalence:",
    round(mean(ana$hcw_in_household, na.rm = TRUE) * 100, 1), "%\n")
cat("n with HCW =", sum(ana$hcw_in_household == 1, na.rm = TRUE),
    "; n without =", sum(ana$hcw_in_household == 0, na.rm = TRUE), "\n\n")

stigma_hcw_results <- list()

for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  yl <- out_labels[oi]

  # Individual-level stigma × HCW
  f_int <- as.formula(paste(y,
    "~ worry_safety_stigma * hcw_in_household + country +",
    "age + female + ethnicity_minority + education + employment +",
    "comorbidity + gen_health + perceived_risk + risk_self + health_trust +",
    "livelihood_disruption + worry_health_fear + worry_economic +",
    "worry_basic_needs + worry_social + (1|site)"))

  m <- tryCatch(lmer(f_int, data = ana, REML = TRUE), error = function(e) NULL)
  if (!is.null(m)) {
    int_term <- "worry_safety_stigma:hcw_in_household"
    res <- tidy(m, conf.int = TRUE) %>% filter(term == int_term)
    if (nrow(res) > 0) {
      stigma_hcw_results[[yl]] <- res %>% mutate(outcome = yl)
      sig <- ifelse(res$p.value < 0.05, " *",
              ifelse(res$p.value < 0.10, " (trend)", ""))
      cat("  ", yl, ": β =", round(res$estimate, 4),
          " 95% CI [", round(res$conf.low, 3), ",", round(res$conf.high, 3), "]",
          " p =", round(res$p.value, 4), sig, "\n")
    }
  }
}

# Save
hcw_df <- bind_rows(stigma_hcw_results)
write_csv(hcw_df, file.path(tab_dir, "Table_Stigma_HCW_Interaction.csv"))
cat("\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART 3: Country fixed effect — necessity check
# ═══════════════════════════════════════════════════════════════════════════════
cat("═══ PART 3: COUNTRY FIXED EFFECT — NECESSARY? ═══\n\n")
cat("Compare M3 with vs without country fixed effect (site random effect retained)\n\n")

f_with <- "~ country + age + female + ethnicity_minority + education + employment +
            comorbidity + gen_health + perceived_risk + risk_self + health_trust +
            worry_health_fear + worry_economic + worry_basic_needs +
            worry_social + worry_safety_stigma + livelihood_disruption + (1|site)"

f_without <- gsub("country \\+ ", "", f_with)

country_compare <- list()
for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  yl <- out_labels[oi]

  m_with <- lmer(as.formula(paste(y, f_with)), data = ana, REML = FALSE)
  m_without <- lmer(as.formula(paste(y, f_without)), data = ana, REML = FALSE)

  # Likelihood ratio test
  lrt <- anova(m_without, m_with)

  # ICCs
  vc_with <- as.data.frame(VarCorr(m_with))
  icc_with <- vc_with$vcov[1] / sum(vc_with$vcov)
  vc_without <- as.data.frame(VarCorr(m_without))
  icc_without <- vc_without$vcov[1] / sum(vc_without$vcov)

  cat("─── ", yl, " ───\n")
  cat("  With country FE: ICC =", round(icc_with, 3),
      ", AIC =", round(AIC(m_with), 1), "\n")
  cat("  Without country FE: ICC =", round(icc_without, 3),
      ", AIC =", round(AIC(m_without), 1), "\n")
  cat("  LRT chi-sq =", round(lrt$Chisq[2], 2),
      "df =", lrt$`Df`[2],
      "p =", format(lrt$`Pr(>Chisq)`[2], digits = 4), "\n\n")

  country_compare[[yl]] <- tibble(
    outcome = yl,
    icc_with_country = round(icc_with, 3),
    icc_without_country = round(icc_without, 3),
    aic_with = round(AIC(m_with), 1),
    aic_without = round(AIC(m_without), 1),
    lrt_chisq = round(lrt$Chisq[2], 2),
    lrt_df = lrt$`Df`[2],
    lrt_p = lrt$`Pr(>Chisq)`[2]
  )
}

cc_df <- bind_rows(country_compare)
write_csv(cc_df, file.path(tab_dir, "Table_Country_FE_Necessity.csv"))

cat("Decision rule: country FE justified if LRT p < 0.05 (improves fit)\n")
all_sig <- all(cc_df$lrt_p < 0.05)
if (all_sig) {
  cat("→ Country FE significantly improves fit for ALL outcomes. Retain.\n\n")
} else {
  cat("→ Country FE marginal/not needed for some outcomes. Consider dropping.\n\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 4: Sensitivity to reference category (Nepal as reference)
# ═══════════════════════════════════════════════════════════════════════════════
cat("═══ PART 4: NEPAL AS REFERENCE (vs Indonesia) ═══\n\n")
cat("Re-fit M3 with country re-leveled to Nepal as reference\n")
cat("Worry sub-domain coefficients should NOT change (only country dummies do)\n\n")

ana_nepal <- ana %>% mutate(country = relevel(factor(country), ref = "Nepal"))

nepal_results <- list()
for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  yl <- out_labels[oi]

  m <- lmer(as.formula(paste(y, f_with)), data = ana_nepal, REML = TRUE)
  res <- tidy(m, conf.int = TRUE) %>%
    filter(grepl("worry_|country", term)) %>%
    mutate(outcome = yl)
  nepal_results[[yl]] <- res
}

nepal_df <- bind_rows(nepal_results) %>%
  mutate(across(c(estimate, std.error, conf.low, conf.high), ~ round(.x, 4)),
         p.value = round(p.value, 4))
write_csv(nepal_df, file.path(tab_dir, "Table_Nepal_Reference.csv"))

# Verify worry coefficients unchanged
cat("Worry sub-domain coefficients comparison (Indonesia ref vs Nepal ref):\n")
cat("(Should be identical — only country dummies change)\n")
indo_ref <- read.csv(file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv"))
for (w in c("worry_basic_needs", "worry_safety_stigma")) {
  for (yl in out_labels) {
    indo_b <- indo_ref$estimate[indo_ref$term == w & indo_ref$outcome == yl]
    nep_b  <- nepal_df$estimate[nepal_df$term == w & nepal_df$outcome == yl]
    cat("  ", w, "→", yl, ": Indo ref =", round(indo_b, 4),
        " Nepal ref =", round(nep_b, 4),
        ifelse(abs(indo_b - nep_b) < 0.01, " ✓", " ✗"), "\n")
  }
}

cat("\nCountry contrasts with Nepal as reference:\n")
nepal_country <- nepal_df %>% filter(grepl("country", term))
print(as.data.frame(nepal_country[, c("outcome","term","estimate","p.value")]),
      row.names = FALSE)

# ═══════════════════════════════════════════════════════════════════════════════
# PART 5: Worry × location_type interactions
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n═══ PART 5: WORRY × LOCATION_TYPE INTERACTIONS ═══\n\n")
cat("Tests whether worry-distress associations vary by community type\n")
cat("(supervisor noted Discussion claim about 'community types' lacked supporting analysis)\n\n")

loc_int_results <- list()
for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  yl <- out_labels[oi]
  cat("─── ", yl, " ───\n")

  for (w in c("worry_safety_stigma", "worry_basic_needs", "worry_social",
              "worry_economic", "worry_health_fear")) {
    other_worries <- setdiff(c("worry_safety_stigma", "worry_basic_needs", "worry_social",
                                "worry_economic", "worry_health_fear"), w)
    f_int <- as.formula(paste(y, "~",
      w, "* location_type + country +",
      paste(other_worries, collapse = " + "), "+",
      "age + female + ethnicity_minority + education + employment +",
      "comorbidity + gen_health + perceived_risk + risk_self + health_trust +",
      "livelihood_disruption + (1|site)"))

    m <- tryCatch(lmer(f_int, data = ana, REML = TRUE), error = function(e) NULL)
    if (!is.null(m)) {
      # Joint test of all interaction terms (3 dummies × worry)
      coefs <- tidy(m) %>% filter(grepl(paste0("^", w, ":location_type"), term))
      if (nrow(coefs) > 0) {
        any_sig <- any(coefs$p.value < 0.05)
        if (any_sig) {
          sig_rows <- coefs %>% filter(p.value < 0.05)
          for (i in 1:nrow(sig_rows)) {
            cat("  ", sig_rows$term[i], ": β =", round(sig_rows$estimate[i], 4),
                " p =", round(sig_rows$p.value[i], 4), " *\n")
          }
        }
        coefs$worry_var <- w
        coefs$outcome <- yl
        loc_int_results[[paste(yl, w)]] <- coefs
      }
    }
  }
}

loc_df <- bind_rows(loc_int_results)
write_csv(loc_df, file.path(tab_dir, "Table_Worry_Location_Interactions.csv"))

n_sig_loc <- sum(loc_df$p.value < 0.05, na.rm = TRUE)
n_total_loc <- nrow(loc_df)
cat("\nTotal worry × location_type interactions:", n_sig_loc, "/", n_total_loc,
    "significant (p<0.05)\n")

if (n_sig_loc == 0) {
  cat("→ No significant location-based moderation\n")
  cat("→ Discussion claim about 'community types' should be REMOVED or rewritten as null finding\n")
} else {
  cat("→ Some location-based moderation detected; results support Discussion claim\n")
}

cat("\n=== 12_supervisor_v2_responses.R COMPLETE ===\n")
