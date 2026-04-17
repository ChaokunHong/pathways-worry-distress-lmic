# DEPRECATED (v7+ canonical): this script is preserved for historical traceability.
# The canonical 13-site sample writers for all downstream tables are
# 17_primary_13sites.R and 18_rerun_all_on_13sites.R. If you run THIS script
# standalone, its outputs may overwrite canonical CSVs with 14-site or old-filter
# content. Always re-run 17 and then 18 after running this script.
#
################################################################################
# 10_context_effects.R
# Addressing supervisor's v2 feedback:
# (1) Stigma as individual vs community-level (contextual effect)
# (2) Community support × worry interactions (buffering effects)
# (3) ICC of worry sub-domains (how much lives at site level?)
################################################################################

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(mice)
library(mitml)

proj <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
out_dir <- file.path(proj, "Analysis/output")
tab_dir <- file.path(proj, "Analysis/tables")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- dat[dat$complete_dass, ]

# Compute site-level aggregates for worry sub-domains (cluster means)
site_means <- ana %>%
  group_by(site) %>%
  summarise(
    stigma_site = mean(worry_safety_stigma, na.rm = TRUE),
    hfear_site = mean(worry_health_fear, na.rm = TRUE),
    econ_site = mean(worry_economic, na.rm = TRUE),
    basic_site = mean(worry_basic_needs, na.rm = TRUE),
    social_site = mean(worry_social, na.rm = TRUE),
    .groups = "drop"
  )

ana <- ana %>% left_join(site_means, by = "site")

# Group-mean center individual worry (separates within-site from between-site)
ana <- ana %>%
  mutate(
    stigma_within  = worry_safety_stigma - stigma_site,
    hfear_within   = worry_health_fear - hfear_site,
    econ_within    = worry_economic - econ_site,
    basic_within   = worry_basic_needs - basic_site,
    social_within  = worry_social - social_site
  )

outcomes <- c("z_depression", "z_anxiety", "z_stress")
out_labels <- c("Depression", "Anxiety", "Stress")

covs <- "age + female + ethnicity_minority + education + employment +
         comorbidity + gen_health + perceived_risk + risk_self + health_trust +
         livelihood_disruption + country"

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: Stigma as Individual vs Community-Level (Contextual Effect)
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══ PART 1: STIGMA CONTEXTUAL EFFECT ═══\n\n")
cat("Decomposing stigma worry into within-site (individual) and between-site (community) components\n\n")

# ICC of stigma first
m_stigma_icc <- lmer(worry_safety_stigma ~ 1 + (1|site),
                     data = ana, REML = TRUE)
vc <- as.data.frame(VarCorr(m_stigma_icc))
icc_stigma <- vc$vcov[1] / sum(vc$vcov)
cat("ICC of stigma worry:", round(icc_stigma, 4),
    "→", round(icc_stigma*100, 1), "% of variance is between sites\n\n")

# Compare: individual-only vs decomposed
context_results <- list()

for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  yl <- out_labels[oi]
  cat("─── Outcome:", yl, "───\n")

  # Model A: individual stigma only (current M3 approach)
  f_a <- as.formula(paste(y, "~ worry_safety_stigma +",
                           "worry_health_fear + worry_economic + worry_basic_needs + worry_social +",
                           covs, "+ (1|site)"))
  m_a <- lmer(f_a, data = ana, REML = TRUE)

  # Model B: decomposed (within + between)
  f_b <- as.formula(paste(y, "~ stigma_within + stigma_site +",
                           "worry_health_fear + worry_economic + worry_basic_needs + worry_social +",
                           covs, "+ (1|site)"))
  m_b <- lmer(f_b, data = ana, REML = TRUE)

  # Extract
  a_tidy <- tidy(m_a, conf.int = TRUE) %>% filter(term == "worry_safety_stigma") %>%
    mutate(model = "Individual only", outcome = yl)

  b_tidy <- tidy(m_b, conf.int = TRUE) %>%
    filter(term %in% c("stigma_within", "stigma_site")) %>%
    mutate(model = "Decomposed", outcome = yl)

  context_results[[yl]] <- bind_rows(a_tidy, b_tidy)

  cat("  Individual stigma (current M3): β =",
      round(a_tidy$estimate, 3), " p =", round(a_tidy$p.value, 4), "\n")

  b_within <- b_tidy[b_tidy$term == "stigma_within", ]
  b_between <- b_tidy[b_tidy$term == "stigma_site", ]
  cat("  Within-site (individual): β =", round(b_within$estimate, 3),
      " p =", round(b_within$p.value, 4), "\n")
  cat("  Between-site (community): β =", round(b_between$estimate, 3),
      " p =", round(b_between$p.value, 4),
      "  95% CI [", round(b_between$conf.low,3), ",", round(b_between$conf.high,3), "]\n")

  # Contextual effect test (between - within)
  if (b_between$p.value < 0.05 & abs(b_between$estimate) > abs(b_within$estimate)) {
    cat("  → CONTEXTUAL effect detected: community > individual\n")
  } else if (b_between$p.value < 0.05) {
    cat("  → Both individual AND community matter\n")
  } else {
    cat("  → Effect is purely individual\n")
  }
  cat("\n")
}

context_df <- bind_rows(context_results)
write_csv(context_df, file.path(tab_dir, "Table_Stigma_Contextual.csv"))

# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: Community Support × Worry Interactions (Buffering)
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══ PART 2: COMMUNITY SUPPORT × WORRY INTERACTIONS ═══\n\n")
cat("Does site-level supportive community buffer individual worry effects?\n\n")

# Use supportive_community (site-level score, pre-computed)
# Test interactions with each worry sub-domain

int_results <- list()

for (oi in seq_along(outcomes)) {
  y <- outcomes[oi]
  yl <- out_labels[oi]
  cat("─── Outcome:", yl, "───\n")

  # Test support × each worry sub-domain
  for (w in c("worry_safety_stigma", "worry_basic_needs", "worry_social",
              "worry_economic", "worry_health_fear")) {
    other_worries <- setdiff(c("worry_safety_stigma", "worry_basic_needs", "worry_social",
                                "worry_economic", "worry_health_fear"), w)
    f_int <- as.formula(paste(y, "~",
      w, "* supportive_community +",
      paste(other_worries, collapse = " + "), "+",
      covs, "+ (1|site)"))
    m_int <- tryCatch(lmer(f_int, data = ana, REML = TRUE), error = function(e) NULL)

    if (!is.null(m_int)) {
      int_term <- paste0(w, ":supportive_community")
      tidy_res <- tidy(m_int, conf.int = TRUE) %>% filter(term == int_term)

      if (nrow(tidy_res) > 0) {
        int_results[[paste(yl, w)]] <- tidy_res %>%
          mutate(outcome = yl, worry_var = w)

        sig <- ifelse(tidy_res$p.value < 0.05, " *",
                     ifelse(tidy_res$p.value < 0.10, " (trend)", ""))
        cat("  ", w, " × supportive_community: β =",
            round(tidy_res$estimate, 4),
            " p =", round(tidy_res$p.value, 4), sig, "\n")
      }
    }
  }
  cat("\n")
}

int_df <- bind_rows(int_results)
write_csv(int_df, file.path(tab_dir, "Table_Support_Interactions.csv"))

# Count significant interactions
sig_count <- sum(int_df$p.value < 0.05, na.rm = TRUE)
cat("Total significant support × worry interactions:", sig_count, "/ 15\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART 3: ICC of All Worry Sub-Domains
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══ PART 3: WHERE DOES WORRY LIVE? (ICC of sub-domains) ═══\n\n")
cat("Proportion of variance in worry that is between-site (vs within):\n\n")

icc_worries <- list()
for (w in c("worry_health_fear", "worry_economic", "worry_basic_needs",
            "worry_social", "worry_safety_stigma")) {
  m <- lmer(as.formula(paste(w, "~ 1 + (1|site)")), data = ana, REML = TRUE)
  vc <- as.data.frame(VarCorr(m))
  icc <- vc$vcov[1] / sum(vc$vcov)
  icc_worries[[w]] <- round(icc, 4)
  cat(sprintf("  %-22s  ICC = %.3f  (%.1f%% between sites)\n",
              w, icc, icc*100))
}

icc_df <- tibble(
  worry_domain = names(icc_worries),
  icc = unlist(icc_worries),
  between_pct = round(unlist(icc_worries) * 100, 1)
)
write_csv(icc_df, file.path(tab_dir, "Table_Worry_ICC.csv"))

cat("\n→ High ICC means this worry varies more between communities than within\n")
cat("→ Supports treating those domains as community-level phenomena\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══ SUMMARY FOR SUPERVISOR ═══\n\n")
cat("Stigma contextual effect:\n")
stigma_summary <- context_df %>%
  filter(term == "stigma_site", p.value < 0.05) %>%
  dplyr::select(outcome, estimate, p.value, conf.low, conf.high)
if (nrow(stigma_summary) > 0) {
  print(as.data.frame(stigma_summary), row.names = FALSE)
} else {
  cat("  No significant community-level stigma effects\n")
}

cat("\nSupport × worry buffering interactions (p < 0.05):\n")
sig_int_summary <- int_df %>%
  filter(p.value < 0.05) %>%
  dplyr::select(outcome, worry_var, estimate, p.value)
if (nrow(sig_int_summary) > 0) {
  print(as.data.frame(sig_int_summary), row.names = FALSE)
} else {
  cat("  No significant buffering interactions detected\n")
}

cat("\nWorry domains with highest ICC (most community-level):\n")
print(as.data.frame(icc_df[order(-icc_df$icc), ]), row.names = FALSE)

cat("\n=== 10_context_effects.R COMPLETE ===\n")
