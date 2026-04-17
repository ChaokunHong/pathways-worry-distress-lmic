################################################################################
# 24_binary_dass_multilevel.R
# Fix B5: Methods v7 Section 2.3.8(iii) describes the binary DASS sensitivity
# as "logistic multilevel models", but 06_sensitivity.R uses plain glm() with
# family=binomial (single-level). This script runs the proper multilevel
# logistic version using glmer(..., family=binomial).
#
# Output: TableS26_Binary_DASS_Multilevel.csv (15 worry x outcome rows)
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
})

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))

# DASS-21 binary cut-points (Lovibond & Lovibond 1995, severe/extremely severe)
# raw DASS-21 subscale thresholds for "moderate+" on DASS-42 scale after x2:
# Depression >= 14, Anxiety >= 10, Stress >= 19  (on DASS-42 scale)
ana <- d %>%
  filter(complete_dass, !is.na(site), site %in% 1:13) %>%
  drop_na(depr_any, anx_any, stress_any,
          worry_health_fear, worry_economic, worry_basic_needs,
          worry_social, worry_safety_stigma,
          age, female, ethnicity_minority, education, employment,
          comorbidity, gen_health, perceived_risk, risk_self,
          health_trust, livelihood_disruption, country)
cat("CC N =", nrow(ana), "\n")
cat("Prevalence: depr_any", round(mean(ana$depr_any), 3),
    "anx_any", round(mean(ana$anx_any), 3),
    "stress_any", round(mean(ana$stress_any), 3), "\n")

worries  <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
              "worry_social", "worry_safety_stigma")
outcomes <- c(depr_any   = "depression (bin)",
              anx_any    = "anxiety (bin)",
              stress_any = "stress (bin)")
covs <- c("age", "female", "ethnicity_minority", "education", "employment",
          "comorbidity", "gen_health", "perceived_risk", "risk_self",
          "health_trust", "livelihood_disruption", "country")

rows <- list()
for (y in names(outcomes)) {
  cat("Fitting multilevel logistic for", y, "... ")
  f <- as.formula(paste(y, "~",
                         paste(c(worries, covs), collapse = " + "),
                         "+ (1|site)"))
  fit <- tryCatch(glmer(f, data = ana, family = binomial,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5))),
                  error = function(e) {
                    message("glmer failed: ", e$message); NULL
                  })
  cat("done\n")
  if (is.null(fit)) next
  co <- summary(fit)$coefficients
  for (w in worries) {
    if (!w %in% rownames(co)) next
    rows[[paste(y, w, sep = "_")]] <- tibble(
      outcome = outcomes[y],
      term    = w,
      beta_logOR = round(co[w, "Estimate"], 4),
      se         = round(co[w, "Std. Error"], 4),
      OR         = round(exp(co[w, "Estimate"]), 3),
      OR_lo      = round(exp(co[w, "Estimate"] - 1.96 * co[w, "Std. Error"]), 3),
      OR_hi      = round(exp(co[w, "Estimate"] + 1.96 * co[w, "Std. Error"]), 3),
      p          = signif(co[w, "Pr(>|z|)"], 4)
    )
  }
}
res <- bind_rows(rows) %>%
  mutate(p_adj_BH = round(p.adjust(p, method = "BH"), 4),
         sig_BH   = ifelse(p_adj_BH < 0.05, "*", ""))
print(res, n = Inf)
write_csv(res, file.path(tab_dir, "TableS26_Binary_DASS_Multilevel.csv"))

cat("\n=== 24_binary_dass_multilevel.R COMPLETE ===\n")
