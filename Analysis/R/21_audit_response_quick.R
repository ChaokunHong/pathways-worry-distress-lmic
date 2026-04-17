################################################################################
# 21_audit_response_quick.R
# Fast-running audit-response analyses (VIF, residuals, spline, f-squared,
# attenuation, Harman, SEM re-verification, Heywood identification).
#
# Addresses audit findings:
#   - G1: VIF for 5 worry sub-domains                    -> TableS18_VIF.csv
#   - SHOULD-ADD: M3 residual diagnostics                -> FigureS2
#   - SHOULD-ADD: age nonlinearity via spline LR test    -> TableS19_Age_Spline.csv
#   - SHOULD-ADD: Cohen's f-squared for worry block      -> TableS20_fsq.csv
#   - SHOULD-ADD: attenuation-corrected beta upper bound -> TableS21_Attenuation.csv
#   - SHOULD-ADD: Harman's single-factor test            -> TableS22_Harman.csv
#   - B1: re-verify SEM 5F-MLR fit exact numbers         -> log output
#   - B2: identify Heywood factor/item                   -> log output
#
# All outputs are NEW supplement tables; does not overwrite any canonical file.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4); library(lmerTest)
  library(car)
  library(lavaan)
  library(psych)
  library(splines)
})

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- d %>%
  filter(complete_dass, !is.na(site), site %in% 1:13)
cat("Analytic sample: n =", nrow(ana), "\n")

covs <- c("age", "female", "ethnicity_minority", "education",
          "comorbidity", "gen_health", "perceived_risk", "risk_self",
          "health_trust", "livelihood_disruption", "country")
worries <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
             "worry_social", "worry_safety_stigma")
outcomes <- c("z_depression", "z_anxiety", "z_stress")

# ── G1 · VIF among 5 worry sub-domains (plus covariates) ─────────────────
cat("\n== [G1] VIF for M3 fixed-effects block ==\n")
vif_rows <- list()
for (y in outcomes) {
  f <- as.formula(paste(y, "~", paste(c(worries, covs), collapse = " + ")))
  # Fit on CC complete sample to get a stable VIF diagnostic
  fit <- lm(f, data = ana)
  v <- car::vif(fit)
  if (is.matrix(v)) v <- v[, "GVIF^(1/(2*Df))"]^2
  vif_df <- tibble(outcome = y, term = names(v), vif = round(v, 3))
  vif_rows[[y]] <- vif_df
}
vif_tab <- bind_rows(vif_rows) %>%
  pivot_wider(names_from = outcome, values_from = vif)
print(vif_tab, n = Inf)
write_csv(vif_tab, file.path(tab_dir, "TableS18_VIF.csv"))
cat("\nMax VIF across any worry term:",
    max(vif_tab %>% filter(term %in% worries) %>% select(-term), na.rm = TRUE), "\n")
cat("Rule of thumb: VIF < 5 = acceptable; < 2.5 = comfortable.\n")

# ── M3 residual diagnostics (Figure S2) ──────────────────────────────────
cat("\n== [Residual diagnostics] M3 on CC ==\n")
m3_cc <- list()
for (y in outcomes) {
  f <- as.formula(paste(y, "~", paste(c(worries, covs[covs != "country"]),
                                       collapse = " + "),
                         "+ country + (1|site)"))
  m3_cc[[y]] <- lmer(f, data = ana, REML = TRUE)
}

# Helper: draw the 2×3 residual panel (shared by PDF and PNG devices).
# `oma` gives the outer-margin strip inside which `box("outer")` draws the
# figure-wide grey frame (matches the frame used on all ggplot figures).
draw_residual_panels <- function() {
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1))
  for (y in outcomes) {
    fit <- m3_cc[[y]]
    fv  <- fitted(fit); rs <- resid(fit)
    plot(fv, rs, pch = 19, cex = 0.4,
         col = adjustcolor("#4E8EC7", 0.4),
         xlab = "Fitted", ylab = "Residual",
         main = paste("Residual vs fitted:", y))
    abline(h = 0, lty = 2, col = "grey40")
    qqnorm(rs, pch = 19, cex = 0.4,
           col = adjustcolor("#4E8EC7", 0.4),
           main = paste("QQ plot:", y))
    qqline(rs, lty = 2, col = "grey40")
  }
  box("outer", col = "#aaaaaa", lwd = 1)
}

pdf(file.path(fig_dir, "FigureS2_M3_Residuals.pdf"),
    width = 10, height = 7)
draw_residual_panels()
dev.off()

png(file.path(fig_dir, "FigureS2_M3_Residuals.png"),
    width = 10, height = 7, units = "in", res = 300)
draw_residual_panels()
dev.off()

cat("Saved FigureS2_M3_Residuals.{pdf,png}\n")

# ── Age nonlinearity: spline vs linear LR test ───────────────────────────
cat("\n== [Age nonlinearity] natural cubic spline (df=4) vs linear ==\n")
age_rows <- list()
for (y in outcomes) {
  f_lin <- as.formula(paste(y, "~", paste(c(worries, covs[covs != "age"]),
                                          collapse = " + "),
                            "+ age + (1|site)"))
  f_spl <- as.formula(paste(y, "~", paste(c(worries, covs[covs != "age"]),
                                          collapse = " + "),
                            "+ ns(age, 4) + (1|site)"))
  f_lin <- update(f_lin, . ~ . - country + country)
  f_spl <- update(f_spl, . ~ . - country + country)
  m_lin <- lmer(f_lin, data = ana, REML = FALSE)
  m_spl <- lmer(f_spl, data = ana, REML = FALSE)
  lr <- anova(m_lin, m_spl)
  age_rows[[y]] <- tibble(outcome = y,
                          lr_chisq = round(lr$Chisq[2], 3),
                          df = lr$Df[2],
                          p = signif(lr[["Pr(>Chisq)"]][2], 3))
}
age_tab <- bind_rows(age_rows)
print(age_tab)
write_csv(age_tab, file.path(tab_dir, "TableS19_Age_Spline.csv"))

# ── Cohen's f-squared for worry block (M3 vs M2) ─────────────────────────
cat("\n== [Cohen f^2] worry block effect ==\n")
# f^2 = (R2_full - R2_reduced) / (1 - R2_full)
fsq_rows <- list()
for (y in outcomes) {
  f_m2 <- as.formula(paste(y, "~", paste(covs, collapse = " + "),
                           "+ (1|site)"))
  f_m3 <- as.formula(paste(y, "~", paste(c(worries, covs), collapse = " + "),
                           "+ (1|site)"))
  m2 <- lmer(f_m2, data = ana, REML = FALSE)
  m3 <- lmer(f_m3, data = ana, REML = FALSE)
  r2_m2 <- MuMIn::r.squaredGLMM(m2)[1, "R2m"]
  r2_m3 <- MuMIn::r.squaredGLMM(m3)[1, "R2m"]
  fsq   <- (r2_m3 - r2_m2) / (1 - r2_m3)
  fsq_rows[[y]] <- tibble(outcome = y,
                          R2m_M2 = round(r2_m2, 4),
                          R2m_M3 = round(r2_m3, 4),
                          delta_R2 = round(r2_m3 - r2_m2, 4),
                          cohen_f2 = round(fsq, 4))
}
fsq_tab <- bind_rows(fsq_rows)
print(fsq_tab)
write_csv(fsq_tab, file.path(tab_dir, "TableS20_fsq_worry.csv"))
cat("Cohen benchmarks: f^2 = .02 (small), .15 (medium), .35 (large)\n")

# ── Attenuation-corrected beta for low-reliability worry scales ─────────
cat("\n== [Attenuation] corrected upper bound for SS, SOC ==\n")
# beta_corrected = beta_observed / reliability(predictor)
rel <- c(worry_health_fear    = 0.871,
         worry_economic       = 0.818,
         worry_basic_needs    = 0.838,
         worry_social         = 0.681,   # Cronbach alpha
         worry_safety_stigma  = 0.864)   # Spearman-Brown
# Take observed coefficients from the primary canonical table
primary <- read_csv(file.path(tab_dir, "Table4_Primary_Results_MI.csv"),
                    show_col_types = FALSE)
worry_rows <- primary %>% filter(grepl("^worry_", term))
att <- worry_rows %>%
  mutate(reliability = rel[term],
         beta_corrected = round(estimate / sqrt(reliability), 4),
         se_corrected   = round(std.error / sqrt(reliability), 4))
print(att %>% select(term, outcome, estimate, reliability,
                      beta_corrected, std.error, se_corrected))
write_csv(att, file.path(tab_dir, "TableS21_Attenuation_Corrected.csv"))

# ── Harman's single-factor test (CMV) ────────────────────────────────────
cat("\n== [Harman] single-factor test ==\n")
# Items: all DASS-21 items + all worry items; if single factor explains > 50%
# variance, CMV is likely a concern.
dass_items <- paste0("q47_", 1:21)
worry_items <- c("q51_1_lonely", "q51_2_quarantine", "q51_3_infected",
                 "q51_4_severe", "q51_5_death", "q51_6_health_system",
                 "q51_7_education", "q51_8_teaching", "q51_9_children_home",
                 "q51_10_income", "q51_11_food", "q51_12_water",
                 "q51_13_medicine", "q51_14_rent", "q51_15_debt",
                 "q51_16_agri", "q51_17_harvest", "q51_18_produce",
                 "q51_19_stigma", "q51_20_abuse", "q51_21_recession")
harman_items <- intersect(c(dass_items, worry_items), names(ana))
harman_df <- ana[, harman_items] %>% na.omit()
pc1 <- principal(harman_df, nfactors = 1, rotate = "none")
pct_var <- pc1$Vaccounted["Proportion Var", 1] * 100
cat(sprintf("Harman single-factor variance explained: %.1f%% (n=%d items=%d)\n",
            pct_var, nrow(harman_df), ncol(harman_df)))
cat("Rule: > 50%% suggests CMV; < 50%% acceptable.\n")
write_csv(tibble(harman_pct_variance = round(pct_var, 2),
                 items = ncol(harman_df), n = nrow(harman_df),
                 concern = ifelse(pct_var > 50, "Yes", "No")),
          file.path(tab_dir, "TableS22_Harman.csv"))

# ── B1/B2 · Re-verify SEM 5F-MLR fit + identify Heywood item ────────────
cat("\n== [SEM re-verify] 5-factor MLR (continuous) exact fit ==\n")
worry_mod <- '
  HFEAR =~ q51_2_quarantine + q51_3_infected + q51_4_severe + q51_5_death + q51_6_health_system
  ECON  =~ q51_10_income + q51_14_rent + q51_15_debt + q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession
  BNEED =~ q51_11_food + q51_12_water + q51_13_medicine
  SOC   =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIG  =~ q51_19_stigma + q51_20_abuse
'
fit_mlr <- cfa(worry_mod, data = ana, estimator = "MLR")
fm_mlr <- fitMeasures(fit_mlr, c("cfi.scaled", "tli.scaled",
                                  "rmsea.scaled", "srmr"))
cat("5F-MLR fit: CFI =", round(fm_mlr["cfi.scaled"], 3),
    "  RMSEA =", round(fm_mlr["rmsea.scaled"], 3),
    "  SRMR =", round(fm_mlr["srmr"], 3), "\n")

# Heywood: look for standardized loadings > 1
fit_5f_wlsmv <- cfa(worry_mod, data = ana, estimator = "WLSMV",
                     ordered = c("q51_1_lonely","q51_2_quarantine","q51_3_infected",
                                 "q51_4_severe","q51_5_death","q51_6_health_system",
                                 "q51_7_education","q51_8_teaching","q51_9_children_home",
                                 "q51_10_income","q51_11_food","q51_12_water",
                                 "q51_13_medicine","q51_14_rent","q51_15_debt",
                                 "q51_16_agri","q51_17_harvest","q51_18_produce",
                                 "q51_19_stigma","q51_20_abuse","q51_21_recession"))
std_load <- standardizedSolution(fit_5f_wlsmv)
heywood <- std_load %>% filter(op == "=~", abs(est.std) > 1)
cat("\n5F-WLSMV Heywood indicators (std loading > 1):\n")
print(heywood %>% select(lhs, rhs, est.std))

fit_5f_mlr_post <- tryCatch(lavInspect(fit_mlr, "post.check"),
                             error = function(e) NA)
cat("\n5F-MLR post.check:", fit_5f_mlr_post, "\n")

# 4-factor: merge ECON + BNEED
worry_4f <- '
  HFEAR  =~ q51_2_quarantine + q51_3_infected + q51_4_severe + q51_5_death + q51_6_health_system
  MATDEP =~ q51_10_income + q51_14_rent + q51_15_debt + q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession + q51_11_food + q51_12_water + q51_13_medicine
  SOC    =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIG   =~ q51_19_stigma + q51_20_abuse
'
fit_4f_wlsmv <- cfa(worry_4f, data = ana, estimator = "WLSMV",
                     ordered = c("q51_1_lonely","q51_2_quarantine","q51_3_infected",
                                 "q51_4_severe","q51_5_death","q51_6_health_system",
                                 "q51_7_education","q51_8_teaching","q51_9_children_home",
                                 "q51_10_income","q51_11_food","q51_12_water",
                                 "q51_13_medicine","q51_14_rent","q51_15_debt",
                                 "q51_16_agri","q51_17_harvest","q51_18_produce",
                                 "q51_19_stigma","q51_20_abuse","q51_21_recession"))
fm_4f <- fitMeasures(fit_4f_wlsmv, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))
post_4f <- tryCatch(lavInspect(fit_4f_wlsmv, "post.check"),
                    error = function(e) NA)
cat("\n4F-WLSMV fit: CFI =", round(fm_4f["cfi.scaled"], 3),
    "  RMSEA =", round(fm_4f["rmsea.scaled"], 3),
    "  post.check =", post_4f, "\n")

sem_verify <- tibble(
  spec = c("5F-MLR (continuous)", "5F-WLSMV (ordered)", "4F-WLSMV (merged BN+ECN)"),
  CFI   = c(round(fm_mlr["cfi.scaled"], 3),
            round(fitMeasures(fit_5f_wlsmv, "cfi.scaled"), 3),
            round(fm_4f["cfi.scaled"], 3)),
  RMSEA = c(round(fm_mlr["rmsea.scaled"], 3),
            round(fitMeasures(fit_5f_wlsmv, "rmsea.scaled"), 3),
            round(fm_4f["rmsea.scaled"], 3)),
  post_check = c(fit_5f_mlr_post, NA, post_4f),
  heywood = c("none", paste(heywood$rhs, collapse = ", "), "tbd")
)
print(sem_verify)
write_csv(sem_verify, file.path(tab_dir, "TableS23_SEM_fit_verification.csv"))

cat("\n=== 21_audit_response_quick.R COMPLETE ===\n")
