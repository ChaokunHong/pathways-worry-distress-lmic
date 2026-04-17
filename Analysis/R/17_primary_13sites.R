################################################################################
# 17_primary_13sites.R
# Re-run primary MI multilevel on the 13 protocol-specified sites only
# (excluding the 38 "Vietnam other" off-protocol cases).
#
# Rationale: SPEAR protocol [Van Nuil 2021] specifies 13 districts;
# respondents recruited in Vietnam outside the four pre-specified Vietnamese
# sites (n = 38, site code 14 = "Vietnam other") are off-protocol and are
# excluded for the primary analysis to maintain protocol fidelity.
################################################################################

library(haven)
library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
library(mitml)
library(MuMIn)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))

# Analytic restriction: complete DASS, non-missing site, protocol-specified
# sites only (site codes 1–13, excluding 14 = "Vietnam other")
analytic <- d %>%
  filter(complete_dass, !is.na(site), site %in% 1:13)

cat("Primary analytic sample:", nrow(analytic), "\n")
cat("Sites retained:", length(unique(analytic$site)), "\n")
cat("By country:\n"); print(table(analytic$country))

imp_vars <- c("z_depression","z_anxiety","z_stress",
              "age","female","ethnicity_minority","education","employment",
              "comorbidity","gen_health",
              "perceived_risk","risk_self","health_trust",
              "worry_health_fear","worry_economic","worry_basic_needs",
              "worry_social","worry_safety_stigma",
              "livelihood_disruption",
              "country","site")

imp_data <- analytic %>%
  dplyr::select(all_of(imp_vars)) %>%
  mutate(
    female = as.factor(female),
    ethnicity_minority = as.factor(ethnicity_minority),
    education = as.integer(education),
    employment = as.factor(employment),
    country = as.factor(country),
    site = as.factor(site)
  )

ini <- mice(imp_data, maxit = 0, print = FALSE)
meth <- ini$method
meth[c("z_depression","z_anxiety","z_stress","country","site")] <- ""
meth["female"] <- "logreg"
meth["ethnicity_minority"] <- "logreg"
meth["employment"] <- "polyreg"

pred <- ini$predictorMatrix
pred[, "site"] <- 0L  # match original primary spec
diag(pred) <- 0L

cat("\n=== MI (13 sites, n=1,462) ===\n")
set.seed(2026)
mi_obj <- mice(imp_data, m = 30, maxit = 10,
               method = meth, predictorMatrix = pred, print = FALSE)
saveRDS(mi_obj, file.path(out_dir, "mi_object_13sites.rds"))

f_m3 <- "~ country + age + female + ethnicity_minority + education +
          employment + comorbidity + gen_health +
          perceived_risk + risk_self + health_trust +
          worry_health_fear + worry_economic + worry_basic_needs +
          worry_social + worry_safety_stigma +
          livelihood_disruption + (1|site)"

fit_pool <- function(mi_obj, outcome) {
  fits <- lapply(1:mi_obj$m, function(i) {
    lmer(as.formula(paste(outcome, f_m3)),
         data = complete(mi_obj, i), REML = TRUE)
  })
  mat <- testEstimates(fits, extra.pars = FALSE)$estimates
  data.frame(term = rownames(mat),
             outcome = outcome,
             estimate = round(mat[,1], 4),
             se = round(mat[,2], 4),
             df = round(mat[,4], 1),
             p_value = round(mat[,5], 4),
             fmi = round(mat[,7], 3),
             row.names = NULL)
}

outs <- c("z_depression","z_anxiety","z_stress")
res <- bind_rows(lapply(outs, fit_pool, mi_obj = mi_obj))

# BH correction on worry terms
res <- res %>% group_by(outcome) %>% mutate(
  p_adj = ifelse(grepl("^worry_", term),
                 p.adjust(ifelse(grepl("^worry_", term), p_value, NA),
                          method = "BH"),
                 NA_real_)
) %>% ungroup()

# Cross-outcome BH on worry (correct family = 15 tests total)
worry_rows <- res$term %>% grepl("^worry_", .)
res$p_adj_family15 <- NA_real_
res$p_adj_family15[worry_rows] <- p.adjust(res$p_value[worry_rows], method = "BH")

write_csv(res, file.path(tab_dir, "Table4_Primary_Results_MI_13sites.csv"))
cat("Saved: Table4_Primary_Results_MI_13sites.csv\n\n")

# R2 decomposition for 13 sites
r2_mats <- list()
for (y in outs) {
  r2_vals <- matrix(NA, nrow = 4, ncol = 2,
                    dimnames = list(c("M0","M1","M2","M3"),
                                    c("R2m","R2c")))
  forms <- list(
    M0 = paste(y, "~ country + (1|site)"),
    M1 = paste(y, "~ country + age + female + ethnicity_minority + education + employment + comorbidity + gen_health + (1|site)"),
    M2 = paste(y, "~ country + age + female + ethnicity_minority + education + employment + comorbidity + gen_health + perceived_risk + risk_self + health_trust + livelihood_disruption + (1|site)"),
    M3 = paste(y, f_m3)
  )
  for (k in names(forms)) {
    r2s <- sapply(1:mi_obj$m, function(i) {
      m <- lmer(as.formula(forms[[k]]),
                data = complete(mi_obj, i), REML = TRUE)
      r.squaredGLMM(m)[1, ]
    })
    r2_vals[k, ] <- rowMeans(r2s)
  }
  r2_mats[[y]] <- r2_vals
}
r2_out <- do.call(rbind, lapply(names(r2_mats), function(y) {
  data.frame(outcome = y, model = rownames(r2_mats[[y]]),
             R2m = round(r2_mats[[y]][, "R2m"], 4),
             R2c = round(r2_mats[[y]][, "R2c"], 4))
}))
write_csv(r2_out, file.path(tab_dir, "R2_decomposition_MI_13sites.csv"))
cat("Saved: R2_decomposition_MI_13sites.csv\n")
print(r2_out)

# Compare with 14-site primary (worry coefficients only)
old <- read_csv(file.path(tab_dir, "Table4_Primary_Results_MI.csv"),
                show_col_types = FALSE) %>%
  filter(grepl("^worry_", term)) %>%
  mutate(outcome = paste0("z_", tolower(outcome))) %>%
  dplyr::select(term, outcome, beta_14 = estimate, p_14 = p.value)

new <- res %>% filter(grepl("^worry_", term)) %>%
  dplyr::select(term, outcome, beta_13 = estimate, p_13 = p_value,
                p_adj_13 = p_adj_family15)

cmp <- left_join(old, new, by = c("term","outcome")) %>%
  mutate(delta_beta = round(beta_13 - beta_14, 4))
write_csv(cmp, file.path(tab_dir, "Diagnostic_13sites_vs_14sites.csv"))
cat("\n13 vs 14 sites comparison (worry coefficients):\n")
print(cmp)
cat("\nSign agreement:", sum(sign(cmp$beta_13) == sign(cmp$beta_14)), "/15\n")
cat("Max |Δβ|:", max(abs(cmp$delta_beta)), "\n")
