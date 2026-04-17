################################################################################
# 23_moderator_exploratory.R
# Individual-level moderators of worry -> distress associations.
# Adds to Section 2.3.7 exploratory: gender x worry (15 tests) and
# age x worry (15 tests). CC analytic sample.
#
# Output: TableS25_Individual_Moderators.csv (30 interaction rows)
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4); library(lmerTest)
})

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- d %>%
  filter(complete_dass, !is.na(site), site %in% 1:13) %>%
  drop_na(z_depression, z_anxiety, z_stress,
          worry_health_fear, worry_economic, worry_basic_needs,
          worry_social, worry_safety_stigma,
          age, female, ethnicity_minority, education, employment,
          comorbidity, gen_health, perceived_risk, risk_self,
          health_trust, livelihood_disruption, country)
cat("CC analytic N =", nrow(ana), "\n")

worries  <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
              "worry_social", "worry_safety_stigma")
outcomes <- c("z_depression", "z_anxiety", "z_stress")
covs_nomod <- c("ethnicity_minority", "education", "employment",
                "comorbidity", "gen_health", "perceived_risk", "risk_self",
                "health_trust", "livelihood_disruption", "country")

# Centre age to reduce collinearity with age*worry
ana <- ana %>% mutate(age_c = age - mean(age))

run_interaction <- function(y, w, mod_var, mod_label) {
  f <- as.formula(paste0(y, " ~ ", w, " * ", mod_var, " + ",
                          paste(setdiff(worries, w), collapse = " + "),
                          " + ", paste(covs_nomod, collapse = " + "),
                          " + (1|site)"))
  fit <- suppressMessages(suppressWarnings(
    lmer(f, data = ana, REML = FALSE,
         control = lmerControl(calc.derivs = FALSE))
  ))
  co <- summary(fit)$coefficients
  int_row <- grep(paste0(w, ":", mod_var), rownames(co), value = TRUE)
  if (length(int_row) != 1) return(NULL)
  tibble(
    outcome   = y,
    worry     = w,
    moderator = mod_label,
    beta      = round(co[int_row, "Estimate"], 4),
    se        = round(co[int_row, "Std. Error"], 4),
    df        = round(co[int_row, "df"], 1),
    t         = round(co[int_row, "t value"], 3),
    p         = signif(co[int_row, "Pr(>|t|)"], 4)
  )
}

# Gender moderator
cat("\n== Gender x worry ==\n")
gender_rows <- list()
for (y in outcomes) for (w in worries) {
  gender_rows[[paste(y, w, sep = "_")]] <- run_interaction(y, w,
                                                            "female", "gender")
}
gender_tab <- bind_rows(gender_rows)

# Age moderator (age centred)
cat("\n== Age x worry ==\n")
age_rows <- list()
for (y in outcomes) for (w in worries) {
  age_rows[[paste(y, w, sep = "_")]] <- run_interaction(y, w,
                                                         "age_c", "age (centred)")
}
age_tab <- bind_rows(age_rows)

all_mod <- bind_rows(gender_tab, age_tab) %>%
  mutate(sig = ifelse(p < 0.05, "*", ""))
print(all_mod, n = Inf)

# Apply BH-FDR separately within each moderator family (15 tests each)
all_mod <- all_mod %>%
  group_by(moderator) %>%
  mutate(p_adj_BH = round(p.adjust(p, method = "BH"), 4),
         sig_BH   = ifelse(p_adj_BH < 0.05, "*", "")) %>%
  ungroup()

write_csv(all_mod, file.path(tab_dir, "TableS25_Individual_Moderators.csv"))
cat("\nSaved TableS25_Individual_Moderators.csv\n")

n_sig_raw <- sum(all_mod$p < 0.05)
n_sig_bh  <- sum(all_mod$p_adj_BH < 0.05)
cat(sprintf("Summary: %d/30 uncorrected p<.05; %d/30 BH-FDR<.05\n",
            n_sig_raw, n_sig_bh))

cat("\n=== 23_moderator_exploratory.R COMPLETE ===\n")
