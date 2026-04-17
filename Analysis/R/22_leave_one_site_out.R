################################################################################
# 22_leave_one_site_out.R
# Site-level influence diagnostic (G2 in the audit). Refits M3 (CC) with
# each of the 13 sites removed in turn; reports the maximum |Δβ| per worry
# coefficient as a "cluster-leave-one-out" sensitivity.
#
# Output:
#   TableS24_LeaveOneSite_Worry.csv    (wide: 15 rows, 13 dropped-site cols)
#   TableS24_LeaveOneSite_Summary.csv  (per-coefficient max |Δβ|, sign stability)
#   FigureS5_LeaveOneSite_Forest.pdf   (forest plot, one panel per worry×outcome)
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4); library(lmerTest)
})

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")

d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- d %>%
  filter(complete_dass, !is.na(site), site %in% 1:13) %>%
  tidyr::drop_na(z_depression, z_anxiety, z_stress,
                 worry_health_fear, worry_economic, worry_basic_needs,
                 worry_social, worry_safety_stigma,
                 age, female, ethnicity_minority, education, employment,
                 comorbidity, gen_health, perceived_risk, risk_self,
                 health_trust, livelihood_disruption, country)
cat("CC analytic N =", nrow(ana), " | sites =", length(unique(ana$site)), "\n")

worries  <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
              "worry_social", "worry_safety_stigma")
outcomes <- c("z_depression", "z_anxiety", "z_stress")
covs <- c("age", "female", "ethnicity_minority", "education", "employment",
          "comorbidity", "gen_health", "perceived_risk", "risk_self",
          "health_trust", "livelihood_disruption", "country")

fit_m3 <- function(data, y) {
  f <- as.formula(paste(y, "~",
                         paste(c(worries, covs), collapse = " + "),
                         "+ (1|site)"))
  lmer(f, data = data, REML = FALSE,
       control = lmerControl(calc.derivs = FALSE))
}

# ── Full-sample reference M3 (CC, on 13 sites) ──────────────────────────
cat("\nFitting full-sample reference models...\n")
ref <- list()
for (y in outcomes) {
  fit <- fit_m3(ana, y)
  co  <- summary(fit)$coefficients
  ref[[y]] <- tibble(term = rownames(co),
                     ref_est = co[, "Estimate"],
                     ref_se  = co[, "Std. Error"])
}

# ── Leave-one-site-out refits ───────────────────────────────────────────
cat("\nLeave-one-site-out refits:\n")
loo <- list()
sites <- sort(unique(ana$site))
for (s in sites) {
  cat(sprintf("  dropping site %d ... ", s))
  sub <- ana %>% filter(site != s)
  for (y in outcomes) {
    fit <- tryCatch(fit_m3(sub, y), error = function(e) NULL)
    if (is.null(fit)) next
    co <- summary(fit)$coefficients
    loo_df <- tibble(dropped_site = s, outcome = y,
                     term = rownames(co),
                     est = co[, "Estimate"],
                     se  = co[, "Std. Error"])
    loo[[paste(s, y, sep = "_")]] <- loo_df
  }
  cat("done\n")
}
loo_all <- bind_rows(loo)

# ── Summary: for each worry×outcome, max |Δβ| across 13 drops ───────────
cat("\nSummarising worry-coefficient influence:\n")
summary_tab <- loo_all %>%
  filter(term %in% worries) %>%
  left_join(bind_rows(ref, .id = "outcome_id") %>%
              select(term, ref_est, outcome_id),
            by = c("term", "outcome" = "outcome_id")) %>%
  mutate(delta = est - ref_est) %>%
  group_by(outcome, term) %>%
  summarise(
    ref_beta         = unique(ref_est),
    min_loo_beta     = round(min(est), 4),
    max_loo_beta     = round(max(est), 4),
    max_abs_delta    = round(max(abs(delta)), 4),
    sign_flips       = sum(sign(est) != sign(ref_est)),
    .groups = "drop"
  ) %>%
  arrange(outcome, term)
print(summary_tab, n = Inf)
write_csv(summary_tab, file.path(tab_dir, "TableS24_LeaveOneSite_Summary.csv"))

# ── Wide table: 15 worry rows × 13 dropped-site columns ─────────────────
wide_tab <- loo_all %>%
  filter(term %in% worries) %>%
  select(dropped_site, outcome, term, est) %>%
  mutate(key = paste0("drop_", dropped_site)) %>%
  select(-dropped_site) %>%
  pivot_wider(names_from = key, values_from = est) %>%
  arrange(outcome, term)
write_csv(wide_tab, file.path(tab_dir, "TableS24_LeaveOneSite_Worry.csv"))

# ── Forest plot (one panel per outcome) ─────────────────────────────────
forest_df <- loo_all %>%
  filter(term %in% worries) %>%
  mutate(ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
         worry_label = factor(term,
                              levels = rev(worries),
                              labels = rev(c("Health fears", "Economic",
                                             "Basic needs", "Social",
                                             "Safety/stigma"))))
ref_df <- bind_rows(ref, .id = "outcome") %>%
  filter(term %in% worries) %>%
  mutate(worry_label = factor(term,
                              levels = rev(worries),
                              labels = rev(c("Health fears", "Economic",
                                             "Basic needs", "Social",
                                             "Safety/stigma"))))

p <- ggplot(forest_df, aes(x = est, y = worry_label)) +
  geom_point(alpha = 0.35, size = 1.1, colour = "grey35") +
  geom_point(data = ref_df, aes(x = ref_est), colour = "#aa5500",
             size = 2.5, shape = 18) +
  geom_vline(xintercept = 0, linetype = "dashed",
             linewidth = 0.3, colour = "grey60") +
  facet_wrap(~ outcome, scales = "free_x") +
  labs(x = expression(beta), y = NULL,
       title = NULL,
       subtitle = "Grey points: 13 leave-one-site refits; amber diamond: full-sample reference") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 8, colour = "grey30"),
        strip.text = element_text(face = "bold"))
ggsave(file.path(fig_dir, "FigureS5_LeaveOneSite_Forest.pdf"),
       p, width = 8.5, height = 4.2)
ggsave(file.path(fig_dir, "FigureS5_LeaveOneSite_Forest.png"),
       p, width = 8.5, height = 4.2, dpi = 300)
cat("\nSaved TableS24 (wide + summary) + FigureS5 forest plot\n")

cat("\n=== 22_leave_one_site_out.R COMPLETE ===\n")
