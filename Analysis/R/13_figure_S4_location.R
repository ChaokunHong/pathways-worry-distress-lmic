################################################################################
# 13_figure_S4_location.R
# Figure S4: Worry × Location Type interaction simple slopes
# Two strongest patterns from Table S11 Panel D (worry × location):
#   Panel A: Peri-urban amplifies economic worry effect on depression
#   Panel B: Remote attenuates health fears effect on anxiety
################################################################################

library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

proj <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
out_dir <- file.path(proj, "Analysis/output")
fig_dir <- file.path(proj, "Analysis/figures")

dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- dat[dat$complete_dass & !is.na(dat$site) & dat$site %in% 1:13, ]

# ── Palette consistent with other figures ────────────────────────────────────
loc_colors <- c(
  "Urban"      = "#3C5488",   # navy
  "Peri-urban" = "#B07AA1",   # mauve
  "Rural"      = "#7BAFD4",   # light blue
  "Remote"     = "#4DBBD5"    # teal
)

theme_ssm <- function(bs = 10.5) {
  theme_minimal(base_size = bs) %+replace% theme(
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = "#aaaaaa", linewidth = 0.5),
    axis.line  = element_line(color = "#555555", linewidth = 0.35),
    axis.ticks = element_line(color = "#555555", linewidth = 0.25),
    axis.text  = element_text(size = rel(0.88), color = "#333333"),
    axis.title = element_text(size = rel(0.95), face = "bold"),
    legend.position  = "bottom",
    legend.key = element_rect(fill = NA, color = NA),
    legend.title = element_text(size = rel(0.88), face = "bold"),
    legend.text  = element_text(size = rel(0.85)),
    plot.title    = element_text(size = rel(1.10), face = "bold", hjust = 0,
                                  margin = margin(b = 4)),
    plot.subtitle = element_text(size = rel(0.85), color = "#666666", hjust = 0,
                                  margin = margin(b = 8)),
    plot.caption  = element_text(size = rel(0.72), color = "#888888", hjust = 0,
                                  margin = margin(t = 8)),
    strip.text = element_text(face = "bold", size = rel(0.92)),
    strip.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.margin = margin(10, 14, 8, 10)
  )
}

# ── Common covariates ───────────────────────────────────────────────────────
covs_str <- "age + female + ethnicity_minority + education + employment +
             comorbidity + gen_health + perceived_risk + risk_self + health_trust +
             livelihood_disruption + country"

# ── Helper: prediction grid ─────────────────────────────────────────────────
make_pred <- function(worry_var, worry_seq, model, ana_data) {
  expand_grid(
    worry_value = worry_seq,
    location_type = factor(c("Urban", "Peri-urban", "Rural", "Remote"),
                            levels = levels(ana_data$location_type))
  ) %>%
    mutate(
      age = mean(ana_data$age, na.rm = TRUE),
      female = 0,
      ethnicity_minority = 0,
      education = median(ana_data$education, na.rm = TRUE),
      employment = factor("Full-time", levels = levels(ana_data$employment)),
      comorbidity = mean(ana_data$comorbidity, na.rm = TRUE),
      gen_health = mean(ana_data$gen_health, na.rm = TRUE),
      perceived_risk = mean(ana_data$perceived_risk, na.rm = TRUE),
      risk_self = mean(ana_data$risk_self, na.rm = TRUE),
      health_trust = mean(ana_data$health_trust, na.rm = TRUE),
      livelihood_disruption = mean(ana_data$livelihood_disruption, na.rm = TRUE),
      country = factor("Indonesia", levels = levels(ana_data$country)),
      worry_health_fear = mean(ana_data$worry_health_fear, na.rm = TRUE),
      worry_economic = mean(ana_data$worry_economic, na.rm = TRUE),
      worry_basic_needs = mean(ana_data$worry_basic_needs, na.rm = TRUE),
      worry_social = mean(ana_data$worry_social, na.rm = TRUE),
      worry_safety_stigma = mean(ana_data$worry_safety_stigma, na.rm = TRUE),
      site = NA
    ) %>%
    {.[[worry_var]] <- .$worry_value; .}
}

# ═══════════════════════════════════════════════════════════════════════════════
# PANEL A: Peri-urban amplifies ECONOMIC worry on DEPRESSION
# Strongest interaction in Table S11 Panel D (worry × location): β = 0.127, p < 0.001
# ═══════════════════════════════════════════════════════════════════════════════

f_econ <- as.formula(paste("z_depression ~ worry_economic * location_type +",
  "country + age + female + ethnicity_minority + education + employment +",
  "comorbidity + gen_health + perceived_risk + risk_self + health_trust +",
  "livelihood_disruption + worry_health_fear + worry_basic_needs +",
  "worry_social + worry_safety_stigma + (1|site)"))

m_econ <- lmer(f_econ, data = ana, REML = TRUE)

# Extract interaction coefficient for annotation
econ_coef <- summary(m_econ)$coefficients
peri_int_econ <- econ_coef["worry_economic:location_typePeri-urban", ]

# Build prediction grid for economic worry
econ_seq <- seq(0, 21, length.out = 50)
pd_econ <- make_pred("worry_economic", econ_seq, m_econ, ana)
pd_econ$predicted <- predict(m_econ, newdata = pd_econ, re.form = NA, allow.new.levels = TRUE)

p_a <- ggplot(pd_econ, aes(x = worry_value, y = predicted, color = location_type)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = loc_colors, name = "Location type") +
  scale_x_continuous(breaks = seq(0, 21, 3)) +
  labs(
    title = "A. Economic worry effect on depression by location type",
    subtitle = bquote("Peri-urban × economic worry interaction: " * beta ==
                       .(round(peri_int_econ["Estimate"], 3)) * ", p < 0.001"),
    x = "Economic worry score (0–21)",
    y = "Predicted depression (z-score)"
  ) +
  theme_ssm()

# ═══════════════════════════════════════════════════════════════════════════════
# PANEL B: Remote attenuates HEALTH FEARS on ANXIETY (effect across all 3 outcomes)
# Strongest negative interaction: β = -0.091, p = 0.004 (anxiety)
# ═══════════════════════════════════════════════════════════════════════════════

f_hfear <- as.formula(paste("z_anxiety ~ worry_health_fear * location_type +",
  "country + age + female + ethnicity_minority + education + employment +",
  "comorbidity + gen_health + perceived_risk + risk_self + health_trust +",
  "livelihood_disruption + worry_economic + worry_basic_needs +",
  "worry_social + worry_safety_stigma + (1|site)"))

m_hfear <- lmer(f_hfear, data = ana, REML = TRUE)

hfear_coef <- summary(m_hfear)$coefficients
remote_int_hfear <- hfear_coef["worry_health_fear:location_typeRemote", ]

# Build prediction grid for health fears
hfear_seq <- seq(0, 15, length.out = 50)
pd_hfear <- make_pred("worry_health_fear", hfear_seq, m_hfear, ana)
pd_hfear$predicted <- predict(m_hfear, newdata = pd_hfear, re.form = NA, allow.new.levels = TRUE)

p_b <- ggplot(pd_hfear, aes(x = worry_value, y = predicted, color = location_type)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = loc_colors, name = "Location type") +
  scale_x_continuous(breaks = seq(0, 15, 3)) +
  labs(
    title = "B. Health-fear effect on anxiety by location type",
    subtitle = bquote("Remote × health-fear interaction: " * beta ==
                       .(round(remote_int_hfear["Estimate"], 3)) * ", p = " *
                       .(round(remote_int_hfear["Pr(>|t|)"], 3))),
    x = "Health-fear worry score (0–15)",
    y = "Predicted anxiety (z-score)"
  ) +
  theme_ssm()

# ═══════════════════════════════════════════════════════════════════════════════
# Combine
# ═══════════════════════════════════════════════════════════════════════════════

fig_s2 <- (p_a + p_b + plot_layout(guides = "collect")) +
  plot_annotation(
    title = "Community Type Moderates Worry-Distress Pathways",
    subtitle = "Predicted distress as a function of worry score, separately for four location types",
    caption = paste(
      "Note: Simple slopes from multilevel interaction models with full covariate adjustment.",
      "Panel A: Peri-urban sites show steeper economic-worry slopes than urban sites (the reference),",
      "consistent with greater economic vulnerability in peri-urban populations.",
      "Panel B: Remote sites show flatter health-fear slopes, suggesting psychological distance from",
      "pandemic epicenters attenuates the impact of health-related worries.",
      sep = " "
    ),
    theme = theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "#666666"),
      plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
      legend.position = "bottom"
    )
  )

ggsave(file.path(fig_dir, "FigureS4_Location_Interactions.png"),
       fig_s2, width = 12, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "FigureS4_Location_Interactions.pdf"),
       fig_s2, width = 12, height = 5.5, bg = "white")

cat("Figure S4 saved.\n")
cat("Panel A interaction: β =", round(peri_int_econ["Estimate"], 4),
    " p =", format(peri_int_econ["Pr(>|t|)"], digits = 3), "\n")
cat("Panel B interaction: β =", round(remote_int_hfear["Estimate"], 4),
    " p =", format(remote_int_hfear["Pr(>|t|)"], digits = 3), "\n")
