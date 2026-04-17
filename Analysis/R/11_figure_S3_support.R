################################################################################
# 11_figure_S3_support.R
# Figure S3: Simple slopes of community support × economic worry interaction
# Demonstrates buffering effect on depression and anxiety
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

# ── Palette & Theme (consistent with other figures) ─────────────────────────
navy <- "#3C5488"
teal <- "#4DBBD5"

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
    plot.title    = element_text(size = rel(1.15), face = "bold", hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = rel(0.85), color = "#666666", hjust = 0, margin = margin(b = 8)),
    plot.caption  = element_text(size = rel(0.72), color = "#888888", hjust = 0, margin = margin(t = 8)),
    strip.text = element_text(face = "bold", size = rel(0.92)),
    strip.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.margin = margin(10, 14, 8, 10)
  )
}

# ── Fit interaction models ─────────────────────────────────────────────────

covs <- "age + female + ethnicity_minority + education + employment +
         comorbidity + gen_health + perceived_risk + risk_self + health_trust +
         livelihood_disruption + country +
         worry_health_fear + worry_basic_needs + worry_social + worry_safety_stigma"

f_depr <- as.formula(paste("z_depression ~ worry_economic * supportive_community +",
                            covs, "+ (1|site)"))
f_anx  <- as.formula(paste("z_anxiety ~ worry_economic * supportive_community +",
                            covs, "+ (1|site)"))

m_depr <- lmer(f_depr, data = ana, REML = TRUE)
m_anx  <- lmer(f_anx,  data = ana, REML = TRUE)

# Extract interaction coefficients for annotation
depr_int <- summary(m_depr)$coefficients["worry_economic:supportive_community", ]
anx_int  <- summary(m_anx)$coefficients["worry_economic:supportive_community", ]

# ── Create prediction grid ──────────────────────────────────────────────────

# Low vs High community support: 25th vs 75th percentile
sup_q <- quantile(ana$supportive_community, c(0.25, 0.75), na.rm = TRUE)
econ_range <- range(ana$worry_economic, na.rm = TRUE)
econ_seq <- seq(econ_range[1], econ_range[2], length.out = 50)

# Hold covariates at means / reference levels
make_pred_data <- function(outcome_name) {
  expand_grid(
    worry_economic = econ_seq,
    supportive_community = c(sup_q[1], sup_q[2])
  ) %>%
    mutate(
      age = mean(ana$age, na.rm = TRUE),
      female = 0,
      ethnicity_minority = 0,
      education = median(ana$education, na.rm = TRUE),
      employment = factor("Full-time", levels = levels(ana$employment)),
      comorbidity = mean(ana$comorbidity, na.rm = TRUE),
      gen_health = mean(ana$gen_health, na.rm = TRUE),
      perceived_risk = mean(ana$perceived_risk, na.rm = TRUE),
      risk_self = mean(ana$risk_self, na.rm = TRUE),
      health_trust = mean(ana$health_trust, na.rm = TRUE),
      livelihood_disruption = mean(ana$livelihood_disruption, na.rm = TRUE),
      country = factor("Indonesia", levels = levels(ana$country)),
      worry_health_fear = mean(ana$worry_health_fear, na.rm = TRUE),
      worry_basic_needs = mean(ana$worry_basic_needs, na.rm = TRUE),
      worry_social = mean(ana$worry_social, na.rm = TRUE),
      worry_safety_stigma = mean(ana$worry_safety_stigma, na.rm = TRUE),
      site = NA,
      support_level = ifelse(supportive_community == sup_q[1],
                              "Low support (25th pct)",
                              "High support (75th pct)")
    )
}

pd_depr <- make_pred_data("depression")
pd_anx  <- make_pred_data("anxiety")

pd_depr$predicted <- predict(m_depr, newdata = pd_depr, re.form = NA, allow.new.levels = TRUE)
pd_anx$predicted  <- predict(m_anx,  newdata = pd_anx,  re.form = NA, allow.new.levels = TRUE)

# ── Build panels ────────────────────────────────────────────────────────────

make_panel <- function(pd, title_text, int_coef) {
  ggplot(pd, aes(x = worry_economic, y = predicted,
                 color = support_level, linetype = support_level)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = c("Low support (25th pct)" = navy,
                                   "High support (75th pct)" = teal),
                       name = "Community support") +
    scale_linetype_manual(values = c("Low support (25th pct)" = "solid",
                                      "High support (75th pct)" = "dashed"),
                          name = "Community support") +
    labs(
      title = title_text,
      subtitle = bquote("Interaction " * beta == .(round(int_coef["Estimate"], 3)) *
                          ", p = " * .(round(int_coef["Pr(>|t|)"], 3))),
      x = "Economic worry score (0–21)",
      y = "Predicted z-score"
    ) +
    theme_ssm()
}

p_depr <- make_panel(pd_depr, "A. Depression", depr_int)
p_anx  <- make_panel(pd_anx,  "B. Anxiety",    anx_int)

fig_s1 <- (p_depr + p_anx + plot_layout(guides = "collect")) +
  plot_annotation(
    title = "Community Support Buffers Economic Worry Effects",
    subtitle = "Predicted distress at low vs high community support (25th vs 75th percentile)",
    caption = "Note: Simple slopes from multilevel interaction models. Other covariates held at means/reference levels. The flatter slope under high community support reflects the buffering effect.",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "#666666"),
      plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
      legend.position = "bottom"
    )
  )

ggsave(file.path(fig_dir, "FigureS3_Support_Buffering.png"),
       fig_s1, width = 11, height = 5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "FigureS3_Support_Buffering.pdf"),
       fig_s1, width = 11, height = 5, bg = "white")

cat("Figure S3 saved:\n")
cat("  FigureS3_Support_Buffering.png\n")
cat("  FigureS3_Support_Buffering.pdf\n")
cat("\nInteraction coefficients:\n")
cat("  Depression: β =", round(depr_int["Estimate"], 4),
    " p =", round(depr_int["Pr(>|t|)"], 4), "\n")
cat("  Anxiety:    β =", round(anx_int["Estimate"], 4),
    " p =", round(anx_int["Pr(>|t|)"], 4), "\n")
