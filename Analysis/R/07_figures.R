################################################################################
# 07_figures.R
# Publication-quality figures — cold-tone, low-saturation, SSM standard
# Updated: Uses MI results as primary; matrix layout for worry sub-domains
################################################################################

library(tidyverse)
library(scales)
library(patchwork)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

# ── Palette & Theme ──────────────────────────────────────────────────────────
pal_country <- c("Indonesia" = "#3C5488", "Nepal" = "#B07AA1", "Vietnam" = "#4DBBD5")
pal_outcome <- c("Depression" = "#3C5488", "Anxiety" = "#B07AA1", "Stress" = "#4DBBD5")

theme_ssm <- function(base_size = 10.5) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      text = element_text(family = "sans", color = "#1a1a1a"),
      panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = "#aaaaaa", linewidth = 0.5),
      axis.line  = element_line(color = "#555555", linewidth = 0.35),
      axis.ticks = element_line(color = "#555555", linewidth = 0.25),
      axis.text  = element_text(size = rel(0.88), color = "#333333"),
      axis.title = element_text(size = rel(0.95), face = "bold"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      legend.position  = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      legend.title = element_text(size = rel(0.88), face = "bold"),
      legend.text  = element_text(size = rel(0.85)),
      legend.margin = margin(t = 4),
      plot.title    = element_text(size = rel(1.15), face = "bold", hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.85), color = "#666666", hjust = 0, margin = margin(b = 8)),
      plot.caption  = element_text(size = rel(0.72), color = "#888888", hjust = 0, margin = margin(t = 8)),
      strip.text = element_text(face = "bold", size = rel(0.92)),
      strip.background = element_rect(fill = "#F5F5F5", color = NA),
      plot.margin = margin(10, 14, 8, 10)
    )
}

# Load data
dat <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
ana <- dat[dat$complete_dass, ]

# Determine primary results file (MI > multilevel > CR2)
primary_file <- file.path(tab_dir, "Table4_Primary_Results_MI.csv")
primary_label <- "MI (n = 1,462)"
if (!file.exists(primary_file)) {
  primary_file <- file.path(tab_dir, "Table4_Primary_Results_Multilevel.csv")
  primary_label <- "Multilevel (n = 892)"
}
primary <- read_csv(primary_file, show_col_types = FALSE)
cat("Primary results:", basename(primary_file), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# FIGURE 3 — Constructs by Country
# ═══════════════════════════════════════════════════════════════════════════════
cat("Figure 3...\n")

make_violin <- function(df, facet_var, y_lab, title_txt) {
  ggplot(df, aes(x = country, y = value, fill = country)) +
    geom_violin(alpha = 0.55, color = NA, scale = "width", trim = FALSE) +
    geom_boxplot(width = 0.13, fill = "white", alpha = 0.92,
                 outlier.shape = 21, outlier.size = 0.6, outlier.alpha = 0.35,
                 color = "#444444", linewidth = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 1.8,
                 color = "#D95F02", show.legend = FALSE) +
    facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y", nrow = 1) +
    scale_fill_manual(values = pal_country, guide = "none") +
    scale_x_discrete(labels = c("IDN", "NPL", "VNM")) +
    labs(title = title_txt, x = NULL, y = y_lab) +
    theme_ssm() + theme(axis.text.x = element_text(size = 8))
}

d3a <- ana %>%
  dplyr::select(country, worry_health_fear, worry_economic, worry_basic_needs, worry_social, worry_safety_stigma) %>%
  pivot_longer(-country, names_to = "domain", values_to = "value") %>%
  mutate(domain = dplyr::recode(domain, worry_health_fear = "Health\nfears", worry_economic = "Economic\nconcerns",
    worry_basic_needs = "Basic\nneeds", worry_social = "Social/\nfamily", worry_safety_stigma = "Safety/\nstigma"))
p3a <- make_violin(d3a, "domain", "Score", "A. Worry Sub-Domains")

d3b <- ana %>%
  dplyr::select(country, z_depression, z_anxiety, z_stress) %>%
  pivot_longer(-country, names_to = "outcome", values_to = "value") %>%
  mutate(outcome = dplyr::recode(outcome, z_depression = "Depression", z_anxiety = "Anxiety", z_stress = "Stress"))
p3b <- make_violin(d3b, "outcome", "Z-Score", "B. DASS-21 Outcomes")

d3c <- ana %>%
  dplyr::select(country, prevention_evidence, prevention_folk) %>%
  pivot_longer(-country, names_to = "type", values_to = "value") %>%
  mutate(type = dplyr::recode(type, prevention_evidence = "Evidence-Based", prevention_folk = "Folk/Alternative"))
p3c <- make_violin(d3c, "type", "Score", "C. Preventive Behaviors") +
  labs(caption = "IDN = Indonesia, NPL = Nepal, VNM = Vietnam. Diamonds = means.")

fig3 <- p3a / p3b / p3c + plot_layout(heights = c(1, 0.7, 0.6))
ggsave(file.path(fig_dir, "Figure3_Constructs_by_Country.png"), fig3, width = 11, height = 12, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "Figure3_Constructs_by_Country.pdf"), fig3, width = 11, height = 12, bg = "white")
cat("  saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# FIGURE 4 — Two-panel: (A) Worry coefficient matrix + (B) Other predictors
# ═══════════════════════════════════════════════════════════════════════════════
cat("Figure 4...\n")

# --- Panel A: Worry sub-domain × Outcome matrix (5 rows × 3 columns) ---
worry_vars <- c("worry_health_fear", "worry_economic", "worry_basic_needs",
                "worry_social", "worry_safety_stigma")
worry_labels <- c("Health fears", "Economic worry", "Basic-needs worry",
                  "Social disruption", "Safety/stigma")

mat_data <- primary %>%
  filter(term %in% worry_vars) %>%
  mutate(
    predictor = factor(dplyr::recode(term,
      worry_health_fear = "Health fears", worry_economic = "Economic worry",
      worry_basic_needs = "Basic-needs worry", worry_social = "Social disruption",
      worry_safety_stigma = "Safety/stigma"),
      levels = rev(worry_labels)),
    outcome = factor(outcome, levels = c("Depression", "Anxiety", "Stress")),
    sig = p.value < 0.05,
    cell_label = sprintf("%.3f%s", estimate, ifelse(sig, "*", "")),
    text_col = ifelse(abs(estimate) > 0.06, "white", "#333333")
  )

p4a <- ggplot(mat_data, aes(x = outcome, y = predictor, fill = estimate)) +
  geom_tile(color = "white", linewidth = 1.8) +
  geom_text(aes(label = cell_label, color = text_col), size = 3.5, fontface = "bold",
            show.legend = FALSE) +
  scale_color_identity() +
  # Fresh blue → lilac-cream → violet diverging palette. Clean editorial
  # feel; cool-cool diverging with neutral midpoint.
  scale_fill_gradient2(low = "#4E8EC7", mid = "#F5F2F8", high = "#8D6CB8",
                       midpoint = 0, name = expression(beta),
                       limits = c(-0.10, 0.10), oob = squish,
                       breaks = c(-0.10, -0.05, 0, 0.05, 0.10),
                       guide = guide_colorbar(barwidth = 8, barheight = 0.5,
                                              title.position = "left")) +
  labs(title = "A. Worry Sub-Domain Associations with Distress",
       subtitle = paste("* p < 0.05  |", primary_label),
       x = NULL, y = NULL) +
  theme_ssm() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 9, lineheight = 1.05),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom")

# --- Panel B: Other predictors forest plot ---
other_terms <- c("comorbidity", "female", "age", "perceived_risk",
                 "risk_self", "health_trust", "livelihood_disruption")
other_labels <- c(comorbidity = "Comorbidity", female = "Female",
                  age = "Age (years)", perceived_risk = "Perceived\nhealth threat",
                  risk_self = "Self-assessed\nrisk", health_trust = "Health system\ntrust",
                  livelihood_disruption = "Livelihood\ndisruption")

fp_data <- primary %>%
  filter(term %in% other_terms) %>%
  mutate(label = factor(other_labels[term], levels = rev(other_labels)),
         outcome = factor(outcome, levels = c("Depression", "Anxiety", "Stress")))

p4b <- ggplot(fp_data, aes(x = estimate, y = label, color = outcome, shape = outcome)) +
  geom_vline(xintercept = 0, linetype = "22", color = "#AAAAAA", linewidth = 0.4) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.55),
                  size = 0.3, linewidth = 0.5, fatten = 3) +
  scale_color_manual(values = pal_outcome, name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  coord_cartesian(xlim = c(-0.1, 0.35)) +
  labs(title = "B. Covariates and Contextual Predictors",
       x = expression(beta), y = NULL) +
  theme_ssm() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 9, lineheight = 1.05))

fig4 <- p4a + p4b + plot_layout(widths = c(1, 1.2))
ggsave(file.path(fig_dir, "Figure4_Coefficient_Matrix.png"), fig4, width = 12, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "Figure4_Coefficient_Matrix.pdf"), fig4, width = 12, height = 5.5, bg = "white")
cat("  saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# FIGURE 5 — R² Decomposition
# ═══════════════════════════════════════════════════════════════════════════════
cat("Figure 5...\n")

# Primary source: MI Rubin-pooled marginal R² (m = 30). Canonical per Methods §2.3.
r2_file <- file.path(tab_dir, "R2_decomposition_MI.csv")
if (!file.exists(r2_file)) r2_file <- file.path(tab_dir, "R2_decomposition_multilevel.csv")
if (!file.exists(r2_file)) r2_file <- file.path(tab_dir, "R2_decomposition.csv")
r2 <- read_csv(r2_file, show_col_types = FALSE)

if ("R2m_Depression" %in% names(r2)) {
  r2l <- r2 %>% dplyr::select(model, starts_with("R2m_")) %>%
    pivot_longer(-model, names_to = "outcome", values_to = "r2") %>%
    mutate(outcome = gsub("R2m_", "", outcome))
} else if ("R2_marginal_Depression" %in% names(r2)) {
  r2l <- r2 %>% dplyr::select(model, starts_with("R2_marginal")) %>%
    pivot_longer(-model, names_to = "outcome", values_to = "r2") %>%
    mutate(outcome = gsub("R2_marginal_", "", outcome))
} else {
  r2l <- r2 %>% dplyr::select(model, starts_with("adj_r2")) %>%
    pivot_longer(-model, names_to = "outcome", values_to = "r2") %>%
    mutate(outcome = gsub("adj_r2_", "", outcome))
}

r2l <- r2l %>%
  filter(model %in% c("M0", "M1", "M2", "M3")) %>%
  mutate(model = factor(model, levels = c("M0", "M1", "M2", "M3")),
         pct = r2 * 100)

fig5 <- ggplot(r2l, aes(x = model, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, alpha = 0.88) +
  geom_text(aes(label = sprintf("%.1f", pct)), position = position_dodge(width = 0.72),
            vjust = -0.45, size = 2.6, color = "#333333") +
  scale_fill_manual(values = pal_outcome, name = NULL) +
  scale_x_discrete(labels = c(
    "M0\nCountry + Site",
    "M1\n+ Block 1\n(demographics)",
    "M2\n+ Block 2\n(crisis context)",
    "M3 (primary)\n+ 5 worry\nsub-domains")) +
  scale_y_continuous(limits = c(0, 33), breaks = seq(0, 30, 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Incremental variance explained by sequential block entry",
       subtitle = expression("Marginal"~R^2~"averaged across"~italic(m)==30~"imputations"),
       x = NULL, y = expression("Marginal"~R^2),
       caption = paste("M3 is the primary model (n = 1,462 across 13 sites).",
                       "Block ordering reflects conceptual priors, not data-driven entry.")) +
  theme_ssm() + theme(panel.grid.major.x = element_blank(),
                      legend.position = "top", legend.justification = "left",
                      axis.text.x = element_text(lineheight = 1.1))

ggsave(file.path(fig_dir, "Figure5_R2_Decomposition.png"), fig5, width = 8, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "Figure5_R2_Decomposition.pdf"), fig5, width = 8, height = 5.5, bg = "white")
cat("  saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# FIGURE 6 — Mediation
# ═══════════════════════════════════════════════════════════════════════════════
cat("Figure 6...\n")

# Primary source: Rubin-pooled mediation across m = 30 imputations (Table S17).
# This is the canonical primary per Methods v8 §2.3.6; the complete-case
# bootstrap (Table 5) is the comparator.
med <- read_csv(file.path(tab_dir, "TableS17_Mediation_Rubin_Pooled.csv"),
                show_col_types = FALSE)

# Map TableS17 label strings -> pretty two-line pathway labels
pathway_map <- c(
  "Livelihood -> Econ Worry -> Depression"     =
    "Livelihood disruption\n\u2192 Economic worry \u2192 Depression",
  "Livelihood -> Basic Needs -> Depression"    =
    "Livelihood disruption\n\u2192 Basic-needs worry \u2192 Depression",
  "Perceived Risk -> Health Fear -> Anxiety"   =
    "Perceived risk\n\u2192 Health fears \u2192 Anxiety",
  "Perceived Risk -> Health Fear -> Stress"    =
    "Perceived risk\n\u2192 Health fears \u2192 Stress",
  "Perceived Risk -> Safety/Stigma -> Depression" =
    "Perceived risk\n\u2192 Safety/stigma \u2192 Depression"
)

med <- med %>%
  mutate(pathway = pathway_map[label],
         pathway = fct_reorder(pathway, acme),
         pct_lab = paste0(round(prop_mediated * 100), "% mediated"))

# Auto-derive x-limits from data so no pathway is clipped; add tiny padding
# for the "% mediated" text labels on the right.
x_max <- max(med$acme_ci_high, na.rm = TRUE)
x_pad <- x_max * 0.25   # ~25% of data range for label space

fig6 <- ggplot(med, aes(x = acme, y = pathway)) +
  geom_vline(xintercept = 0, linetype = "22",
             color = "#AAAAAA", linewidth = 0.4) +
  geom_pointrange(aes(xmin = acme_ci_low, xmax = acme_ci_high),
                  color = "#4E8EC7", size = 0.45,
                  linewidth = 0.65, fatten = 3) +
  geom_text(aes(x = acme_ci_high + x_max * 0.03, label = pct_lab),
            hjust = 0, size = 2.9, color = "#555555") +
  scale_x_continuous(limits = c(-x_max * 0.05, x_max + x_pad),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Mediation analysis: indirect-effect decomposition",
       subtitle = paste("Rubin-pooled across m = 30 imputed datasets with 500",
                        "bootstrap resamples per imputation;\nBarnard\u2013Rubin",
                        "95% CIs (Table S9). All pathways p < 0.001."),
       x = expression("Indirect effect ("*beta*")"), y = NULL,
       caption = "Cross-sectional data; a decomposition of covariation, not a causal estimate.") +
  theme_ssm() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8.5, lineheight = 1.1))

ggsave(file.path(fig_dir, "Figure6_Mediation.png"), fig6, width = 9, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "Figure6_Mediation.pdf"), fig6, width = 9, height = 4.5, bg = "white")
cat("  saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# FIGURE 7 — Country Heatmap
# ═══════════════════════════════════════════════════════════════════════════════
cat("Figure 7...\n")

strat_f <- file.path(tab_dir, "Table6_Country_Stratified_Multilevel.csv")
if (!file.exists(strat_f)) strat_f <- file.path(tab_dir, "Table6_Country_Stratified.csv")
strat <- read_csv(strat_f, show_col_types = FALSE)

hm_labels <- c(worry_health_fear = "Health fears", worry_economic = "Economic worry",
               worry_basic_needs = "Basic-needs worry", worry_social = "Social disruption",
               worry_safety_stigma = "Safety/stigma", perceived_risk = "Perceived risk",
               livelihood_disruption = "Livelihood disruption", comorbidity = "Comorbidity")

hm <- strat %>%
  filter(term %in% names(hm_labels)) %>%
  mutate(predictor = factor(hm_labels[term], levels = rev(hm_labels)),
         sig = p.value < 0.05,
         cell_text = sprintf("%.3f%s", estimate, ifelse(sig, "*", "")),
         text_color = ifelse(abs(estimate) > 0.15, "white", "#333333"))

fig7 <- ggplot(hm, aes(x = country, y = predictor, fill = estimate)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = cell_text, color = text_color), size = 3, show.legend = FALSE) +
  scale_color_identity() +
  facet_wrap(~ outcome) +
  # Fresh blue → lilac → violet diverging palette (matches Figure 4A).
  scale_fill_gradient2(low = "#4E8EC7", mid = "#F5F2F8", high = "#8D6CB8",
                       midpoint = 0, name = expression(beta),
                       limits = c(-0.40, 0.40), oob = squish,
                       breaks = c(-0.40, -0.20, 0, 0.20, 0.40),
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "left")) +
  labs(title = "Country-Stratified Regression Coefficients",
       subtitle = "Separate multilevel models per country. * p < 0.05", x = NULL, y = NULL) +
  theme_ssm() + theme(panel.grid = element_blank(), axis.text.x = element_text(size = 9), legend.position = "bottom")

ggsave(file.path(fig_dir, "Figure7_Country_Heatmap.png"), fig7, width = 10.5, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "Figure7_Country_Heatmap.pdf"), fig7, width = 10.5, height = 5.5, bg = "white")
cat("  saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# FIGURE S1 — MICE imputation convergence trace plots
# (Methods §2.3; first cited supplementary figure)
# ═══════════════════════════════════════════════════════════════════════════════
cat("Figure S1 (MICE convergence)...\n")

suppressPackageStartupMessages(library(mice))
mi_obj <- readRDS(file.path(proj_dir, "Analysis/output/mi_object_13sites.rds"))
imputed_vars <- names(mi_obj$data)[colSums(is.na(mi_obj$data)) > 0]

# Extract chainMean / chainVar to long-format ggplot input so that styling
# matches the other figures (grey frame, Calibri, fresh blue).
cm <- mi_obj$chainMean
cv <- mi_obj$chainVar
to_df <- function(arr, stat_label) {
  d <- expand.grid(var = dimnames(arr)[[1]],
                   iter = seq_len(dim(arr)[2]),
                   chain = seq_len(dim(arr)[3]))
  d$value <- as.vector(arr)
  d$stat  <- stat_label
  d
}
mice_long <- rbind(to_df(cm, "mean"), to_df(sqrt(cv), "sd")) |>
  dplyr::filter(var %in% imputed_vars, !is.na(value))

# Prettify variable names for facet strip
pretty_var <- c(age = "Age", female = "Female", ethnicity_minority = "Ethnic minority",
                education = "Education", employment = "Employment",
                gen_health = "General health", risk_self = "Self-assessed risk",
                health_trust = "Health-system trust",
                worry_health_fear = "Worry: Health fears",
                worry_economic = "Worry: Economic",
                worry_basic_needs = "Worry: Basic-needs",
                worry_social = "Worry: Social",
                worry_safety_stigma = "Worry: Safety/stigma")
mice_long$var_pretty <- ifelse(
  as.character(mice_long$var) %in% names(pretty_var),
  pretty_var[as.character(mice_long$var)], as.character(mice_long$var))
mice_long$stat <- factor(mice_long$stat, levels = c("mean", "sd"),
                          labels = c("Chain mean", "Chain SD"))

figS1 <- ggplot(mice_long, aes(x = iter, y = value, group = chain)) +
  geom_line(color = "#4E8EC7", alpha = 0.35, linewidth = 0.35) +
  facet_grid(var_pretty ~ stat, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = seq(2, 10, 2), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "MICE chain convergence by iteration",
       subtitle = expression(italic(m)==30~"chains, 10 iterations; one panel per imputed variable"),
       x = "Iteration", y = NULL) +
  theme_ssm() +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8),
        strip.text.x = element_text(size = 9, face = "bold"),
        strip.placement = "outside",
        panel.spacing.y = unit(0.15, "lines"),
        panel.grid.minor = element_blank())

n_vars <- length(imputed_vars)
fig_w  <- 8.5
fig_h  <- max(6, 0.85 * n_vars + 1)
ggsave(file.path(fig_dir, "FigureS1_MICE_Convergence.pdf"), figS1,
       width = fig_w, height = fig_h, bg = "white")
ggsave(file.path(fig_dir, "FigureS1_MICE_Convergence.png"), figS1,
       width = fig_w, height = fig_h, dpi = 300, bg = "white")
cat("  saved.\n")

cat("\n=== 07_figures.R COMPLETE ===\n")
