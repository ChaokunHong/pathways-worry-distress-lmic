################################################################################
# 09_presentation.R
# Generate PowerPoint presentation for supervisor meeting
################################################################################

library(officer)
library(tidyverse)
library(flextable)

proj <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process"
fig_dir <- file.path(proj, "Analysis/figures")
tab_dir <- file.path(proj, "Analysis/tables")
out_file <- file.path(proj, "Presentation_Findings.pptx")

# ── Color palette ────────────────────────────────────────────────────────────
navy   <- "#3C5488"
mauve  <- "#B07AA1"
teal   <- "#4DBBD5"
dark   <- "#1a1a1a"
grey   <- "#666666"
light  <- "#F5F5F5"

# ── Helper: add styled slide ─────────────────────────────────────────────────
add_title_slide <- function(ppt, title, subtitle = "") {
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
  ppt <- ph_with(ppt, value = title, location = ph_location_type("ctrTitle"))
  if (subtitle != "") {
    ppt <- ph_with(ppt, value = subtitle, location = ph_location_type("subTitle"))
  }
  ppt
}

add_content_slide <- function(ppt, title, body_text = NULL, img_path = NULL) {
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  ppt <- ph_with(ppt, value = title, location = ph_location_type("title"))
  if (!is.null(body_text)) {
    ppt <- ph_with(ppt, value = body_text, location = ph_location_type("body"))
  }
  if (!is.null(img_path) && file.exists(img_path)) {
    ppt <- ph_with(ppt, value = external_img(img_path, width = 8.5, height = 4.5),
                   location = ph_location(left = 0.8, top = 2, width = 8.5, height = 4.5))
  }
  ppt
}

add_two_column <- function(ppt, title, left_text, right_text) {
  ppt <- add_slide(ppt, layout = "Two Content", master = "Office Theme")
  ppt <- ph_with(ppt, value = title, location = ph_location_type("title"))
  ppt <- ph_with(ppt, value = left_text, location = ph_location_label("Content Placeholder 2"))
  ppt <- ph_with(ppt, value = right_text, location = ph_location_label("Content Placeholder 3"))
  ppt
}

# ══════════════════════════════════════════════════════════════════════════════
# BUILD PRESENTATION
# ══════════════════════════════════════════════════════════════════════════════

ppt <- read_pptx()

# ── Slide 1: Title ───────────────────────────────────────────────────────────
ppt <- add_title_slide(ppt,
  title = "Pathways from Public Health Shock to Psychological Distress in Vulnerable LMIC Communities",
  subtitle = "SPEAR Study Analysis — v3 (Stress Process Model)\nTarget: Social Science & Medicine"
)

# ── Slide 2: Research Question ───────────────────────────────────────────────
ppt <- add_content_slide(ppt,
  title = "Research Question",
  body_text = fpar(
    ftext("Do specific domains of worry — ", fp_text(font.size = 16)),
    ftext("economic concerns, health fears, basic needs, social disruption, safety/stigma",
          fp_text(font.size = 16, bold = TRUE, color = navy)),
    ftext(" — predict distinct dimensions of psychological distress (depression, anxiety, stress) through differential pathways?",
          fp_text(font.size = 16))
  )
)

# ── Slide 3: Data Overview ───────────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Study Overview: SPEAR Survey", location = ph_location_type("title"))

data_text <- block_list(
  fpar(ftext("Cross-sectional household survey, Feb-June 2021", fp_text(font.size = 14))),
  fpar(ftext("3 LMICs: Indonesia (n=607), Nepal (n=460), Vietnam (n=395)", fp_text(font.size = 14))),
  fpar(ftext("13 community sites (urban, peri-urban, rural, remote)", fp_text(font.size = 14))),
  fpar(ftext("Analytic sample: n = 1,462 (complete DASS-21)", fp_text(font.size = 14))),
  fpar(ftext("461 raw variables → 110 analysis variables (194 items used)", fp_text(font.size = 14))),
  fpar(ftext("", fp_text(font.size = 10))),
  fpar(ftext("Original dataset: 461 variables × 1,825 observations", fp_text(font.size = 12, color = grey))),
  fpar(ftext("Data utilization: 42% of raw items incorporated", fp_text(font.size = 12, color = grey)))
)
ppt <- ph_with(ppt, value = data_text, location = ph_location_type("body"))

# ── Slide 4: Theoretical Framework ──────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Theoretical Framework: Stress Process Model", location = ph_location_type("title"))

framework_text <- block_list(
  fpar(ftext("Layer 1: Structural Vulnerability", fp_text(font.size = 14, bold = TRUE, color = "#8C9FB5")),
       ftext("  →  Demographics, comorbidity, SES", fp_text(font.size = 13))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Layer 2: Shock Appraisal", fp_text(font.size = 14, bold = TRUE, color = navy)),
       ftext("  →  Perceived risk, health trust", fp_text(font.size = 13))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Layer 3: Stressor Proliferation", fp_text(font.size = 14, bold = TRUE, color = mauve)),
       ftext("  →  5 worry sub-domains, livelihood disruption", fp_text(font.size = 13))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Layer 4: Coping Resources", fp_text(font.size = 14, bold = TRUE, color = teal)),
       ftext("  →  Support, prevention, help-seeking", fp_text(font.size = 13))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Layer 5: Outcomes", fp_text(font.size = 14, bold = TRUE, color = dark)),
       ftext("  →  DASS-21 Depression, Anxiety, Stress", fp_text(font.size = 13))),
  fpar(ftext("", fp_text(font.size = 10))),
  fpar(ftext("Key innovation: Decompose 21-item worry into 5 validated sub-domains",
             fp_text(font.size = 13, italic = TRUE, color = grey)))
)
ppt <- ph_with(ppt, value = framework_text, location = ph_location_type("body"))

# ── Slide 5: Worry Decomposition ────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Key Innovation: Worry Score Decomposition",
               location = ph_location_type("title"))

worry_df <- data.frame(
  `Sub-Domain` = c("Health fears", "Economic concerns", "Basic needs", "Social/family", "Safety/stigma"),
  Items = c(5, 7, 3, 4, 2),
  alpha = c(0.870, 0.816, 0.837, 0.680, 0.861),
  Content = c("Infection, death, health system",
              "Income, rent, debt, agriculture",
              "Food, water, medicine access",
              "Loneliness, education, childcare",
              "Stigma, abuse"),
  check.names = FALSE
)

ft_worry <- flextable(worry_df) %>%
  set_header_labels(`Sub-Domain` = "Sub-Domain", Items = "Items", alpha = "\u03b1", Content = "Content") %>%
  theme_vanilla() %>%
  bg(part = "header", bg = navy) %>%
  color(part = "header", color = "white") %>%
  fontsize(size = 12, part = "all") %>%
  width(j = 1, width = 1.5) %>% width(j = 2, width = 0.6) %>%
  width(j = 3, width = 0.6) %>% width(j = 4, width = 3.5) %>%
  bold(part = "header") %>%
  align(align = "center", j = 2:3, part = "all")

ppt <- ph_with(ppt, value = ft_worry, location = ph_location(left = 0.8, top = 1.8, width = 8, height = 3))
ppt <- ph_with(ppt, value = fpar(
  ftext("CFA on independent hold-out sample (n=660): CFI = 0.990, RMSEA = 0.068",
        fp_text(font.size = 13, italic = TRUE, color = navy))),
  location = ph_location(left = 0.8, top = 5.2, width = 8, height = 0.6))

# ── Slide 6: KEY FIGURE — Coefficient Matrix ────────────────────────────────
ppt <- add_slide(ppt, layout = "Title Only", master = "Office Theme")
ppt <- ph_with(ppt, value = "Core Finding: Differential Pathways (MI, n = 1,462)",
               location = ph_location_type("title"))
img_path <- file.path(fig_dir, "Figure4_Coefficient_Matrix.png")
if (file.exists(img_path)) {
  ppt <- ph_with(ppt, value = external_img(img_path, width = 9.2, height = 4.8),
                 location = ph_location(left = 0.4, top = 1.6, width = 9.2, height = 4.8))
}

# ── Slide 7: Interpretation ──────────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Differential Pattern: What Drives What?",
               location = ph_location_type("title"))

interp_text <- block_list(
  fpar(ftext("Depression", fp_text(font.size = 16, bold = TRUE, color = navy)),
       ftext(" — broadest vulnerability (4/5 domains significant)", fp_text(font.size = 14))),
  fpar(ftext("    Economic + Basic needs + Social + Stigma", fp_text(font.size = 13, color = grey))),
  fpar(ftext("", fp_text(font.size = 8))),
  fpar(ftext("Anxiety", fp_text(font.size = 16, bold = TRUE, color = mauve)),
       ftext(" — fear-driven (health fears + stigma)", fp_text(font.size = 14))),
  fpar(ftext("    Fear of infection/death + fear of discrimination", fp_text(font.size = 13, color = grey))),
  fpar(ftext("", fp_text(font.size = 8))),
  fpar(ftext("Stress", fp_text(font.size = 16, bold = TRUE, color = teal)),
       ftext(" — reality-driven (economic + social disruption)", fp_text(font.size = 14))),
  fpar(ftext("    Income/agricultural loss + role disruption", fp_text(font.size = 13, color = grey))),
  fpar(ftext("", fp_text(font.size = 12))),
  fpar(ftext("→ Distress is NOT a uniform response. Different stressors activate different psychological dimensions.",
             fp_text(font.size = 14, bold = TRUE, italic = TRUE)))
)
ppt <- ph_with(ppt, value = interp_text, location = ph_location_type("body"))

# ── Slide 8: R² Decomposition ───────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title Only", master = "Office Theme")
ppt <- ph_with(ppt, value = "Each Layer Adds Independent Variance",
               location = ph_location_type("title"))
img_path <- file.path(fig_dir, "Figure5_R2_Decomposition.png")
if (file.exists(img_path)) {
  ppt <- ph_with(ppt, value = external_img(img_path, width = 8, height = 5),
                 location = ph_location(left = 1, top = 1.5, width = 8, height = 5))
}

# ── Slide 9: Mediation ──────────────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title Only", master = "Office Theme")
ppt <- ph_with(ppt, value = "Mediation: How Livelihood Loss Causes Depression",
               location = ph_location_type("title"))
img_path <- file.path(fig_dir, "Figure6_Mediation.png")
if (file.exists(img_path)) {
  ppt <- ph_with(ppt, value = external_img(img_path, width = 8.5, height = 4.2),
                 location = ph_location(left = 0.8, top = 1.8, width = 8.5, height = 4.2))
}
ppt <- ph_with(ppt, value = fpar(
  ftext("66% of livelihood → depression association transmitted through economic worry",
        fp_text(font.size = 14, bold = TRUE, color = navy))),
  location = ph_location(left = 0.8, top = 6.2, width = 8, height = 0.5))

# ── Slide 10: Country Differences ────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title Only", master = "Office Theme")
ppt <- ph_with(ppt, value = "Country Heterogeneity",
               location = ph_location_type("title"))
img_path <- file.path(fig_dir, "Figure7_Country_Heatmap.png")
if (file.exists(img_path)) {
  ppt <- ph_with(ppt, value = external_img(img_path, width = 9, height = 4.8),
                 location = ph_location(left = 0.5, top = 1.6, width = 9, height = 4.8))
}

# ── Slide 11: Methods Summary ────────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Analytical Approach", location = ph_location_type("title"))

methods_text <- block_list(
  fpar(ftext("Primary: ", fp_text(font.size = 14, bold = TRUE)),
       ftext("Multilevel models (lme4) with multiple imputation (m=30)", fp_text(font.size = 14))),
  fpar(ftext("   Sample restored from n=892 (CC) to n=1,462 (MI)", fp_text(font.size = 12, color = grey))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Supplementary: ", fp_text(font.size = 14, bold = TRUE)),
       ftext("SEM (lavaan, 4-factor worry model)", fp_text(font.size = 14))),
  fpar(ftext("   CFI = 0.997, TLI = 0.998, RMSEA = 0.060", fp_text(font.size = 12, color = grey))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Validation: ", fp_text(font.size = 14, bold = TRUE)),
       ftext("Split-sample EFA/CFA, 5 sensitivity analyses", fp_text(font.size = 14))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Mediation: ", fp_text(font.size = 14, bold = TRUE)),
       ftext("Bootstrap CIs (1,000 iterations), 5 pre-specified pathways", fp_text(font.size = 14))),
  fpar(ftext("", fp_text(font.size = 6))),
  fpar(ftext("Robustness: ", fp_text(font.size = 14, bold = TRUE)),
       ftext("CC vs MI (100% directionally consistent), CR2 as conservative bound", fp_text(font.size = 14)))
)
ppt <- ph_with(ppt, value = methods_text, location = ph_location_type("body"))

# ── Slide 12: Novel Contributions ────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Novel Contributions for SSM", location = ph_location_type("title"))

novel_text <- block_list(
  fpar(ftext("1. ", fp_text(font.size = 15, bold = TRUE, color = navy)),
       ftext("First Stress Process Model application to multi-country LMIC public health shock data",
             fp_text(font.size = 14))),
  fpar(ftext("", fp_text(font.size = 8))),
  fpar(ftext("2. ", fp_text(font.size = 15, bold = TRUE, color = navy)),
       ftext("Worry decomposition reveals differential pathways hidden by aggregate scoring",
             fp_text(font.size = 14))),
  fpar(ftext("   15 → 8 → 11 significant (CC → multilevel → MI)", fp_text(font.size = 12, color = grey, italic = TRUE))),
  fpar(ftext("", fp_text(font.size = 8))),
  fpar(ftext("3. ", fp_text(font.size = 15, bold = TRUE, color = navy)),
       ftext("66% mediation: livelihood loss → depression operates through economic worry",
             fp_text(font.size = 14))),
  fpar(ftext("", fp_text(font.size = 8))),
  fpar(ftext("4. ", fp_text(font.size = 15, bold = TRUE, color = navy)),
       ftext("Layer-by-layer R² decomposition validates theoretical framework empirically",
             fp_text(font.size = 14))),
  fpar(ftext("   Each layer adds 4–15% independent variance", fp_text(font.size = 12, color = grey, italic = TRUE)))
)
ppt <- ph_with(ppt, value = novel_text, location = ph_location_type("body"))

# ── Slide 13: Limitations & Next Steps ───────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Limitations & Next Steps", location = ph_location_type("title"))

lim_text <- block_list(
  fpar(ftext("Limitations:", fp_text(font.size = 15, bold = TRUE, color = mauve))),
  fpar(ftext("  • Cross-sectional: all pathways are associational, not causal", fp_text(font.size = 13))),
  fpar(ftext("  • k = 13 clusters (sites) — limited for multilevel inference", fp_text(font.size = 13))),
  fpar(ftext("  • SEM Heywood case persists (supplementary, not primary)", fp_text(font.size = 13))),
  fpar(ftext("  • Self-report measures only", fp_text(font.size = 13))),
  fpar(ftext("", fp_text(font.size = 10))),
  fpar(ftext("Before submission:", fp_text(font.size = 15, bold = TRUE, color = navy))),
  fpar(ftext("  • Refine theoretical narrative (\"selective pathway specificity\")", fp_text(font.size = 13))),
  fpar(ftext("  • Weaken causal language throughout (effect → association)", fp_text(font.size = 13))),
  fpar(ftext("  • Strengthen LMIC policy implications in Discussion", fp_text(font.size = 13))),
  fpar(ftext("  • Create Figure 2 (conceptual framework diagram)", fp_text(font.size = 13)))
)
ppt <- ph_with(ppt, value = lim_text, location = ph_location_type("body"))

# ── Slide 14: Summary ────────────────────────────────────────────────────────
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Take-Home Message", location = ph_location_type("title"))

summary_text <- block_list(
  fpar(ftext("", fp_text(font.size = 10))),
  fpar(ftext("Psychological distress during a public health shock is not a uniform response.",
             fp_text(font.size = 18, bold = TRUE, color = dark))),
  fpar(ftext("", fp_text(font.size = 14))),
  fpar(ftext("Different types of worry activate different psychological dimensions:",
             fp_text(font.size = 16, color = grey))),
  fpar(ftext("", fp_text(font.size = 10))),
  fpar(ftext("  Depression ← material deprivation + stigma", fp_text(font.size = 16, color = navy))),
  fpar(ftext("  Anxiety ← health fears + stigma", fp_text(font.size = 16, color = mauve))),
  fpar(ftext("  Stress ← economic + social disruption", fp_text(font.size = 16, color = teal))),
  fpar(ftext("", fp_text(font.size = 14))),
  fpar(ftext("→ Interventions should target specific worry domains, not generic \"stress reduction\"",
             fp_text(font.size = 15, bold = TRUE, italic = TRUE)))
)
ppt <- ph_with(ppt, value = summary_text, location = ph_location_type("body"))

# ── Save ─────────────────────────────────────────────────────────────────────
print(ppt, target = out_file)
cat("\n=== Presentation saved:", out_file, "===\n")
cat("Slides:", length(ppt), "\n")
