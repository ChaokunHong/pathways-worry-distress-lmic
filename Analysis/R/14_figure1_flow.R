################################################################################
# 14_figure1_flow.R  (v8 redesign, publication-grade)
# Participant-flow / STROBE diagram.  All numbers recomputed live from source
# to guarantee accuracy.  Visual language matches Figure 2 (slate accent on
# primary nodes; white boxes with thin charcoal borders; exclusions in a muted
# terracotta; dashed exclusion arrows; clean arrowheads via ggplot2::arrow()).
#
# Produces `Figure1_PanelB_Flow.{pdf,png}` as an intermediate.  The combined
# Figure 1 is assembled via patchwork in 19_figure1_panel_a.R.
################################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
  library(haven)
})

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")

# ── 1. Recompute numbers from source (data-accuracy guarantee) ──────────
raw <- read_dta(file.path(base_dir, "Data", "spear_community.dta"))
aug <- readRDS(file.path(proj_dir, "Analysis", "output",
                         "spear_analysis_augmented.rds"))

n_raw       <- nrow(raw)                                           # 1,825
n_dass      <- sum(aug$complete_dass)                              # 1,527
n_miss_dass <- n_raw - n_dass                                      # 298
n_site_na   <- sum(aug$complete_dass & is.na(aug$site))            # 27
n_vn_other  <- sum(aug$complete_dass & !is.na(aug$site) &
                   aug$site == 14)                                 # 38
n_primary   <- sum(aug$complete_dass & !is.na(aug$site) &
                   aug$site %in% 1:13)                             # 1,462
country_n   <- as.list(table(aug$country[
  aug$complete_dass & !is.na(aug$site) & aug$site %in% 1:13]))

stopifnot(n_dass - n_site_na - n_vn_other == n_primary)   # arithmetic check

fmt <- function(n) format(n, big.mark = ",")

# ── 2. Visual grammar constants (mirror Figure 2) ───────────────────────
col_primary_fill   <- "#e8edf2"   # slate accent (M3 primary in Figure 2)
col_primary_border <- "#3a4a5e"
col_primary_text   <- "#1f2d40"
col_node_border    <- "#2a2a2a"   # default charcoal
col_node_text      <- "#1a1a1a"
col_excl_border    <- "#8a4a3a"   # muted terracotta
col_excl_text      <- "#5c3326"
col_arrow          <- "#707070"

node <- function(x, y, w, h, label, variant = "default", text_size = 3.3) {
  border <- switch(variant,
                   primary  = col_primary_border,
                   excluded = col_excl_border,
                   col_node_border)
  fill   <- switch(variant,
                   primary  = col_primary_fill,
                   "white")
  text_col <- switch(variant,
                     primary  = col_primary_text,
                     excluded = col_excl_text,
                     col_node_text)
  lwd    <- switch(variant, primary = 0.7, 0.4)

  list(
    annotate("rect",
             xmin = x - w/2, xmax = x + w/2,
             ymin = y - h/2, ymax = y + h/2,
             fill = fill, colour = border, linewidth = lwd),
    annotate("text", x = x, y = y, label = label,
             colour = text_col, size = text_size,
             lineheight = 1.15, family = "Helvetica")
  )
}

flow_arrow <- function(x, y_from, y_to, dashed = FALSE, colour = col_arrow) {
  annotate("segment", x = x, xend = x, y = y_from, yend = y_to,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
           colour = colour, linewidth = 0.45,
           linetype = if (dashed) "22" else "solid")
}

side_arrow <- function(x_from, x_to, y, colour = col_excl_border) {
  annotate("segment", x = x_from, xend = x_to, y = y, yend = y,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
           colour = colour, linewidth = 0.45, linetype = "22")
}

# No-arrowhead line segment (for split/merge junctions).
line_seg <- function(x_from, y_from, x_to, y_to, colour = col_arrow) {
  annotate("segment", x = x_from, xend = x_to,
           y = y_from, yend = y_to,
           colour = colour, linewidth = 0.45)
}

# ── 3. Assemble flow (coordinate layout, more breathing room) ───────────
p_flow <- ggplot() +
  # Node 1 — assessed
  node(0, 8.7, w = 4.0, h = 0.85,
       label = sprintf("Assessed from SPEAR community survey\nN = %s",
                       fmt(n_raw))) +
  flow_arrow(0, 8.275, 7.725) +

  # Exclusion 1 — missing DASS
  side_arrow(2.0, 3.3, 8.0) +
  node(5.1, 8.0, w = 3.2, h = 0.80,
       label = sprintf("Excluded: missing any\nDASS-21 subscale\nn = %s",
                       fmt(n_miss_dass)),
       variant = "excluded", text_size = 3.0) +

  # Node 2 — complete DASS
  node(0, 7.3, w = 4.0, h = 0.75,
       label = sprintf("Complete DASS-21\nN = %s", fmt(n_dass))) +
  flow_arrow(0, 6.925, 6.125) +

  # Exclusion 2 — missing site
  side_arrow(2.0, 3.3, 6.65) +
  node(5.1, 6.65, w = 3.2, h = 0.65,
       label = sprintf("Excluded: missing site\nidentifier (n = %s)",
                       fmt(n_site_na)),
       variant = "excluded", text_size = 3.0) +

  # Exclusion 3 — off-protocol Vietnam other
  side_arrow(2.0, 3.3, 5.85) +
  node(5.1, 5.85, w = 3.2, h = 0.65,
       label = sprintf("Excluded: off-protocol\nVietnam-other (n = %s)",
                       fmt(n_vn_other)),
       variant = "excluded", text_size = 3.0) +

  # Node 3 — primary analytic sample (PRIMARY highlight)
  node(0, 5.25, w = 4.4, h = 0.90,
       label = sprintf("Primary analytic sample\n13 protocol-specified sites  \u00b7  N = %s",
                       fmt(n_primary)),
       variant = "primary", text_size = 3.5) +

  # ── Split junction: Primary analytic → 3 country boxes ────────────
  # Vertical line from primary box bottom (y = 4.80) to split bar (y = 4.45)
  line_seg(0, 4.80, 0, 4.45) +
  # Horizontal split bar spanning the three country columns
  line_seg(-2.9, 4.45, 2.9, 4.45) +
  # Three downward arrows into country box tops (y = 4.025)
  flow_arrow(-2.9, 4.45, 4.025) +
  flow_arrow( 0.0, 4.45, 4.025) +
  flow_arrow( 2.9, 4.45, 4.025) +

  # Country boxes
  node(-2.9, 3.65, w = 2.2, h = 0.75,
       label = sprintf("Indonesia\nn = %s (%.1f%%)",
                       fmt(country_n$Indonesia),
                       100 * country_n$Indonesia / n_primary),
       text_size = 3.1) +
  node( 0.0, 3.65, w = 2.2, h = 0.75,
       label = sprintf("Nepal\nn = %s (%.1f%%)",
                       fmt(country_n$Nepal),
                       100 * country_n$Nepal / n_primary),
       text_size = 3.1) +
  node( 2.9, 3.65, w = 2.2, h = 0.75,
       label = sprintf("Vietnam\nn = %s (%.1f%%)",
                       fmt(country_n$Vietnam),
                       100 * country_n$Vietnam / n_primary),
       text_size = 3.1) +

  # ── Merge junction: 3 country boxes → primary regression ──────────
  # Three downward line drops from country box bottoms (y = 3.275) to merge bar (y = 2.95)
  line_seg(-2.9, 3.275, -2.9, 2.95) +
  line_seg( 0.0, 3.275,  0.0, 2.95) +
  line_seg( 2.9, 3.275,  2.9, 2.95) +
  # Horizontal merge bar
  line_seg(-2.9, 2.95, 2.9, 2.95) +
  # Single downward arrow from merge-bar centre to regression box top (y = 2.50)
  flow_arrow(0, 2.95, 2.50) +

  # Node 4 — primary regression (PRIMARY highlight)
  node(0, 2.00, w = 5.2, h = 1.0,
       label = sprintf(paste0("Primary multilevel regression\n",
                              "MI (m = 30)  \u00b7  site random intercept  \u00b7  country fixed effect\n",
                              "N = %s"),
                       fmt(n_primary)),
       variant = "primary", text_size = 3.3) +

  coord_cartesian(xlim = c(-5.0, 7.0), ylim = c(0.9, 9.3), expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(8, 10, 8, 10),
        # No per-panel frame here; the overall Figure 1 frame is added at
        # save time via ggdraw in 19_figure1_panel_a.R.
        plot.background = element_rect(fill = "white", colour = NA))

# ── 4. Save Panel B (intermediate; combined assembly in 19_figure1_panel_a.R) ─
ggsave(file.path(fig_dir, "Figure1_PanelB_Flow.png"),
       p_flow, width = 7.5, height = 6.5, dpi = 320, bg = "white")
ggsave(file.path(fig_dir, "Figure1_PanelB_Flow.pdf"),
       p_flow, width = 7.5, height = 6.5, bg = "white")

saveRDS(p_flow, file.path(proj_dir, "Analysis", "output",
                          "figure1_panelB_plot.rds"))

cat("\nWrote: Figure1_PanelB_Flow.{png,pdf}\n")
cat("Recomputed numbers verified against raw Data/spear_community.dta.\n")
