################################################################################
# 19_figure1_panel_a.R  (v8 redesign, Lancet / NEJM journal-grade)
#
# Figure 1 Panel A:
#   Single elegant regional map of South / Southeast Asia.  Indonesia, Nepal,
#   and Vietnam are highlighted against pale neighbouring land and a subtle
#   cool ocean fill.  All 13 sites appear as dark slate markers with white
#   halos; site names are placed adjacent to markers via ggrepel and styled
#   with restrained typography.  Country names are rendered in bold uppercase
#   at each country's centroid with a subtle italic subtitle beneath giving
#   sample size and site count.  No insets, no magnifier lines — one composed
#   map, in the editorial style used by The Lancet, Lancet Global Health,
#   BMJ Global Health, and NEJM.
#
# Composition with Panel B via patchwork / cowplot -> single vector PDF.
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggrepel)
  library(cowplot)
})

fig_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process/Analysis/figures"
out_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19/v3_stress_process/Analysis/output"

# ── 1. Country polygons + site centroids ──────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

sites <- tribble(
  ~country,     ~site_short,   ~site_full,              ~lon,   ~lat,
  "Indonesia",  "Jakarta",     "DKI Jakarta",          106.85,  -6.21,
  "Indonesia",  "Jawa Barat",  "Jawa Barat",           107.62,  -6.91,
  "Indonesia",  "NTT",         "Nusa Tenggara Timur",  121.07, -10.18,
  "Nepal",      "Bhaktapur",   "Bhaktapur",             85.42,  27.67,
  "Nepal",      "Kapilvastu",  "Kapilvastu",            83.05,  27.55,
  "Nepal",      "Mustang",     "Lower Mustang",         83.72,  28.78,
  "Nepal",      "Morang",      "Morang/Sunsari",        87.28,  26.46,
  "Nepal",      "Patan",       "Patan",                 85.32,  27.67,
  "Nepal",      "Sindhu.",     "Sindhupalchowk",        85.71,  27.78,
  "Vietnam",    "Dak Lak",     "Dak Lak",              108.05,  12.71,
  "Vietnam",    "Ha Noi",      "Ha Noi",               105.85,  21.03,
  "Vietnam",    "HCMC",        "Ho Chi Minh City",     106.66,  10.78,
  "Vietnam",    "Nam Dinh",    "Nam Dinh",             106.18,  20.42
)

# ── 2. Editorial palette (warm-neutral, single accent) ────────────────
col_ocean         <- "#f4f6f8"    # very pale cool, near-white
col_neighbour     <- "#e8eaee"    # quiet gray
col_neighbour_brd <- "#ffffff"    # white interior borders (Lancet-style)
col_focus         <- "#8898b0"    # medium slate, warmer than neighbour
col_focus_brd     <- "#1f2d40"    # dark slate definition
col_marker_halo   <- "#ffffff"
col_marker_fill   <- "#1f2d40"
col_label_country <- "#1a1a1a"
col_label_site    <- "#222222"

# ── 3. Country-label anchor points + subtitle text ────────────────────
# Anchors placed inside country (where space allows) or in adjacent empty
# neighbour area (for tiny countries such as Nepal).  Each label has a
# subtitle line 1.3° south of it.
country_labels <- tribble(
  ~country,     ~lon,   ~lat,   ~label_upper,  ~subtitle_below,
  "Indonesia",  117.0,  -2.5,   "INDONESIA",   "n = 607 (41.5%)  \u00b7  3 sites",
  "Nepal",       85.0,  30.5,   "NEPAL",       "n = 460 (31.5%)  \u00b7  6 sites",
  "Vietnam",    107.5,  16.0,   "VIETNAM",     "n = 395 (27.0%)  \u00b7  4 sites"
)

# ── 4. Regional extent ───────────────────────────────────────────────
# Generous buffer so ggrepel has empty-neighbour land / ocean to push
# site labels into without clipping.
main_xlim <- c(77, 130)
main_ylim <- c(-13, 33)

focus_iso   <- c("IDN", "NPL", "VNM")
focus_sf    <- world %>% filter(iso_a3 %in% focus_iso)
region_sf   <- world %>%
  filter(!iso_a3 %in% focus_iso) %>%
  filter(st_intersects(
    geometry,
    st_as_sfc(st_bbox(c(xmin = main_xlim[1], xmax = main_xlim[2],
                         ymin = main_ylim[1], ymax = main_ylim[2]),
                      crs = 4326)),
    sparse = FALSE)[, 1])

# ── 5. Build the regional map ────────────────────────────────────────
p_map <- ggplot() +
  # Ocean / background
  annotate("rect",
           xmin = main_xlim[1], xmax = main_xlim[2],
           ymin = main_ylim[1], ymax = main_ylim[2],
           fill = col_ocean, colour = NA) +

  # Neighbouring land
  geom_sf(data = region_sf,
          fill = col_neighbour, colour = col_neighbour_brd,
          linewidth = 0.30) +

  # Focus countries
  geom_sf(data = focus_sf,
          fill = col_focus, colour = col_focus_brd,
          linewidth = 0.45) +

  # Country labels (bold uppercase, placed inside each country)
  geom_text(data = country_labels,
            aes(x = lon, y = lat, label = label_upper),
            family = "Helvetica", fontface = "bold",
            size = 3.6, colour = col_label_country) +

  # Site marker halos + fills (2-layer for print contrast)
  geom_point(data = sites, aes(x = lon, y = lat),
             colour = col_marker_halo, size = 3.0, stroke = 0) +
  geom_point(data = sites, aes(x = lon, y = lat),
             colour = col_marker_fill, size = 1.8, stroke = 0) +

  # Nepal site labels — pushed SOUTH into empty India area
  geom_text_repel(data = sites %>% filter(country == "Nepal"),
                  aes(x = lon, y = lat, label = site_short),
                  family = "Helvetica", size = 2.2,
                  colour = col_label_site,
                  nudge_y = -2.8, direction = "both",
                  min.segment.length = 0,
                  segment.colour = "#999999", segment.size = 0.25,
                  box.padding = 0.25, point.padding = 0.20,
                  force = 2, force_pull = 0.5,
                  max.overlaps = 30, seed = 42) +

  # Vietnam site labels — pushed EAST into South China Sea
  geom_text_repel(data = sites %>% filter(country == "Vietnam"),
                  aes(x = lon, y = lat, label = site_short),
                  family = "Helvetica", size = 2.2,
                  colour = col_label_site,
                  nudge_x = 2.5, direction = "y",
                  min.segment.length = 0,
                  segment.colour = "#999999", segment.size = 0.25,
                  box.padding = 0.25, point.padding = 0.20,
                  force = 1.5,
                  max.overlaps = 30, seed = 42) +

  # Indonesia site labels — pushed SOUTH into Indian Ocean
  geom_text_repel(data = sites %>% filter(country == "Indonesia"),
                  aes(x = lon, y = lat, label = site_short),
                  family = "Helvetica", size = 2.2,
                  colour = col_label_site,
                  nudge_y = -2.2, direction = "x",
                  min.segment.length = 0,
                  segment.colour = "#999999", segment.size = 0.25,
                  box.padding = 0.25, point.padding = 0.20,
                  force = 1.5,
                  max.overlaps = 30, seed = 42) +

  coord_sf(xlim = main_xlim, ylim = main_ylim,
           expand = FALSE, datum = NA) +
  labs(caption = paste(
    "Indonesia  n = 607 (41.5%)  \u00b7  3 sites.",
    "Nepal  n = 460 (31.5%)  \u00b7  6 sites.",
    "Vietnam  n = 395 (27.0%)  \u00b7  4 sites.",
    sep = "   ")) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.margin = margin(6, 10, 4, 10),
    plot.caption = element_text(size = 8.6, hjust = 0.5,
                                colour = "#333333",
                                margin = margin(t = 6)),
    panel.background = element_rect(fill = col_ocean, colour = NA),
    # Thin viewport border around the map only; the overall Figure 1 frame
    # (Panel A + Panel B combined) is added at save time via ggdraw below.
    panel.border = element_rect(fill = NA, colour = "#aaaaaa",
                                linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# ── Helper: wrap any plot in a single outer Figure-wide grey frame ───
add_outer_frame <- function(p, pad = 4) {
  cowplot::ggdraw(p) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white",
                                              colour = "#aaaaaa",
                                              linewidth = 0.5),
      plot.margin = ggplot2::margin(pad, pad, pad, pad)
    )
}

# ── 6. Save Panel A (standalone) with outer frame ────────────────────
panel_a_framed <- add_outer_frame(p_map)
ggsave(file.path(fig_dir, "Figure1_PanelA_Maps.png"),
       panel_a_framed, width = 10, height = 6.0, dpi = 320, bg = "white")
ggsave(file.path(fig_dir, "Figure1_PanelA_Maps.pdf"),
       panel_a_framed, width = 10, height = 6.0, bg = "white")

# ── 7. Combine Panel A (top) + Panel B (below) → single vector PDF ───
p_flow <- readRDS(file.path(out_dir, "figure1_panelB_plot.rds"))

# One overall Figure 1 frame wrapping both panels (no per-panel inner
# borders — `14_figure1_flow.R` and the map theme above strip their own
# `plot.background` border so only ONE outer frame is visible).
fig1 <- add_outer_frame(
  plot_grid(p_map, p_flow, ncol = 1, rel_heights = c(1.0, 2.0)),
  pad = 2
)

ggsave(file.path(fig_dir, "Figure1_StudySites_Flow.pdf"),
       fig1, width = 8.5, height = 11, bg = "white")
ggsave(file.path(fig_dir, "Figure1_StudySites_Flow.png"),
       fig1, width = 8.5, height = 11, dpi = 320, bg = "white")

# ── 7b. Save Panel B standalone with its own frame ───────────────────
panel_b_framed <- add_outer_frame(p_flow)
ggsave(file.path(fig_dir, "Figure1_PanelB_Flow.pdf"),
       panel_b_framed, width = 7.5, height = 6.5, bg = "white")
ggsave(file.path(fig_dir, "Figure1_PanelB_Flow.png"),
       panel_b_framed, width = 7.5, height = 6.5, dpi = 320, bg = "white")

cat("Wrote:\n",
    "  Figure1_PanelA_Maps.{png,pdf}  (single integrated regional map)\n",
    "  Figure1_StudySites_Flow.{png,pdf}  (Panel A + B combined)\n")
