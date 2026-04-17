################################################################################
# 20_figure2_methods_overview.R
# Build Figure 2 (analytical pipeline overview) from a hand-tuned HTML source.
#
# WHY HTML rather than ggplot2/grid: this figure is a typographic flowchart with
# nested 2-column rows, sequential M0->M3 with thin connectors, and varying box
# treatments (default, step, primary). HTML+CSS gives precise control over
# typography, spacing, and visual hierarchy that would be brittle to replicate
# in grid graphics. The HTML is rendered to vector PDF and high-DPI PNG via
# headless Chrome through the webshot2 package.
#
# Source : figures/source/Figure2_Methods_Overview.html (canonical layout)
# Output : figures/Figure2_Methods_Overview.pdf  (vector, journal-submission)
#          figures/Figure2_Methods_Overview.png  (300 DPI, preview)
################################################################################

suppressPackageStartupMessages({
  library(webshot2)
  library(chromote)
  library(jsonlite)
})

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
fig_dir  <- file.path(proj_dir, "Analysis", "figures")
src_html <- file.path(fig_dir, "source", "Figure2_Methods_Overview.html")

stopifnot(file.exists(src_html))
src_url <- paste0("file://", src_html)

# ── PDF: single-page, page sized to actual figure box (vector) ────────────
# webshot2's PDF path uses Chrome's default Letter paper and splits long
# figures across pages. Use chromote directly to get exact figure dimensions
# from the DOM, then printToPDF with a matching custom paper size.
# Wrapped in a function so that `on.exit()` has a well-defined scope even
# when this script is source()'d by 00_master.R (top-level on.exit fires at
# source() return, prematurely closing the ChromoteSession).
out_pdf <- file.path(fig_dir, "Figure2_Methods_Overview.pdf")

build_fig2_pdf <- function(src_url, out_pdf) {
  b <- ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  b$Page$navigate(src_url)
  Sys.sleep(1.5)   # let HTML parse + fonts settle

  dims <- b$Runtime$evaluate(
    "(function(){var r = document.querySelector('.figure2').getBoundingClientRect();
      return JSON.stringify({w: r.width, h: r.height});})()",
    returnByValue = TRUE
  )
  fig_dims  <- fromJSON(dims$result$value)
  margin_in <- 0.10
  w_in      <- fig_dims$w / 96 + 2 * margin_in
  h_in      <- fig_dims$h / 96 + 2 * margin_in

  pdf_resp <- b$Page$printToPDF(
    paperWidth        = w_in,
    paperHeight       = h_in,
    marginTop         = margin_in,
    marginBottom      = margin_in,
    marginLeft        = margin_in,
    marginRight       = margin_in,
    printBackground   = TRUE,
    preferCSSPageSize = FALSE
  )
  writeBin(base64_dec(pdf_resp$data), out_pdf)
  cat(sprintf("Wrote: %s  (%.2f x %.2f inches, single page)\n",
              out_pdf, w_in, h_in))
}

build_fig2_pdf(src_url, out_pdf)

# ── PNG: 300 DPI equivalent for preview / non-vector submission ──────────
out_png <- file.path(fig_dir, "Figure2_Methods_Overview.png")
webshot2::webshot(
  url      = src_url,
  file     = out_png,
  vwidth   = 820,
  vheight  = 1200,
  zoom     = 4,
  selector = ".figure2",
  expand   = 10
)
cat("Wrote:", out_png, "\n")

cat("\n=== 20_figure2_methods_overview.R COMPLETE ===\n")
