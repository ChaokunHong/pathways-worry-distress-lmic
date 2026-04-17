# Pathways from Public-Health Shock Exposure to Psychological Distress

Analysis code and supplementary materials for the paper:

> **Differential associations between worry sub-domains and depression,
> anxiety, and stress under a public-health shock: a cross-sectional
> household survey in Indonesia, Nepal, and Vietnam.**
> Submitted to *Social Science & Medicine*.

Secondary analysis of the SPEAR study (n = 1,462 across 13 community sites
in Indonesia, Nepal, and Vietnam; data-collection window February–June 2021).

## What is in this repository

```
.
├── Thesis/                                  # Manuscript source (Markdown)
│   ├── Introduction.md
│   ├── Methods.md
│   ├── Methods_Supplementary_Notes.md       # Notes S1, S2
│   ├── References.md
│   └── Manuscript.md                        # Auto-assembled combined copy
├── Analysis/
│   ├── R/                                   # 26 numbered R scripts
│   │   └── 00_master.R                      # Pipeline entry point
│   ├── tables/                              # Aggregate output CSVs (64 files)
│   ├── figures/                             # Main + supplementary figures (PDF + PNG)
│   └── Publication_Tables_v23.xlsx          # Tables 1–6 + S1–S17
├── supplementary/                           # Journal-submission bundle
│   ├── README.md
│   ├── Supplementary_Notes.md
│   ├── Supplementary_Tables.xlsx
│   └── figures/FigureS1–S5.pdf
├── ANALYSIS_DOCUMENTATION.md                # Full analytical documentation
├── LICENSE-code.txt                         # MIT (for code)
├── LICENSE-content.txt                      # CC-BY-4.0 (for manuscript text/figures)
└── README.md                                # This file
```

## What is NOT in this repository

- **Raw SPEAR data** — not redistributable under the study's ethics
  approvals. Available from the parent-study authors under a data-sharing
  agreement; see Methods §2.1 for IRB details.
- **Individual-level derived datasets** (`spear_analysis_augmented.{csv,rds}`,
  `mi_object*.rds`) — excluded for the same reason.
- The pipeline entry point `00_master.R` therefore expects the raw data to be
  placed at `Data/spear_community.dta` locally. Without it the pipeline stops
  at Step 1.

## Reproducing the analysis

With the raw data in place:

```r
# From the repo root
setwd("path/to/pathways-worry-distress-lmic")
source("Analysis/R/00_master.R")
```

This runs 26 scripts in the canonical order, writing all Tables and Figures
into `Analysis/tables/` and `Analysis/figures/`. See
`ANALYSIS_DOCUMENTATION.md` for full details of each step.

### Software requirements

- R ≥ 4.4.3
- Required packages: `tidyverse`, `lme4`, `lmerTest`, `mice`, `mitml`,
  `lavaan`, `clubSandwich`, `openxlsx`, `ggplot2`, `patchwork`, `cowplot`
  (see individual script headers for the complete list)

Get a `sessionInfo()` dump matching the submitted paper by running:

```r
source("Analysis/R/00_master.R"); sessionInfo()
```

## Ethics

The parent SPEAR study was reviewed and approved by six institutional review
boards (see Methods §2.1). Local government permission was obtained in each
context as required by local regulations. This repository contains only
aggregate output and code; no individual-level data are redistributed.

## Citation

If you use this code, please cite the paper (to be filled in on acceptance)
and, if you use the code directly, the Zenodo snapshot:

```
[GitHub repo snapshot DOI — to be minted via Zenodo integration upon first
  tagged release]
```

## Licence

- **Code** (`Analysis/R/`, utility scripts): MIT (see `LICENSE-code.txt`)
- **Manuscript text and figures** (`Thesis/`, `Analysis/figures/`,
  `supplementary/`): Creative Commons Attribution 4.0 (CC-BY-4.0;
  see `LICENSE-content.txt`)

## Contact

Hongchao Kun — chaokunhong@gmail.com
