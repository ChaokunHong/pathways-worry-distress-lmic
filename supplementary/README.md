# Supplementary Materials

Bundle for journal submission. All files referenced from `../Thesis/Methods.md`
and `../Thesis/Manuscript.md`.

## Contents

```
supplementary/
├── README.md                          (this file — inventory)
├── Supplementary_Notes.md             (Notes S1, S2)
├── Supplementary_Tables.xlsx          (Tables S1–S17, one sheet each)
└── figures/
    ├── FigureS1_MICE_Convergence.pdf       (Methods §2.3; MI diagnostic)
    ├── FigureS2_M3_Residuals.pdf           (Methods §2.3; residual diagnostic)
    ├── FigureS3_Support_Buffering.pdf      (exploratory moderator simple slopes)
    ├── FigureS4_Location_Interactions.pdf  (worry × location simple slopes)
    └── FigureS5_LeaveOneSite_Forest.pdf    (post-hoc; site leave-one-out)
```

## Index to Methods-cited supplementary items

### Supplementary Notes

| ID | Subject | Methods cite location |
|----|---------|-----------------------|
| **Note S1** | Cluster-count justification for k = 13 (Maas–Hox framing, CR2 bound, non-interpretation of variance components) | §2.3 *Primary multilevel models* |
| **Note S2** | SEM specifications (5-factor / 4-factor merged-deprivation / pure-CFA MLR) and Heywood warnings | §2.3 *Supplementary, exploratory, and sensitivity analyses* |

### Supplementary Tables

| ID | Subject | Methods cite location |
|----|---------|-----------------------|
| Table S1 | Variable construction (items, scoring, α) | §2.2 throughout |
| Table S2 | Split-sample EFA loadings + CFA fit | §2.3 |
| Table S3 | DASS-21 invariance (configural/metric/scalar/partial) | §2.3 |
| Table S4 | Worry 5-factor partial invariance | §2.3 |
| Table S5 | Missing data pattern + FMI | §2.3 |
| Table S6 | MI congeniality sensitivity | §2.3 |
| Table S7 | VIF (M3 fixed-effects block) | §2.3 |
| Table S8 | Cohen's f² (worry block, M3 over M2) | §2.3 |
| Table S9 | Mediation Rubin-pooled (Schomaker–Heumann) | §2.3 |
| Table S10 | SEM fit + structural paths (merged panels) | §2.3 |
| Table S11 | Moderation families — 5 panels | §2.3 |
| Table S12 | Binary DASS caseness multilevel logistic | §2.3 |
| Table S13 | Attenuation-corrected β | §2.3 post-hoc |
| Table S14 | Leave-one-site summary | §2.3 post-hoc |
| Table S15 | Harman CMV test | §2.3 post-hoc |
| Table S16 | Age spline LR test | §2.3 post-hoc |
| Table S17 | 14-site vs 13-site protocol-fidelity sensitivity | §2.3 post-hoc |

### Supplementary Figures

| ID | Subject | Methods cite location |
|----|---------|-----------------------|
| Figure S1 | MICE chain-mean convergence | §2.3 MI diagnostics |
| Figure S2 | M3 residual diagnostics | §2.3 *Primary multilevel models* |
| Figure S3 | Community-support × economic worry simple slopes | §2.3 moderation (Table S11 Panel A) |
| Figure S4 | Worry × location-type simple slopes | §2.3 moderation (Table S11 Panel D) |
| Figure S5 | Site-level leave-one-out forest plot | §2.3 post-hoc (Table S14) |

## Code and data availability

Methods §2.3 says "package versions and analysis scripts are provided in the
supplementary materials" and "the analysis scripts, imputation specification,
and variable codebook are provided in the supplementary materials".

For journal submission, bundle the following from `../Analysis/`:

| Artefact | Location | How to include |
|----------|----------|----------------|
| R analysis scripts | `../Analysis/R/` (26 files, entry point `00_master.R`) | zip the folder; attach as a single `SupplementaryCode.zip` |
| R package versions | generate via `Rscript -e 'sessionInfo()'` after loading the pipeline | save as `session_info.txt` inside the zip |
| Imputation specification | `01_construct_development.R` + `03c_primary_mi.R` set up MICE; spec is reproducible from those files | already covered by the scripts bundle |
| Variable codebook | `Supplementary_Tables.xlsx` → sheet `Table S1` | already in this folder |

## Ethics statement (for reference)

The parent SPEAR study was approved by the National Hospital for Tropical
Diseases Ethics Committee (Hanoi, VN), Hospital for Tropical Diseases Ethics
Committee (HCMC, VN), Ethics Committee of the Nepal Health Research Council
(Kathmandu, NP), Patan Hospital Ethics Committee (Kathmandu, NP), Ethics
Committee of the Faculty of Medicine, University of Indonesia (Jakarta, ID),
and the Oxford Tropical Research Ethics Committee (Oxford, UK); local
government permission was obtained in each context as required. These are
listed inline in Methods §2.1 — **no separate Supplementary File required**.

## Rebuild note

`Supplementary_Notes.md` and `Supplementary_Tables.xlsx` are **copies** — they
are regenerated from canonical sources. To refresh after any upstream edit:

```bash
# Refresh Notes
cp ../Thesis/Methods_Supplementary_Notes.md  Supplementary_Notes.md

# Refresh Tables xlsx (re-runs the builder)
Rscript ../Analysis/R/25_rebuild_publication_tables_v23.R
cp ../Analysis/Publication_Tables_v23.xlsx   Supplementary_Tables.xlsx

# Refresh Figures (re-runs relevant scripts)
Rscript ../Analysis/R/07_figures.R              # FigureS1
Rscript ../Analysis/R/21_audit_response_quick.R # FigureS2
Rscript ../Analysis/R/11_figure_S3_support.R    # FigureS3
Rscript ../Analysis/R/13_figure_S4_location.R   # FigureS4
Rscript ../Analysis/R/22_leave_one_site_out.R   # FigureS5
cp ../Analysis/figures/FigureS*.pdf  figures/
```
