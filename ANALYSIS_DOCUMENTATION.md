# Analysis Documentation

## Pathways from Public-Health Shock Exposure to Psychological Distress in Vulnerable LMIC Communities

**Target journal**: *Social Science & Medicine*
**Analysis version**: v3 Stress-Process Model — **Methods v23** (canonical)
**Last updated**: 2026-04-17 (synchronised with `Introduction.md`, `Methods.md`, `Methods_Supplementary_Notes.md`, `References.md`, `Publication_Tables_v23.xlsx`, and the `00_master.R` pipeline)

---

## 1. Project Overview

### Research question

What are the differential pathways through which a large-scale public-health shock affects psychological distress (depression, anxiety, stress) among vulnerable populations in LMICs, and do specific domains of worry — health fears, economic worry, basic-needs worry, social disruption, safety/stigma — predict distinct dimensions of distress?

### Data source

SPEAR (Van Nuil et al. 2021, Wellcome Open Res 6:352): cross-sectional household survey, Feb–Jun 2021, 13 community sites pre-specified in the SPEAR protocol across Indonesia, Nepal, Vietnam.

| Sample stage | n |
|---|---|
| Enrolled | 1,825 |
| Complete DASS-21 | 1,527 |
| Missing site identifier (excluded) | 27 |
| "Vietnam other" off-protocol (excluded) | 38 |
| **Primary analytic sample (13 sites)** | **1,462** |
| — Indonesia | 607 (41.5%) |
| — Nepal | 460 (31.5%) |
| — Vietnam | 395 (27.0%) |
| Complete-case subset | 892 |
| Handled by multiple imputation | 570 |

### Analysis history

| Version | Approach | Outcome |
|---|---|---|
| v1 (thesis) | Country-stratified logistic, binary DASS-21 | No multilevel, no framework |
| v2 (multilevel) | Multilevel mixed-effects, aggregate worry | Aggregate near-tautological |
| **v3 (current, v23)** | **Stress-Process Model; 5 worry sub-domains; multilevel + MI Rubin-pool; partial invariance; comprehensive supplement** | **See §8** |

The transition from thesis-length v8 Methods (~3,000 words) to journal-length v23 Methods (~1,000 words, including inline IRB list and GitHub/Zenodo URLs) was driven by SSM style; v8 content was redistributed into Supplementary Tables S1–S17 and Notes S1–S2.

---

## 2. Theoretical Framework

Pearlin's Stress-Process Model + Hobfoll's Conservation of Resources: structural vulnerability → shock exposure & appraisal → stressor proliferation (worry sub-domains) → coping resources → distress outcomes. In v23, covariate groupings are called **Blocks** (not Layers) explicitly to avoid causal reading — the block structure is a variance-partitioning convenience, not a temporal DAG.

| Block | Variables | Source |
|---|---|---|
| Block 1 (demographics) | age, gender, ethnic-minority, education, employment, gen. health, comorbidity | q1–q7, q33, q43 |
| Block 2 (crisis context) | perceived health threat (0–30), infection risk (0–4), health-system trust (0–8), livelihood disruption (0–12) | q27, q31, q28+q29, q8 |
| Focal exposures | 5 worry sub-domains | q51 (21 items) |
| Moderators (exploratory) | HCW-in-household; site-level community support | q52 |
| Outcomes | DASS-21 depression / anxiety / stress (z-standardised) | q47 |

---

## 3. Variable Construction

### DASS-21

Prorated subscale sums (≥6/7 items), ×2 for DASS-42 equivalence, z-standardised on the DASS-complete sample (n = 1,527; primary-sample mean ≈ 0 / SD ≈ 0.97). Binary caseness uses Lovibond cut-offs on the DASS-42 scale: **Dep ≥ 14, Anx ≥ 10, Str ≥ 19**.

### Worry 5-factor (split-sample validation)

Sample with complete q51 (n = 1,320) split 50/50 (seed = 123) into exploratory and confirmatory halves. EFA: ML extraction, oblimin, polychoric, Horn's parallel analysis. CFA: WLSMV on n = 660 hold-out.

| Sub-domain | Items | Range | α |
|---|---|---|---|
| Health fears | q51_2, q51_3, q51_4, q51_5, q51_6 | 0–15 | 0.871 |
| Economic | q51_10, q51_14, q51_15, q51_16, q51_17, q51_18, q51_21 | 0–21 | 0.818 |
| Basic needs | q51_11, q51_12, q51_13 | 0–9 | 0.838 |
| Social | q51_1, q51_7, q51_8, q51_9 | 0–12 | 0.681 |
| Safety/stigma | q51_19, q51_20 | 0–6 | S-B 0.864 |

Hold-out CFA fit: CFI = 0.990, TLI = 0.988, RMSEA = 0.068, SRMR = 0.076.

### Measurement invariance (DASS-21 and worry 5-factor)

Multi-group CFA, MLR estimator (WLSMV precluded by sparse 4-point cells in one country). Results:

| Level | DASS-21 ΔCFI | Worry 5-factor ΔCFI |
|---|---|---|
| Metric vs configural | −0.007 ✓ | −0.005 ✓ |
| Full scalar vs metric | −0.031 ✗ | −0.031 ✗ |
| **Partial scalar (4 of 21 intercepts freed)** | **−0.008 ✓** | **−0.010 ✓** |

Freed intercepts are substantively interpretable: DASS (three stress-arousal items + q47_15 anxiety); Worry (q51_2 quarantine, q51_3 infection, q51_8 teaching, q51_9 children-home — mapping to differential pandemic-phase and school-closure context).

### Block 2 composites (all details in Table S1)

| Variable | Items | Likert | Range | α |
|---|---|---|---|---|
| Perceived health threat | q27_1–q27_6 | 0–5 | 0–30 | 0.85 |
| Infection risk | q31 | 0–4 | 0–4 | — |
| Health-system trust | q28 + q29 | 0–4 each | 0–8 | — |
| Livelihood disruption | q8_1, q8_3, q8_6, q8_8 | 0–3 each | 0–12 | 0.74 |

### Moderator: site-level community support

Per-site mean of individual 13-item binary support-source count. KR-20 = 0.64, tetrachoric α = 0.84. ICC(1) = 0.08 (between-site variance), ICC(2) = 0.90 (site-mean reliability, mean n ≈ 102). Observed range 1.60–3.59.

---

## 4. Statistical Methods

### Missing data

MICE (van Buuren & Groothuis-Oudshoorn 2011) under MAR. m = 30 imputations, 10 iterations, seed = 2026. PMM for continuous, logistic/polytomous for categorical. Site identifier **excluded** from predictor matrix (congeniality sensitivity Table S6: |Δβ| ≤ 0.003, no significance change). Rubin pooling via `mitml::testEstimates()`; Barnard–Rubin df. Max FMI among focal worry coefficients = 0.132. Convergence verified via chain-mean trace plots (Figure S1).

### Primary multilevel regression

For each z-standardised DASS-21 subscale:
```
outcome ~ 5 worry sub-domains + Block 1 + Block 2 + country + (1 | site)
```

- `lme4::lmer` + `lmerTest` (Satterthwaite df)
- Fitted on each of 30 imputed datasets; pooled via Rubin's rules
- Random slopes not fitted (k = 13 yields imprecise variance components)
- VIF max = 3.6 (basic-needs), well below 5 (Table S7)
- Residual diagnostics show no gross departures (Figure S2)
- Cluster-count justification in **Supplementary Note S1** (Maas–Hox thresholds, CR2 df contraction, why REML is acceptable at k = 13)

### Sequential R² decomposition (marginal R²)

| Model | Depression | Anxiety | Stress |
|---|---|---|---|
| M0 (country + site RI) | 0.005 | 0.010 | 0.005 |
| M1 (+ Block 1) | 0.076 | 0.086 | 0.073 |
| M2 (+ Block 2) | 0.110 | 0.145 | 0.123 |
| **M3 (+ 5 worry)** | **0.234** | **0.258** | **0.252** |

Cohen's f² for worry block over M2: **0.18 / 0.14 / 0.16** (medium effect; Table S8).

### Multiple testing

Benjamini–Hochberg FDR across **15 pre-specified tests** (5 worry × 3 outcomes). All 11 nominally significant coefficients remained significant after FDR (max p_adj = 0.026). Exploratory families (~105 tests) corrected within-family only.

### Mediation (two specifications)

| Specification | n | Canonical | Interpretation |
|---|---|---|---|
| **Rubin-pooled across m = 30 (PRIMARY)** | 1,462 | **Table S9** | `04b_mediation_rubin_pooled.R`; Schomaker–Heumann framework; Barnard–Rubin CIs |
| Complete-case bootstrap (comparator) | 1,086–1,149 | Table 5 | `04_mediation.R`; 1,000 bootstraps |

All 5 pathways: ACME p < 0.001, FMI ≤ 0.04.

### SEM (exploratory)

Both 5-factor (theorised) and 4-factor (material-deprivation merged) specifications via `lavaan` WLSMV; both yield CFI ≥ 0.997 but both **fail post-estimation check** (negative residual variance). Factor merging does not resolve the identification issue. SEM is reported as convergence check only; multilevel regression remains primary. Details in **Supplementary Note S2**; fit indices in Table S10.

### Exploratory moderation (4 families, consolidated into Table S11)

- Community support × worry (15 tests)
- Stigma contextual decomposition + stigma × HCW
- Worry × location type (45 tests)
- Gender × worry + age × worry (30 tests)

### Sensitivity and robustness

**5 pre-specified**: (i) complete-case re-estimation; (ii) aggregate worry as single predictor; (iii) multilevel logistic for binary DASS caseness (Table S12); (iv) CR2 cluster-robust SE; (v) exclusion of q51_1 (loneliness) + q51_20 (abuse) for criterion contamination.

**5 post-hoc diagnostics** (Tables S13–S17):
- S13 Attenuation-corrected β (upper bound for measurement error)
- S14 Leave-one-site influence (39 refits; max |Δβ| = 0.073)
- S15 Harman's single-factor CMV (30.9% variance — well below 50%)
- S16 Age non-linearity (spline vs linear LR)
- S17 14-site inclusion (|Δβ| ≤ 0.008, 15/15 sign agreement)

---

## 5. Results Summary

### Primary worry × outcome coefficients (MI, n = 1,462, Table 4)

| Sub-domain | Depression | Anxiety | Stress |
|---|---|---|---|
| Health fears | −0.011 | **0.055\*\*\*** | **0.030\*** |
| Economic | **0.028\*\*** | 0.013 | **0.033\*\*** |
| Basic needs | **0.074\*\*\*** | 0.037 | **0.047\*** |
| Social | **0.051\*\*\*** | **0.043\*\*\*** | **0.068\*\*\*** |
| Safety/stigma | **0.091\*\*** | **0.075\*\*** | 0.021 |

**11/15 BH-FDR significant (max p_adj = 0.026); 15/15 directional agreement with CC comparator.**

### Rubin-pooled mediation (Table S9)

| Pathway | ACME | 95% CI | FMI | % mediated |
|---|---|---|---|---|
| Livelihood → Economic → Dep | 0.033 | [0.022, 0.045] | 0.019 | 67% |
| Livelihood → Basic needs → Dep | 0.025 | [0.016, 0.035] | 0.031 | 52% |
| Risk → Health fear → Anx | 0.015 | [0.010, 0.021] | 0.028 | 44% |
| Risk → Health fear → Str | 0.015 | [0.010, 0.020] | 0.036 | 49% |
| Risk → Safety/stigma → Dep | 0.009 | [0.005, 0.013] | 0.025 | 37% |

All p < 0.001; all FMI < 0.04.

### Patterns

- **Depression**: broad vulnerability (economic, basic needs, social, safety/stigma)
- **Anxiety**: fear-driven (health fears, social, safety/stigma)
- **Stress**: reality-driven (economic, social)
- **Social disruption**: only domain significant across all three outcomes
- **Community context**: basic-needs (ICC 42.7%) and safety/stigma (ICC 36.0%) most clustered between sites
- **Location moderation**: peri-urban amplifies material-type worries; remote attenuates health fears
- **HCW household**: amplifies stigma → anxiety (β = 0.124, p = 0.025)
- **Individual moderation**: 0/30 gender × worry or age × worry interactions survive BH-FDR

---

## 6. Methodological Decisions Log

| Decision | Rationale | Where defended |
|---|---|---|
| 13 sites over 14 | Protocol-faithful; Vietnam-other is off-protocol | §2.1; Table S17 |
| Primary = MI, not CC | 40% sample loss if CC; MAR supported by outcome check | §2.3 |
| lme4 + Satterthwaite over CR2 | k = 13 contracts CR2 df to 2–5 | Note S1 |
| M3 as primary (not M4 with coping) | M4 loses additional 40% sample | §2.3 |
| Partial invariance reported | Full scalar ΔCFI = −0.031 (3× Chen); partial achieves −0.008/−0.010 | Tables S3, S4 |
| Rubin-pooled mediation as primary | `mediation` does not natively accept MI; Schomaker–Heumann framework is cleaner | Table S9 |
| 4-factor SEM not a "Heywood fix" | Both specifications fail post.check | Note S2 |
| Binary DASS uses glmer with site RI | True multilevel logistic | Table S12 |
| Blocks, not Layers | Block structure is descriptive, not causal | §2.2 |

---

## 7. Limitations

1. **Cross-sectional** — all pathways associational, not causal
2. **Self-report** — DASS validated in region; worry is bespoke SPEAR instrument
3. **Purposive site selection** — HCW prevalence 20.9% reflects this
4. **k = 13 clusters** — below Maas–Hox thresholds; variance components not interpreted
5. **SEM post.check fails** in both specifications; SEM is supplementary only
6. **Scalar invariance not supported** — cross-country latent mean comparisons not attempted
7. **Multiple testing** — BH-FDR on 15 primary tests; exploratory families within-family corrected
8. **Pandemic-phase heterogeneity** — country FE partially absorbs; not fully resolved
9. **Cross-cultural semantics** — "stigma" translations (*malu* / *lajja* / *xấu hổ*) differ; 2-item index cannot disambiguate

---

## 8. File Inventory

### R scripts — 00_master.R runs 26 steps end-to-end (~26 min)

```
01 construct_development       Raw → augmented.rds
02 descriptives                Table 1 + psychometrics + variance summary
03b primary_multilevel         CC multilevel comparator
03c primary_mi                 Archival 14-site MI
17 primary_13sites             ★ CANONICAL 13-site MI (n = 1,462)
04 mediation                   CC bootstrap mediation (Table 5)
05 sem_analysis                SEM (Table S10)
06 sensitivity                 Sensitivity analyses (glmer fix for S12)
10 context_effects             Support × worry, stigma contextual
12 supervisor_v2_responses     HCW×stigma, country FE, Nepal-ref, worry×location
15 measurement_invariance      Configural/metric/scalar (Table S3)
15b partial_invariance         Sequential intercept-freeing (Table S4)
16 methodological_fixes        MI congeniality (Table S6)
18 rerun_all_on_13sites        ★ CANONICAL CONSOLIDATION — runs AFTER 10/12
04b mediation_rubin_pooled     Rubin-pool mediation (Table S9)
21 audit_response_quick        VIF, residuals, f², attenuation, Harman, SEM verify
22 leave_one_site_out          Leave-one-site (Table S14)
23 moderator_exploratory       Gender/age × worry (within Table S11)
24 binary_dass_multilevel      glmer binary DASS (Table S12)
07 figures                     Figures 3–7 + Figure S1 (MICE trace)
25 rebuild_publication_tables_v23   ★ NEW v23 — 23-sheet xlsx (6 main + 17 supp)
11 figure_S1                   Community support simple slopes
13 figure_S2                   Location interaction simple slopes
14 figure1_flow                Figure 1 Panel B
19 figure1_panel_a             Figure 1 Panel A + combined
20 figure2_methods_overview    Figure 2 (chromote HTML → PDF)
```

### Publication_Tables_v23.xlsx — 23 sheets (first-mention order)

**Main (6)**: Table 1 (sample), 2 (psychometrics), 3 (correlations), 4 (primary MI worry coefficients), 5 (CC mediation comparator), 6 (country-stratified).

**Supplementary (17)** — numbered S1 through S17 in first-mention order:

| # | Content |
|---|---|
| S1 | Variable construction (items, scoring, reliability for all derived measures) |
| S2 | Split-sample EFA loadings + hold-out CFA fit |
| S3 | DASS-21 measurement invariance (configural/metric/scalar + partial) |
| S4 | Worry 5-factor measurement invariance + partial |
| S5 | Missing data pattern (complete vs incomplete comparison) |
| S6 | MI congeniality sensitivity (site dummies retained) |
| S7 | VIF for M3 fixed-effects block |
| S8 | Cohen's f² for worry block |
| S9 | Rubin-pooled mediation (5 pathways) |
| S10 | SEM — fit indices + structural paths (5-factor, 4-factor, MLR verify) |
| S11 | Moderation families (support × worry + stigma contextual + worry × location + gender/age × worry) |
| S12 | Binary DASS multilevel logistic (glmer) |
| S13 | Attenuation-corrected β |
| S14 | Leave-one-site influence |
| S15 | Harman's single-factor CMV |
| S16 | Age non-linearity spline |
| S17 | 14-site inclusion sensitivity |

### Figures (12 PDF + PNG pairs)

| File | Content |
|---|---|
| Figure 1 | Lancet-style regional map + STROBE flow |
| Figure 2 | Analytical pipeline schematic |
| Figure 3 | Constructs by country (violin plots) |
| Figure 4 | A: worry × outcome heatmap (blue-lilac-violet); B: covariate forest |
| Figure 5 | Sequential R² gain |
| Figure 6 | Mediation forest (Rubin-pooled) |
| Figure 7 | Country heatmap (same palette) |
| Figure S1 | MICE chain-mean convergence |
| Figure S2 | M3 residual diagnostics |
| Figure S3 | Support buffering simple slopes (from 11_figure_S3_support.R) |
| Figure S4 | Location interaction simple slopes (from 13_figure_S4_location.R) |
| Figure S5 | Leave-one-site forest |

All figures have consistent `#aaaaaa` 0.5pt gray outer frame.

### Supplementary Notes (S1, S2 — in `Methods_Supplementary_Notes.md`)

- **Note S1** — k = 13 cluster-count justification (Maas–Hox, CR2 df contraction, why REML works)
- **Note S2** — SEM specifications + post-estimation check results (both 5F and 4F fail; neither resolves the other)

### Code and data availability

Full pipeline (26 R scripts, session info, MI specification, variable
codebook, STROBE checklist) archived at:

- GitHub: https://github.com/ChaokunHong/pathways-worry-distress-lmic
- Zenodo snapshot: https://doi.org/10.5281/zenodo.19631187

Methods §2.3 references these URLs directly; no separately-numbered
"Supplementary Codes S1–S4" or "Supplementary File S1" (previous drafts'
placeholder numbering has been retired).

---

## 9. Reproducibility

```bash
cd /Users/hongchaokun/Documents/PhD/COVID_19
Rscript v3_stress_process/Analysis/R/00_master.R   # 26 steps, ~26 min
```

**R 4.4.3**. Key packages: `mice`, `lme4`, `lmerTest`, `mitml`, `lavaan`, `MuMIn`, `mediation`, `tidyverse`, `patchwork`, `cowplot`, `ggrepel`, `rnaturalearth`, `RColorBrewer`, `openxlsx`, `webshot2`, `chromote`.

**Seeds**: 123 (EFA/CFA split), 2026 (MICE), 42 (mediation bootstrap).

**Repository**: https://github.com/ChaokunHong/pathways-worry-distress-lmic (private during peer review). Pinned Zenodo snapshot: **https://doi.org/10.5281/zenodo.19631187**. Raw SPEAR data are excluded per the ethics approvals listed in Methods §2.1; all aggregate outputs and scripts are reproducible from the pipeline given access to the raw `.dta` file.

---

## 10. Directory Structure (current)

```
COVID_19/
├── Data/                             # raw (read-only)
│   ├── spear_community.dta
│   └── Codebook/
├── _archive_prior_versions/          # historical work bundled
│   ├── First_Version/
│   ├── v1_thesis/
│   ├── v2_multilevel/
│   └── README.md
└── v3_stress_process/                # ★ canonical current
    ├── ANALYSIS_DOCUMENTATION.md     # THIS FILE
    ├── supplementary/                # journal-submission supplementary bundle
    │   ├── README.md                 # inventory + rebuild commands
    │   ├── Supplementary_Notes.md    # Notes S1, S2
    │   ├── Supplementary_Tables.xlsx # Tables S1–S17
    │   └── figures/FigureS1–S5.pdf
    ├── Thesis/
    │   ├── Introduction.md                       # canonical Intro (Vancouver)
    │   ├── Methods.md                            # canonical Methods
    │   ├── Methods_Supplementary_Notes.md        # Notes S1 + S2
    │   ├── References.md                         # Vancouver-numbered [1]–[45], all verified ✅
    │   ├── Manuscript.md                         # auto-assembled review copy (Intro + Methods + Notes + Refs)
    │   ├── HANDOFF_FOR_NEXT_SESSION.md
    │   └── archive/                              # v1–v8 historical drafts
    └── Analysis/
        ├── R/                        # 26 canonical scripts
        ├── tables/                   # raw CSVs (Table 1-6, S1-S17 source)
        ├── figures/                  # 12 PDF + PNG pairs (grey frames)
        │   └── source/               # Figure 2 HTML source
        ├── output/                   # intermediate RDS
        │   └── archive/
        └── Publication_Tables_v23.xlsx   # ★ 23 sheets, first-mention order
```

---

**End of v23-synchronised analysis documentation.** Any future edit to Methods, tables, figures, or pipeline scripts should be reflected here to keep this file the single source of truth.
