# Handoff — SPEAR Secondary Analysis Manuscript

**Purpose**: Brief the next Claude session to continue writing **Results**.
Read this file first. Do not touch Introduction / Methods / Figures / Tables
unless the user explicitly asks — they are **final**.

**Last updated**: 2026-04-17 (Intro + Methods + Figures + Supplementary + GitHub/Zenodo + Results all done)
**Author**: Hongchao Kun (chaokunhong@gmail.com), PhD student
**Target journal**: *Social Science & Medicine*
**Status**: Intro ✅, Methods ✅, Figures ✅, Supplementary ✅, **Results ✅**, **Discussion: to be written**

## Changelog — end of 2026-04-17 session (post-Results audit)
- `Thesis/Results.md` drafted (§3.1–§3.7, ~1,700 words)
- `Figure5_R2_Decomposition` rebuilt from `R2_decomposition_MI.csv` (was sourcing CC multilevel); `07_figures.R` L194 updated
- `Methods_Supplementary_Notes.md` §S2: 5F SEM CFI 0.998→0.997, RMSEA 0.055→0.054; 4F SEM TLI 0.998→0.997, RMSEA 0.060→0.058 (aligned to TableS2b CSV)
- `ANALYSIS_DOCUMENTATION.md`: DASS metric ΔCFI −0.005→−0.007; ICC line 41.9%/34.8% → 42.7%/36.0% (aligned to TableS13 and TableS7 CSVs)
- `supplementary/Supplementary_Notes.md` re-synced from Thesis copy
- `Publication_Tables_v23.xlsx` rebuilt via `25_rebuild_publication_tables_v23.R` (CSV sources were already canonical; rebuild is defensive)
- `supplementary/Supplementary_Tables.xlsx` re-synced from Publication xlsx
- `/tmp/build_combined_md.R` updated to concatenate `Results.md`; `Manuscript.md` re-assembled
- `~/Documents/GitHub/pathways-worry-distress-lmic/` mirror file-synced (no git commit/push — user's call)
- **Full pipeline rerun (`00_master.R`, 26 steps, ~30 min) completed 2026-04-18**: diff vs pre-rerun snapshot `_backups/tables_pre_rerun_20260418_005955/` shows **64/64 CSVs byte-identical, 14/14 figure PDFs content-identical**. Pipeline is deterministic; current canonical state reproduces exactly. Highest-confidence verification available without independently re-implementing the analysis.
- `Submission/` folder created at **`/Users/hongchaokun/Documents/PhD/COVID_19/Submission/`** (project root, parallel to `v3_stress_process/` and `Data/`; re-organised 2026-04-18 from the earlier `v3_stress_process/Submission/` location). Five subfolders: `01_Text_Documents/` (10 files: Intro/Methods/Results/Discussion/References/Figure_legends/Table_legends .docx + Abstract & Cover-letter PENDING placeholders + Manuscript_combined_review_copy.md), `02_Figures/` (all 12 figures main + supp as PDFs), `03_Tables/` (single Publication_Tables.xlsx, 23 sheets), `04_Supplementary_Notes/` (Supplementary_Notes.docx = Notes S1 + S2), `05_Code_Availability/` (CODE_AVAILABILITY.txt). README at bundle root has inventory + pre-submission checklist.
- Outstanding debt (Phase 4; not blocking): 3 orphan scripts (`03_primary_regression.R`, `08_publication_tables.R`, `09_presentation.R`), stale `Methods_FINAL_v7.md` ref in `00_master.R` header, dual-named CSVs in `Analysis/tables/` (values identical)

**Word doc (`manuscript.docx`) must still be updated manually**:
1. §S2 SEM indices (fix 0.998→0.997 / 0.055→0.054 / 0.060→0.058 / second 0.998→0.997)
2. Paste new Results section
3. Ctrl+A → Update Citations

---

## 1. Start here — the only files that matter

### Canonical manuscript sources (Markdown, edit these)
| File | Contents | Status |
|------|----------|--------|
| `Thesis/Introduction.md` | 5-para Vancouver-cited introduction | ✅ final |
| `Thesis/Methods.md` | §2.1 + §2.2 + §2.3 | ✅ final |
| `Thesis/Methods_Supplementary_Notes.md` | Note S1 (k=13), Note S2 (SEM) | ✅ final |
| `Thesis/References.md` | `[1]`–`[45]` + verification tables | ✅ `[21]` is placeholder for SPEAR |
| `Thesis/Manuscript.md` | Auto-assembled combined copy | generated from the four above |

### How to regenerate Manuscript.md after any edit
```bash
Rscript /tmp/build_combined_md.R
```
(Script concatenates the 4 md files; if missing, recreate from the build_combined_md.R scaffold in the session log — just reads all four and writes them with `---` separators.)

### User's working Word doc (in Zotero workflow)
- Location: `/Users/hongchaokun/Documents/PhD/COVID_19/manuscript.docx`
- Author+affiliation block + Abstract header + Intro + Methods already filled in
- Figure 1 and Figure 2 captions already embedded after their first mention
- Zotero field codes manage citation numbers `[1]`–`[45]`; user hits **Ctrl+A → Update Citations** to refresh
- **Known editing glitch**: §2.3 last sentence currently double-wrapped ("…provided (The analysis scripts…"). User has been told how to fix; do not touch Word unless asked.

---

## 2. What the paper claims (memorise for Results writing)

### Central empirical question
Do five crisis-related worry sub-domains show **differential** associations with depression, anxiety, and stress (DASS-21)?

### Primary finding (preview from existing figures/tables)
Yes. Each worry domain has a distinctive symptom signature:
| Worry domain | Depression | Anxiety | Stress |
|---|---|---|---|
| Health fears | n.s. | **↑** | **↑** |
| Economic worry | **↑** | n.s. | **↑** |
| Basic-needs worry | **↑** | n.s. | **↑** |
| Social disruption | **↑** | **↑** | **↑** |
| Safety/stigma | **↑↑** | **↑** | n.s. |

(Exact β values are in `Analysis/tables/Table4_Primary_Results_MI.csv` and `Figure4_Coefficient_Matrix.png`. **Do not re-estimate — numbers are canonical.**)

### Sample
- n = 1,462 adults across 13 sites
- Indonesia 607 (41.5%) · Nepal 460 (31.5%) · Vietnam 395 (27.0%)
- Fieldwork Feb–Jun 2021

### Study design (for Results phrasing)
- Secondary analysis, cross-sectional household survey
- Primary analysis: **multilevel MI Rubin-pooled** (m = 30, random intercept for site, country fixed effect)
- Comparator: complete-case n = 892
- k = 13 below Maas–Hox thresholds ⇒ **only fixed effects interpreted**, no site-level variance-component discussion

---

## 3. Key decisions already made (don't re-open)

| Decision | Resolution |
|---|---|
| Title | "Differential associations between worry sub-domains and depression, anxiety, and stress under a public-health shock: a cross-sectional household survey in Indonesia, Nepal, and Vietnam" |
| Framing for crisis | "public-health shock" — **not** "COVID-19" — in title and abstract lead. Body mentions COVID-19 once. |
| 5 worry sub-domain labels | **health fears / economic worry / basic-needs worry / social disruption / safety/stigma** (no other variants) |
| Block structure (not Layer) | Block 1 = demographics (7 vars), Block 2 = crisis context (4 vars) |
| Model sequence | M0 (country + site) → M1 (+Block 1) → M2 (+Block 2) → M3 primary (+5 worry). **No M4.** |
| Invariance | Metric supported ✅, scalar not ✅ → countries compared descriptively only |
| Mediation primary | Rubin-pooled (Table S9, 5 paths) — **not** complete-case bootstrap (Table 5 is comparator) |
| SEM | Reported as *convergence check*, not primary inference (Heywood warnings; see Note S2) |
| Pre-specified tests | 15 worry × outcome with BH-FDR; exploratory families counted separately |
| Mediation language | "Indirect effect (β)" not "ACME"; "decomposition of covariation, not a causal estimate" |
| Country comparisons | Descriptive only (purposive site selection; k = 3/6/4 within country) |
| GitHub / DOI | https://github.com/ChaokunHong/pathways-worry-distress-lmic  archived at  https://doi.org/10.5281/zenodo.19631187 |

---

## 4. Canonical artefacts (cite in Results, do not regenerate)

### Tables (see `Analysis/Publication_Tables_v23.xlsx` for formatted; raw CSVs in `Analysis/tables/`)
| ID | Content | CSV |
|----|---------|-----|
| Table 1 | Sample characteristics by country | `Table1_Sample_Characteristics.csv` |
| Table 2 | DASS + 5 worry psychometrics | `Table2_psychometric_properties.csv` |
| Table 3 | Spearman correlation matrix | `Table3_correlation_matrix.csv` |
| **Table 4** | Primary MI Rubin-pooled (3 outcomes × 19 predictors) | `Table4_Primary_Results_MI.csv` |
| Table 5 | Mediation CC bootstrap (comparator) | `Table5_Mediation_Results.csv` |
| Table 6 | Country-stratified multilevel | `Table6_Country_Stratified_Multilevel.csv` |
| Table S1 | Variable construction | (xlsx) |
| Table S2 | EFA + CFA worry | (xlsx) |
| Tables S3–S4 | DASS-21 + worry invariance | (xlsx) |
| Tables S5–S6 | Missingness + MI congeniality | (xlsx) |
| Tables S7–S8 | VIF + Cohen's f² | (xlsx) |
| **Table S9** | **Mediation Rubin-pooled primary** | `TableS17_Mediation_Rubin_Pooled.csv` |
| Table S10 | SEM (merged panels) | (xlsx only) |
| Table S11 | Moderation (5 panels) | (xlsx only) |
| Table S12 | Binary DASS multilevel | `TableS26_Binary_DASS_Multilevel.csv` |
| Tables S13–S17 | Post-hoc (attenuation, leave-one-site, Harman, age spline, 14-site) | `TableS21/24/22/19/16_*.csv` |

### Figures (see `Analysis/figures/`; PDF + PNG pairs)
| ID | Content |
|----|---------|
| Figure 1 | Study sites map (A) + CONSORT flow (B) |
| Figure 2 | Analytical pipeline schematic |
| Figure 3 | Constructs by country (violin plots) |
| **Figure 4** | Worry × outcome coefficient matrix + covariate forest |
| Figure 5 | Marginal R² decomposition across M0–M3 |
| **Figure 6** | Mediation indirect effects (5 paths) |
| Figure 7 | Country-stratified coefficient heatmap |
| Figure S1 | MICE convergence |
| Figure S2 | M3 residual diagnostics |
| Figure S3 | Support × worry simple slopes |
| Figure S4 | Worry × location simple slopes |
| Figure S5 | Leave-one-site forest |

---

## 5. Pending items (user may ask about these)

| # | Item | Status |
|---|------|--------|
| 1 | **Fix broken sentence in §2.3 end of Word doc** | User has instructions; may ask for help |
| 2 | **"Data and code availability" standalone section** (Option B I proposed) | Discussed but **not yet decided** by user. If they say "do option B", move GitHub+DOI from §2.3 to a new section before References. This fixes the justified-URL spacing issue. |
| 3 | `[21]` SPEAR citation | User fills from Zotero; placeholder in References.md |
| 4 | Results section | **This is the next task.** |
| 5 | Discussion | After Results |
| 6 | Move Figure captions to "Figure legends" section at end of Word | For final submission cleanup, not now |

---

## 6. Writing Results — structural guidance

### Suggested order (SSM convention)
1. **Sample description** → Table 1 + some prose about missingness (Table S5)
2. **Psychometric validation** → Table 2 + brief EFA/CFA fit + invariance status (Tables S2–S4)
3. **Descriptive associations** → Table 3 (correlations) + Figure 3 (country distributions)
4. **Primary differential associations** → **Figure 4** + Table 4 + R² decomposition (Figure 5)
5. **Mediation convergence** → Figure 6 + Table S9
6. **Country-stratified descriptives** → Figure 7 + Table 6
7. **Sensitivity & exploratory** → brief prose referencing Tables S6–S17 + Figures S1–S5

### Word count target
SSM standard: 5000 words main text (check current guidelines). Introduction + Methods already ~1500 → Results budget ~1500–2000, Discussion ~1500.

### Tone rules (don't break)
- **Never** say "determinants" (cross-sectional constraint)
- **Differential associations** is the headline concept — use it
- Report **β + 95% CI + BH-adjusted p** for primary; raw p for secondary
- Country differences are **descriptive**, not inferential
- Mediation: "indirect effect", "decomposition of covariation" — never "causal"
- SEM: "convergence check", not "confirmatory test"
- No sentence-level dramatisation ("strikingly", "remarkably")

### What NOT to write
- Tables/Figures 1–7 + S1–S17 already exist. **Do not regenerate**, just interpret.
- Do not re-do analyses. Numbers in Table 4 etc. are final.
- Do not add new figures without explicit user request.

---

## 7. Project infrastructure

### GitHub + Zenodo
- Repo: https://github.com/ChaokunHong/pathways-worry-distress-lmic (**private**)
- Zenodo DOI: 10.5281/zenodo.19631187
- Local mirror: `~/Documents/GitHub/pathways-worry-distress-lmic/`
- Before pushing: always check nothing sensitive (Data/, mi_object*.rds, *.dta) staged

### Supplementary submission bundle
`v3_stress_process/supplementary/` has everything ready:
- `Supplementary_Notes.md` (S1 + S2)
- `Supplementary_Tables.xlsx` (S1–S17)
- `figures/FigureS1–S5.pdf`
- `README.md` (inventory + ethics statement + code availability)

### Pipeline
`Analysis/R/00_master.R` runs 26 scripts in order. **Do not rerun unless explicitly asked** — last full run produced canonical outputs currently in `Analysis/tables/` and `Analysis/figures/`.

---

## 8. How the user works

### Preferences observed
- Direct, concise responses; values accurate self-assessment over reassurance
- Wants flags for things that might be wrong — "不用担心" ≠ trust blindly
- Uses Zotero for citations (auto-renumber on Ctrl+A → Update)
- Prefers markdown canonical; Word is derived
- Values fresh blue-violet palette, grey borders on figures
- When overwhelmed says so ("我现在也混乱"); then wants you to think and lead
- Has been corrected twice for over-modification; respect "修改能改的，但是不要过度修改"

### Red flags to watch for
- User asking "都完美了对吗" / "没有问题了对吗" → they want honest gap-finding, not reassurance. Always audit, always flag remaining issues.
- Auto-generated docx output was rejected for being ugly. **Do not rebuild Manuscript.docx.** Generate Manuscript.md only.
- Linter warnings (markdown style) are noise; ignore unless blocking.

---

## 9. Opening prompt for the next session

Paste into the new Claude window:

```
Continuing the SPEAR Methods paper. Read
v3_stress_process/Thesis/HANDOFF_FOR_NEXT_SESSION.md in full first —
that's the state as of 2026-04-17. Intro + Methods + Figures +
Supplementary are final. I want to start writing Results. Open
v3_stress_process/Analysis/tables/Table4_Primary_Results_MI.csv and
Figure4_Coefficient_Matrix.png first; these are the core empirical
claim. Do not regenerate any tables or figures. Draft Results section
by section using the existing canonical numbers.
```
