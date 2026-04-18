# SSM Submission Bundle

**Target journal**: *Social Science & Medicine*
**Manuscript title**: Differential associations between worry sub-domains and depression, anxiety, and stress under a public-health shock: a cross-sectional household survey in Indonesia, Nepal, and Vietnam
**Author**: Hongchao Kun (chaokunhong@gmail.com) et al.
**Bundle organized**: 2026-04-18
**Status**: Text drafted, figures finalized, tables finalized, supplementary ready; Abstract and Cover letter **PENDING**.

---

## Folder structure

```
Submission/
├── README.md                           (this file)
├── 01_Text_Documents/                  (文本文档: all manuscript prose in .docx)
├── 02_Figures/                         (图: all main + supp figure PDFs)
├── 03_Tables/                          (表: single xlsx, all tables)
├── 04_Supplementary_Notes/             (附录: Note S1, Note S2)
└── 05_Code_Availability/               (code/data archive pointers)
```

Every file has exactly one home. No duplicates.

---

## `01_Text_Documents/` — 文本文档

Word-ready text components in manuscript order. The user assembles these into the final `manuscript.docx` (see assembly guide below).

| # | File | Contents | Status |
|---|------|----------|--------|
| 01 | *(Abstract — PENDING)* | Structured abstract (Background, Methods, Results, Conclusions) | Write after Discussion is final |
| 02 | `02_Introduction.docx` | §1 five-paragraph intro with Vancouver citations | ✅ |
| 03 | `03_Methods.docx` | §2.1 + §2.2 + §2.3 | ✅ |
| 04 | `04_Results.docx` | §3.1 – §3.7 | ✅ |
| 05 | `05_Discussion.docx` | §4.1 – §4.7 | ✅ |
| 06 | `06_References.docx` | [1]–[60] Vancouver format | ✅ |
| 07 | `07_Figure_legends.docx` | 7 main + 5 supp figure captions | ✅ |
| 08 | `08_Table_legends.docx` | 6 main + 17 supp table legends | ✅ |
| — | *(Cover letter — PENDING)* | Address editor, highlight novelty, suggest reviewers | Write after Abstract |
| — | `Manuscript_combined_review_copy.md` | All eight text components concatenated into a single markdown for reviewer skim-reading | auto-built |

Methods Supplementary Notes (S1, S2) live separately in `04_Supplementary_Notes/` because they ship in the journal's supplementary bundle, not the main manuscript.

### How to assemble the final Word manuscript

1. Open your working `manuscript.docx` (lives at `~/Documents/PhD/COVID_19/manuscript.docx`; already contains Title + Authors + Intro + Methods + References with Zotero fields)
2. After §2.3 Methods, paste the contents of `04_Results.docx`
3. After Results, paste the contents of `05_Discussion.docx`
4. Before References, insert `07_Figure_legends.docx` + `08_Table_legends.docx` as two sub-sections titled "Figure legends" and "Table legends"
5. Convert every plain-text `[N]` in the pasted Results/Discussion to Zotero fields (Add/Edit Citation; 30+ citations)
6. Ctrl+A → Zotero Refresh to renumber
7. Write Abstract at top; write Cover letter separately

---

## `02_Figures/` — 图

All 12 figures as publication-quality PDFs. Submit main figures (1–7) with the main manuscript; submit supplementary figures (S1–S5) as part of the supplementary bundle.

| # | File | Description |
|---|------|-------------|
| 1 | `Figure1_StudySites_Flow.pdf` | Study-sites map + CONSORT flow (2 panels) |
| 2 | `Figure2_Methods_Overview.pdf` | Analytical-pipeline schematic |
| 3 | `Figure3_Constructs_by_Country.pdf` | Violin plots by country (3 panels) |
| 4 | `Figure4_Coefficient_Matrix.pdf` | Worry × outcome heatmap + covariate forest |
| 5 | `Figure5_R2_Decomposition.pdf` | Marginal R² across M0–M3 |
| 6 | `Figure6_Mediation.pdf` | Five mediation pathways |
| 7 | `Figure7_Country_Heatmap.pdf` | Country-stratified coefficients |
| S1 | `FigureS1_MICE_Convergence.pdf` | MICE chain diagnostics |
| S2 | `FigureS2_M3_Residuals.pdf` | M3 residual diagnostics |
| S3 | `FigureS3_Support_Buffering.pdf` | Support × worry simple slopes |
| S4 | `FigureS4_Location_Interactions.pdf` | Worry × location simple slopes |
| S5 | `FigureS5_LeaveOneSite_Forest.pdf` | Leave-one-site forest |

Captions are in `01_Text_Documents/07_Figure_legends.docx`.

---

## `03_Tables/` — 表

A single Excel workbook with all 23 tables on separate sheets. Each sheet has the table title embedded in row 1.

| File | Sheets |
|------|--------|
| `Publication_Tables.xlsx` | `Table 1` – `Table 6` (main, 6 sheets); `Table S1` – `Table S17` (supplementary, 17 sheets) |

Legends are in `01_Text_Documents/08_Table_legends.docx`. If the journal asks to upload main tables and supplementary tables separately, extract the corresponding sheets into two workbooks at submission time (or split manually in Excel).

---

## `04_Supplementary_Notes/` — 附录

Methods supplementary notes (goes in the supplementary-materials upload, not the main manuscript).

| File | Contents |
|------|----------|
| `Supplementary_Notes.docx` | Note S1 (cluster-count justification for *k* = 13); Note S2 (SEM specifications and improper-solution diagnosis) |

---

## `05_Code_Availability/` — code/data archive

| File | Contents |
|------|----------|
| `CODE_AVAILABILITY.txt` | GitHub URL + Zenodo DOI + pipeline entry point + R version + imputation seed + data-access statement + ethics statement pointer |

Full analysis scripts (26 R files) are archived at the GitHub/Zenodo URLs listed in this text file.

---

## Pre-submission checklist

**Text completeness**
- [ ] Abstract written (~250 words, structured: Background, Methods, Results, Conclusions)
- [ ] Cover letter drafted (novelty, fit, suggested reviewers)
- [ ] Manuscript assembled from `01_Text_Documents/` into final `.docx`
- [ ] Zotero field conversion done for all pasted `[N]` citations
- [ ] Zotero Refresh run (Ctrl+A → Refresh)

**Content completeness**
- [ ] Reference [21] SPEAR parent study populated from Zotero
- [ ] Abstract word count within journal limit
- [ ] Main-text word count within journal limit (SSM default ~5,000)
- [ ] Author/affiliation/conflict-of-interest statements final
- [ ] Ethics IDs verified in §2.1

**Format checks**
- [ ] Figures ≥ 300 DPI (PDFs from `02_Figures/` are vector-quality)
- [ ] Tables loaded correctly in `.xlsx`; titles visible on each sheet
- [ ] Cross-references (Figure/Table numbers) all resolve
- [ ] Citations [1]–[60] all cited in text and all listed in References

**Technical verification (already done — 2026-04-18)**
- [x] All numerical claims verified against canonical CSVs (64/64 CSVs byte-identical after full `00_master.R` rerun)
- [x] Figures content-identical post-rerun (14/14)
- [x] Internal cross-file consistency verified (three-agent adversarial review × 2 rounds)
- [x] [46]–[60] references added; all `[N]` resolve
- [x] SEM "improper solution" language aligned across Methods S2 / Results §3.5 / Discussion
- [x] Country-stratified descriptive framing with multicollinearity caveat
- [x] Covariate raw-*p* caveat added
- [x] Mediation single-model-specificity caveat on livelihood absorption
- [x] ICC–effect correspondence reframed with full 5-domain reporting + Spearman ρ
- [x] Lajja/laj pan-South-Asian cultural precision

**Submission portal steps** (when ready)
- [ ] Upload final `manuscript.docx` as main document
- [ ] Upload 7 main figure PDFs individually from `02_Figures/`
- [ ] Upload `Publication_Tables.xlsx` (main-table sheets) as table file OR ensure tables are embedded in main docx
- [ ] Upload supplementary bundle: `Supplementary_Notes.docx` + supp-table sheets + 5 supp figure PDFs
- [ ] Upload cover letter
- [ ] Submit

---

## Provenance

This Submission bundle is regenerated from canonical project sources:

- Text: `../Thesis/*.md` → pandoc-converted to `.docx`
- Figures: `../Analysis/figures/*.pdf` (generated by `../Analysis/R/07_figures.R` and supporting scripts)
- Tables: `../Analysis/Publication_Tables_v23.xlsx` (built by `../Analysis/R/25_rebuild_publication_tables_v23.R`)

If any source file is updated, re-run the corresponding generator and re-copy to this bundle. The source-of-truth chain is: **CSV → xlsx → Submission; Markdown → docx → Submission**.
