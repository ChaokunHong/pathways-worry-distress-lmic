# Methods — Supplementary Notes

Detailed methodological notes expanding on technical points referenced in the
main Methods section. Numbered in the order of first mention in the main text.

---

## Supplementary Note S1 — Cluster-count justification for k = 13

Multilevel linear modelling with k = 13 clusters falls below the Maas and Hox
(2005; [38]) benchmark of k ≈ 30 for reasonably calibrated fixed-effect
standard errors and their k ≥ 50 benchmark for variance-component
interpretation. We treat this as a substantive constraint rather than a
contraindication, for three reasons.

First, REML fixed-effect *point estimates* remain approximately unbiased at
small k; the Maas–Hox concern is standard-error calibration, not bias. We
therefore report fixed-effect coefficients with the caveat that their
nominal confidence intervals are likely slightly too narrow in the direction
of anti-conservatism.

Second, to bound this anti-conservative direction we fit CR2 cluster-robust
standard errors with Satterthwaite adjustment (Pustejovsky and Tipton 2018;
[39]) as a pre-specified sensitivity check. In preliminary comparisons,
CR2 Satterthwaite degrees of freedom contracted to 2–5 for several of the
worry coefficients. At such low df the CR2-based confidence intervals are
several-fold wider than the REML-based intervals and are treated as a
conservative upper bound on inference rather than as the primary inferential
framework.

Third, we deliberately do *not* interpret individual site-level variance
components or individual site random-intercept BLUPs, both because of the
Maas–Hox k ≥ 50 benchmark for variance-component precision and because the
purposive site-selection strategy of the parent SPEAR study (§2.1) makes the
13 sites non-exchangeable in a way that conventional random-effect theory
assumes. Between-site clustering is used solely to adjust the fixed-effect
standard errors for within-site correlation. Cross-level interactions
involving the site-level community-support scale are explicitly flagged as
preliminary.

---

## Supplementary Note S2 — Structural equation model specifications

Two confirmatory SEMs were fitted in *lavaan* to jointly model the worry
latent structure and its associations with the DASS-21 factors.

**Five-factor specification** (theorised structure): worry indicators load
on five correlated factors (health fears, economic, basic needs, social,
safety/stigma) identical to the primary-regression decomposition in §2.2,
with the three DASS-21 factors (depression, anxiety, stress) regressed on
all five worry factors plus perceived risk, comorbidity, age, and gender.
Fitted with WLSMV on ordinal indicators: CFI = 0.998, TLI = 0.998,
RMSEA = 0.055, SRMR = 0.080 (Table S10). Although the global fit indices
exceed conventional thresholds, the post-estimation check flagged negative
residual variance for at least one indicator, producing a warning
consistent with empirical under-identification at this level of
worry-sub-domain disaggregation.

**Four-factor material-deprivation specification**: basic-needs and economic
worry are merged into a single material-deprivation factor; the remaining
three worry factors and the DASS-21 structure are unchanged. Fitted with
WLSMV: CFI = 0.997, TLI = 0.998, RMSEA = 0.060. The post-estimation check
still flags negative residual variance, indicating that the identification
concern is *not* resolved by factor merging. Both specifications reproduce
the directional pattern of worry → DASS-21 associations found in the
multilevel regression; neither is used for primary inference.

A pure-CFA refit of the five-factor worry measurement model under MLR
(no structural paths, no DASS-21 indicators; Table S10 Panel C) yielded
CFI = 0.837 and RMSEA = 0.070 — the appropriate reference for absolute
fit under MLR with ordinal items. Methodological and substantive
interpretation of both SEMs is bounded by these identification warnings;
the multilevel regression remains the primary analytic framework, and SEM
results are reported as a convergence check rather than as an independent
inferential model.
