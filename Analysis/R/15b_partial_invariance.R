################################################################################
# 15b_partial_invariance.R
# Partial scalar invariance analysis for DASS-21 and Worry 5-factor models.
# Triggered by reviewer concern: ΔCFI = -0.031 for scalar invariance is 3x
# Chen (2007) threshold; partial invariance procedure should be attempted
# before declaring full scalar non-invariance.
#
# Procedure (Byrne, Shavelson & Muthen 1989; Steenkamp & Baumgartner 1998):
#   1. Fit configural, metric, scalar models (sanity reproduction of TableS13)
#   2. From the scalar model, identify intercepts with largest modification
#      indices (MI) - these are the most non-invariant
#   3. Sequentially free the worst intercept across groups, refit, test
#      ΔCFI from metric. Continue until |ΔCFI| ≤ 0.010 OR until majority
#      of intercepts have been freed (in which case partial invariance fails)
#   4. Report sequence + final partial invariance status
#
# Output: TableS13b_Partial_Invariance.csv
################################################################################

library(lavaan)
library(dplyr)
library(readr)

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")
out_dir  <- file.path(proj_dir, "Analysis", "output")

# Same data filter as 15_measurement_invariance.R for consistency with TableS13
d <- readRDS(file.path(out_dir, "spear_analysis_augmented.rds"))
d <- d %>% filter(complete_dass)
cat("N:", nrow(d), " | by country:\n"); print(table(d$country))

# ── Models (identical to 15_measurement_invariance.R) ──────────────────────
dass_mod <- '
  DEP =~ q47_3 + q47_5 + q47_10 + q47_13 + q47_16 + q47_17 + q47_21
  ANX =~ q47_2 + q47_4 + q47_7 + q47_9 + q47_15 + q47_19 + q47_20
  STR =~ q47_1 + q47_6 + q47_8 + q47_11 + q47_12 + q47_14 + q47_18
'

worry_mod <- '
  HFEAR =~ q51_2_quarantine + q51_3_infected + q51_4_severe + q51_5_death + q51_6_health_system
  ECON  =~ q51_10_income + q51_14_rent + q51_15_debt + q51_16_agri + q51_17_harvest + q51_18_produce + q51_21_recession
  BNEED =~ q51_11_food + q51_12_water + q51_13_medicine
  SOC   =~ q51_1_lonely + q51_7_education + q51_8_teaching + q51_9_children_home
  STIG  =~ q51_19_stigma + q51_20_abuse
'

# ── Partial invariance routine ────────────────────────────────────────────
run_partial_invariance <- function(model, label, max_steps = 8) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("Partial invariance analysis:", label, "\n")
  cat(strrep("=", 70), "\n", sep = "")

  # 1) Reproduce configural / metric / scalar for sanity
  cfg <- cfa(model, data = d, group = "country", estimator = "MLR")
  met <- cfa(model, data = d, group = "country", estimator = "MLR",
             group.equal = "loadings")
  sca <- cfa(model, data = d, group = "country", estimator = "MLR",
             group.equal = c("loadings", "intercepts"))

  fit_cfg <- fitMeasures(cfg, c("cfi.scaled", "rmsea.scaled"))
  fit_met <- fitMeasures(met, c("cfi.scaled", "rmsea.scaled"))
  fit_sca <- fitMeasures(sca, c("cfi.scaled", "rmsea.scaled"))

  metric_cfi <- as.numeric(fit_met["cfi.scaled"])
  d_sca_met  <- as.numeric(fit_sca["cfi.scaled"]) - metric_cfi

  cat(sprintf("Configural CFI = %.3f\n", fit_cfg["cfi.scaled"]))
  cat(sprintf("Metric     CFI = %.3f  (ΔCFI from configural = %+.3f)\n",
              fit_met["cfi.scaled"],
              fit_met["cfi.scaled"] - fit_cfg["cfi.scaled"]))
  cat(sprintf("Scalar     CFI = %.3f  (ΔCFI from metric     = %+.3f)\n",
              fit_sca["cfi.scaled"], d_sca_met))

  # If scalar already invariant, stop
  results <- list()
  results[[1]] <- data.frame(
    model = label, step = 0,
    freed_intercept = "(full scalar — all 21 intercepts equal)",
    n_freed = 0,
    cfi_scaled = round(as.numeric(fit_sca["cfi.scaled"]), 3),
    delta_cfi_from_metric = round(d_sca_met, 3),
    rmsea_scaled = round(as.numeric(fit_sca["rmsea.scaled"]), 3),
    partial_invariance_supported = ifelse(abs(d_sca_met) <= 0.010, "Yes", "No"),
    stringsAsFactors = FALSE
  )

  if (abs(d_sca_met) <= 0.010) {
    cat("Full scalar invariance already supported — no partial freeing needed.\n")
    return(do.call(rbind, results))
  }

  # 2) Sequentially free intercepts based on modification indices
  current_fit  <- sca
  freed_so_far <- character(0)

  for (step in 1:max_steps) {
    # Modification indices for currently-constrained intercept equality
    mi <- modindices(current_fit, op = "~1", sort = TRUE,
                     free.remove = FALSE)
    # Restrict to indicator intercepts, exclude factor means
    factor_names <- c("HFEAR", "ECON", "BNEED", "SOC", "STIG",
                      "DEP", "ANX", "STR")
    mi <- mi[!mi$lhs %in% factor_names, ]
    mi <- mi[!mi$lhs %in% freed_so_far, ]
    if (nrow(mi) == 0) break

    worst <- mi$lhs[1]
    worst_mi <- mi$mi[1]
    freed_so_far <- c(freed_so_far, worst)

    # Refit scalar model with the worst intercept freed across groups
    partial_fit <- tryCatch(
      cfa(model, data = d, group = "country", estimator = "MLR",
          group.equal   = c("loadings", "intercepts"),
          group.partial = paste0(freed_so_far, "~1")),
      error = function(e) { message("  Refit failed: ", e$message); NULL }
    )

    if (is.null(partial_fit)) break
    current_fit <- partial_fit

    fm <- fitMeasures(current_fit, c("cfi.scaled", "rmsea.scaled"))
    d_curr <- as.numeric(fm["cfi.scaled"]) - metric_cfi

    cat(sprintf("Step %d: free %-25s (MI = %5.1f)  CFI = %.3f  ΔCFI(metric) = %+.3f\n",
                step, worst, worst_mi, fm["cfi.scaled"], d_curr))

    results[[step + 1]] <- data.frame(
      model = label, step = step,
      freed_intercept = paste(freed_so_far, collapse = ", "),
      n_freed = length(freed_so_far),
      cfi_scaled = round(as.numeric(fm["cfi.scaled"]), 3),
      delta_cfi_from_metric = round(d_curr, 3),
      rmsea_scaled = round(as.numeric(fm["rmsea.scaled"]), 3),
      partial_invariance_supported = ifelse(abs(d_curr) <= 0.010, "Yes", "No"),
      stringsAsFactors = FALSE
    )

    if (abs(d_curr) <= 0.010) {
      cat(sprintf("\n→ Partial scalar invariance achieved after freeing %d of %d intercepts\n",
                  length(freed_so_far),
                  ifelse(label == "Worry (5-factor)", 21, 21)))
      break
    }
  }

  do.call(rbind, results)
}

# ── Run for both models ───────────────────────────────────────────────────
dass_partial  <- run_partial_invariance(dass_mod,  "DASS-21 (3-factor)")
worry_partial <- run_partial_invariance(worry_mod, "Worry (5-factor)")

out <- bind_rows(dass_partial, worry_partial)
out_path <- file.path(tab_dir, "TableS13b_Partial_Invariance.csv")
write_csv(out, out_path)
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Saved:", out_path, "\n")
cat(strrep("=", 70), "\n", sep = "")
print(out)
