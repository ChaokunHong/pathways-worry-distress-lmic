################################################################################
# 04b_mediation_rubin_pooled.R
# Multiple-imputation-aware mediation analysis via Rubin's-rules pooling.
#
# WHY: The original 04_mediation.R runs mediation::mediate() on complete cases
# only (n = 1,086-1,149) because the mediation package does not natively accept
# multiply-imputed data. v7 disclosed this as a limitation. To address reviewer
# concern, this script runs mediation on each of the m=30 imputed datasets
# independently, then pools indirect effects (a*b), direct effects, and total
# effects across imputations using Rubin's rules.
#
# OUTPUT: TableS17_Mediation_Rubin_Pooled.csv (NEW supplement table; does NOT
# overwrite Table5_Mediation_Results.csv from 04_mediation.R)
#
# COMPUTATIONAL COST: 5 pathways x 30 imputations x 500 bootstraps = 75,000
# mediation fits. Expect ~30-60 minutes wall time.
#
# NOTE on scoping: mediate() refits models during bootstrap and resolves
# 'data' by name in the parent environment. Each imputed dataset is therefore
# assigned to the global environment as 'd_imp' before each pair of lm() fits,
# and lm() is invoked via eval(bquote(...)) so the formula is inlined into
# the call. This pattern is documented in mediation::mediate help.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(mice)
  library(mediation)
})
select <- dplyr::select

base_dir <- "/Users/hongchaokun/Documents/PhD/COVID_19"
proj_dir <- file.path(base_dir, "v3_stress_process")
out_dir  <- file.path(proj_dir, "Analysis", "output")
tab_dir  <- file.path(proj_dir, "Analysis", "tables")

mi_obj <- readRDS(file.path(out_dir, "mi_object_13sites.rds"))
M      <- mi_obj$m
N_BOOT <- 500
SEED   <- 42

cat("MI object: m =", M, "imputations | n per dataset =", nrow(mi_obj$data), "\n")
cat("Bootstrap replicates per imputation:", N_BOOT, "\n\n")

covs    <- c("age", "female", "ethnicity_minority", "education",
             "comorbidity", "gen_health", "country")
cov_str <- paste(covs, collapse = " + ")

# ── Rubin's-rules pooling for a single quantity ───────────────────────────
rubin_pool <- function(estimates, ses) {
  m   <- length(estimates)
  Q   <- mean(estimates)              # pooled point estimate
  W   <- mean(ses^2, na.rm = TRUE)    # within-imputation variance
  B   <- var(estimates)               # between-imputation variance
  T_  <- W + (1 + 1/m) * B            # total variance
  SE  <- sqrt(T_)
  r        <- (1 + 1/m) * B / W
  v_old    <- (m - 1) * (1 + 1/r)^2   # Barnard-Rubin df
  fmi      <- (r + 2/(v_old + 3)) / (r + 1)
  ci_lo    <- Q - qt(0.975, df = v_old) * SE
  ci_hi    <- Q + qt(0.975, df = v_old) * SE
  z_stat   <- Q / SE
  p_val    <- 2 * pt(-abs(z_stat), df = v_old)
  list(Q = Q, SE = SE, ci_lo = ci_lo, ci_hi = ci_hi,
       p = p_val, fmi = fmi, df = v_old)
}

# ── 5 pathways (same as 04_mediation.R) ───────────────────────────────────
pathways <- list(
  list(X = "livelihood_disruption", M = "worry_economic",
       Y = "z_depression", lbl = "Livelihood -> Econ Worry -> Depression"),
  list(X = "livelihood_disruption", M = "worry_basic_needs",
       Y = "z_depression", lbl = "Livelihood -> Basic Needs -> Depression"),
  list(X = "perceived_risk",        M = "worry_health_fear",
       Y = "z_anxiety",    lbl = "Perceived Risk -> Health Fear -> Anxiety"),
  list(X = "perceived_risk",        M = "worry_health_fear",
       Y = "z_stress",     lbl = "Perceived Risk -> Health Fear -> Stress"),
  list(X = "perceived_risk",        M = "worry_safety_stigma",
       Y = "z_depression", lbl = "Perceived Risk -> Safety/Stigma -> Depression")
)

# ── Run mediation on each imputed dataset for each pathway ────────────────
all_results <- list()
t0 <- Sys.time()

for (p in seq_along(pathways)) {
  pw <- pathways[[p]]
  cat(sprintf("\n[%d/%d] %s\n", p, length(pathways), pw$lbl))

  f_med <- as.formula(paste(pw$M, "~", pw$X, "+", cov_str))
  f_out <- as.formula(paste(pw$Y, "~", pw$X, "+", pw$M, "+", cov_str))

  per_imp <- vector("list", M)

  for (m in 1:M) {
    # Assign current imputed dataset to globalenv so bootstrap can resolve it
    assign("d_imp", complete(mi_obj, m), envir = globalenv())

    m_med <- eval(bquote(lm(.(f_med), data = d_imp)))
    m_out <- eval(bquote(lm(.(f_out), data = d_imp)))

    seed_m <- SEED + 1000 * p + m
    set.seed(seed_m)
    med <- mediate(m_med, m_out, treat = pw$X, mediator = pw$M,
                   boot = TRUE, sims = N_BOOT, boot.ci.type = "perc")

    acme_se  <- if (!is.null(med$d0.sims))  sd(med$d0.sims)  else NA_real_
    ade_se   <- if (!is.null(med$z0.sims))  sd(med$z0.sims)  else NA_real_
    total_se <- if (!is.null(med$tau.sims)) sd(med$tau.sims) else NA_real_

    per_imp[[m]] <- tibble(
      acme_est  = med$d0,        acme_se  = acme_se,
      ade_est   = med$z0,        ade_se   = ade_se,
      total_est = med$tau.coef,  total_se = total_se,
      prop_med  = med$n0
    )

    if (m %% 5 == 0) {
      elapsed_min <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
      cat(sprintf("  imputation %2d/%d | elapsed %.1f min\n", m, M, elapsed_min))
    }
  }
  per_imp_df <- bind_rows(per_imp)

  acme_pool  <- rubin_pool(per_imp_df$acme_est,  per_imp_df$acme_se)
  ade_pool   <- rubin_pool(per_imp_df$ade_est,   per_imp_df$ade_se)
  total_pool <- rubin_pool(per_imp_df$total_est, per_imp_df$total_se)
  prop_mean  <- mean(per_imp_df$prop_med)

  all_results[[p]] <- tibble(
    label = pw$lbl,
    n_per_imputation = nrow(mi_obj$data),
    m_imputations    = M,
    n_bootstraps     = N_BOOT,
    # ACME (a*b indirect)
    acme         = round(acme_pool$Q, 4),
    acme_se      = round(acme_pool$SE, 4),
    acme_ci_low  = round(acme_pool$ci_lo, 4),
    acme_ci_high = round(acme_pool$ci_hi, 4),
    acme_p       = signif(acme_pool$p, 4),
    acme_fmi     = round(acme_pool$fmi, 3),
    # ADE (direct)
    ade          = round(ade_pool$Q, 4),
    ade_se       = round(ade_pool$SE, 4),
    ade_ci_low   = round(ade_pool$ci_lo, 4),
    ade_ci_high  = round(ade_pool$ci_hi, 4),
    ade_p        = signif(ade_pool$p, 4),
    # Total
    total        = round(total_pool$Q, 4),
    total_se     = round(total_pool$SE, 4),
    total_p      = signif(total_pool$p, 4),
    # Proportion mediated (averaged; bounded ratio so no Rubin)
    prop_mediated = round(prop_mean, 4)
  )
}

results_df <- bind_rows(all_results)
elapsed_total <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
cat(sprintf("\nAll pathways complete | total elapsed %.1f min\n", elapsed_total))

cat("\n=== RUBIN-POOLED MEDIATION RESULTS ===\n")
print(as.data.frame(results_df), row.names = FALSE)

write_csv(results_df, file.path(tab_dir, "TableS17_Mediation_Rubin_Pooled.csv"))
saveRDS(results_df, file.path(out_dir, "mediation_rubin_pooled.rds"))

cat("\nSaved: TableS17_Mediation_Rubin_Pooled.csv\n")
cat("=== 04b_mediation_rubin_pooled.R COMPLETE ===\n")
