# Reproducibility bundle

The individual-level LongROAD data are controlled-access and cannot be shared.
This folder lets other researchers reproduce the paper's **main results** from
shareable, de-identified artifacts only — no participant-level records.

It covers two complementary artifact types:

- **A. Summary statistics** — per-year covariance matrices, means/SDs, and
  baseline item-correlation matrices. These reproduce the regression-based
  results (bivariate associations, multivariate path models) and the
  reliabilities.
- **B. Per-subject prediction errors** — de-identified squared-error files (the
  `subject` id column removed). These reproduce the out-of-time prediction
  results, which a covariance matrix cannot (they need a permutation test on
  participant-level errors).

Nothing here contains predictors, identifiers, or any single participant's
responses: only covariance matrices and unlabeled rows of model error.

---

## What reproduces, and how closely

| Result (paper) | Artifact | Fidelity |
|---|---|---|
| Bivariate associations — **Table 4**, life-satisfaction text, **Table S5** | A | β within ~0.01 (median 0.003); **all directions and significance preserved** |
| Multivariate contrasts — **Table S4 / Figure 4** | A | coefficients within ~0.009 (median 0.003); **no sign changes**; 45/47 carried-forward contrasts reach the same replication decision |
| Internal reliability — Cronbach's α (.76–.90) | A | **exact** |
| Out-of-time prediction — **Figure 5** (ridge) and **Figure S1** (elastic net) | B | **exact** (ΔMSE identical to ~1e-16; identical set of significant outcomes) |

**Why A is close but not bit-exact.** The paper uses *analysis-specific* sample
sizes (each model uses the cases complete on its own variables; no global
listwise deletion). A single shared covariance matrix must fix one sample — here
the cases complete on all analysis variables (N = 2,448 in Year 1, 2,303 in
Year 2). Estimates therefore correspond to that complete-case subsample and
match the paper's per-model-N estimates to about two decimals, with every
inferential conclusion (sign, significance, replication) preserved. The
prediction results (B) are computed from the actual per-participant errors and
are exact.

---

## Contents

```
reproducibility/
├── REPRODUCIBILITY.md            this file
│
│  # ---- reproduction scripts (need only the shareable artifacts) ----
├── reproduce_from_summary.R      Table 4, Table S4, Table S5, Cronbach's alpha  (needs: lavaan)
├── reproduce_prediction.R        Figure 5 and Figure S1
│
│  # ---- artifact generators (run once by someone with data access) ----
├── build_summary_stats.R         builds summary_stats/ from the controlled data
├── prepare_prediction_errors.R   copies analysis/output/*_errors, dropping the id column
│
├── summary_stats/                                              (Artifact A)
│   ├── y1_covariance.csv, y2_covariance.csv        56×56 covariance matrices
│   │                                               (24 driving + 12 outcomes +
│   │                                                Age/Education/Income + 17
│   │                                                demographic dummy columns)
│   ├── y1_variable_summary.csv, y2_variable_summary.csv   mean, sd, N per variable
│   └── items_baseline_cor/<scale>.csv              baseline item correlation
│                                                   matrices (12 scales, for α)
│
├── prediction_errors/                                         (Artifact B)
│   ├── ridge/<outcome>_per_subject.csv    squared error of the 4 models per row
│   └── enet/<outcome>_per_subject.csv     (rows paired; no id, no predictors)
│
└── reproduced/                   outputs written by the reproduce_* scripts
```

---

## How to run

Reproduction needs only base R plus **lavaan** (for the path models). From this
folder:

```bash
Rscript reproduce_from_summary.R     # -> reproduced/repro_bivariate.csv,
                                     #    repro_contrasts_y2.csv, repro_tableS5_y1.csv,
                                     #    repro_alpha.csv
Rscript reproduce_prediction.R       # -> reproduced/repro_prediction_ridge.csv,
                                     #    repro_prediction_enet.csv
```

Both scripts read only from `summary_stats/` and `prediction_errors/`. They set
their own working directory and run under `Rscript` or RStudio.

### Regenerating the artifacts (data holders only)

With access to the controlled `data/` files and `analysis/output/`:

```bash
Rscript build_summary_stats.R        # rebuilds summary_stats/
Rscript prepare_prediction_errors.R  # rebuilds prediction_errors/ (drops ids)
```

---

## Method notes

- **Bivariate.** Each standardized multiple-regression coefficient (a driving
  variable predicting an outcome, controlling for the eight demographic
  covariates) is recovered in closed form from the correlation matrix
  (`cov2cor` of the covariance matrix); standard errors and t use
  (1 − R²)/(N − p − 1). Categorical covariates are the dummy columns already in
  the matrix.
- **Multivariate.** `lavaan` fits each path model directly from the covariance
  matrix (`sample.cov`, `sample.nobs`); continuous variables are rescaled to the
  analysis's z-scored metric using the shared SDs. The candidate-vs-other slope
  contrasts and their z-tests come from the fitted model; Year-2 one-tailed
  BY-FDR reproduces the replication decisions.
- **Reliability.** Standardized Cronbach's α = k·r̄ / (1 + (k−1)·r̄) from each
  baseline item correlation matrix.
- **Prediction.** For each outcome and comparison, ΔMSE is the mean paired
  squared-error difference; significance is a one-tailed sign-flip permutation
  test (10,000 permutations) with BY-FDR across the 12 outcomes — the paper's
  exact procedure.

## For fully exact, end-to-end reproduction

Bit-exact reproduction of the per-model-N estimates, or re-running the entire
pipeline (cross-validation, imputation, tuning), requires the individual-level
data. Apply to the AAA Foundation for Traffic Safety for LongROAD data access;
the full preprocessing and analysis code is in `preprocessing/` and `analysis/`.
