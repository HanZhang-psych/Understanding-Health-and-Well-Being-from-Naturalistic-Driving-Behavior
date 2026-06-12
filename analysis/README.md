# Year-stratified discovery → replication pipeline

This folder is a self-contained re-implementation of the bivariate, multivariate, and predictive analyses that respects the longitudinal structure of the LongROAD data. It does **not** modify or delete anything in the original `analysis/` folder; the originals remain reproducible.

## What changed and why

The original pipeline aggregated every subject's monthly driving data across all years (≈ 29 months on average) and merged it with the average of every available annual health visit. Reviewers raised three connected issues:

1. (R1, point 3) The 24 × 12 = 288 bivariate tests + several multivariate and predictive tests are exploratory and risk capitalizing on chance.
2. (R1, point 9) In the path models, the candidate slope is selected as the strongest data-driven effect and then compared to weaker ones, which is circular.
3. (R1, point 10) Averaging across ~29 months discards within-person variation; a longitudinal approach would be more compelling.
4. (R1, point 7) Listwise deletion of 332 participants was not motivated and may not be MCAR/MAR.

The new pipeline converts the single-pool analysis into a **pre-specified discovery → temporal replication design**, and uses analysis-specific N (no listwise deletion at the file level):

* **Year 1 (discovery).** Subject-level aggregates built from monthly driving in study Interval 0, paired prospectively with health outcomes at the Interval 1 visit. All 288 bivariate tests are run here, candidate slopes for the path models are identified here, and within-Y1 nested CV is run here. All of these are exploratory.
* **Year 2 (confirmation).** Subject-level aggregates built from monthly driving in study Interval 1, paired prospectively with health outcomes at the Interval 2 visit. *Only* the (IV, DV) pairs that survived in Y1 are tested in Y2, one-tailed in the Y1-implied direction, Bonferroni-corrected over the much smaller surviving family. Path-model candidate slopes are *not* re-chosen in Y2 — they are frozen from Y1, and the candidate-vs-others contrasts are tested for the first time in Y2. The Ridge regression model is trained on Y1 and evaluated on Y2.

This is *temporal* / *within-person* replication, not an independent-sample replication. It is stronger than the original single-pass analysis because (a) the test data are from non-overlapping driving months, (b) outcomes are re-measured at a later visit, and (c) the confirmatory family is much smaller and direction is pre-specified.

## Missing-data policy (no listwise deletion at the file level)

`aggregate_data_yearly.R` keeps all N = 2,990 subjects per year. Missing values are handled inside each analysis:

| Analysis | Handling |
|---|---|
| Bivariate (R `lm`) | Row-wise complete-case per fit (the lm default). Each row of the output CSV carries its own `n_obs`. |
| Path model (lavaan) | Listwise per model (`missing = "listwise"`, `fixed.x = TRUE`, the lavaan default). Each path model uses complete cases on the IV, all 12 DVs, and demographic covariates. Per-model `n_obs` is saved alongside each contrast. |
| Nested CV (sklearn) | `SimpleImputer` (mean for numeric, most-frequent for categorical) fit on the training fold only, applied to test. Subjects with missing outcome are dropped (squared error is undefined without a true value). |

The two inferential analyses (bivariate and multivariate) therefore use a consistent rule: complete cases on the model variables, no FIML, no imputation. The predictive analysis uses within-fold imputation to give every subject a prediction (standard ML practice).

## Path-model design (proposal 2)

* Any IV with **≥ 1** significant Y1 bivariate DV enters the path model.
* Each path model includes **all 12 outcomes**, not only the significant ones.
* The candidate DV is identified from the Y1 *significant* bivariate set (largest |β|; or the single significant DV if only one). Contrasts compare the candidate against each of the **other 11 DVs**, including non-significant ones. This answers: is the significant slope reliably larger than the non-significant ones, or are they all about the same?

## How each reviewer concern is addressed

| Concern | Addressed by |
|---|---|
| R1 #3: exploratory testing, alpha inflation | Pre-specified two-step design. Y2 family is much smaller than 288 and tests are directional. The exploratory/confirmatory boundary is now explicit. |
| R1 #7: listwise deletion | `aggregate_data_yearly.R` no longer drops rows. Each analysis uses its own appropriate missing-data approach (see table above). |
| R1 #8: variance-corrected CV inference, guessing baseline | `nestedCV_yearly.ipynb` trains on Y1 and evaluates on Y2, yielding non-overlapping training and test data. Each Y2 prediction also has a permutation test on the paired error difference, and a mean-of-training-outcome baseline. |
| R1 #9: circularity in candidate slope | The candidate slope is selected in Y1 (`y1_pathmodel_spec.csv`, locked in), and the slope-difference contrasts are tested in Y2 only (`multivariate_y2_confirmation.R`). The Y2 test is therefore free of the selection-on-magnitude bias. |
| R1 #10: longitudinal use of data | The prospective pairing (Y1 driving → Interval 1 health; Y2 driving → Interval 2 health) means each model's outcome comes from a visit *after* the driving period. This is a lagged design within each year. |

## Files

| File | Purpose |
|---|---|
| `funcs.R` | Shared variables and helpers; `fit_lm`, `fit_pathmodel_oneIV`, `make_one_tailed_p`, `add_dummies_for_pathmodel` |
| `aggregate_data_yearly.R` | Builds `../data/y1_subject_level.csv` and `../data/y2_subject_level.csv` from the monthly driving and per-Interval health files |
| `bivariate_y1_discovery.R` | All 288 Y1 bivariate tests; Bonferroni; saves Y1 significant pairs |
| `bivariate_y2_confirmation.R` | Pre-specified one-tailed tests on the Y1-significant pairs in Y2; replication flag |
| `multivariate_y1_discovery.R` | Identifies candidate DV per IV (only IVs with ≥ 2 sig Y1 DVs); fits Y1 path models |
| `multivariate_y2_confirmation.R` | Refits the same path models on Y2; tests Y2 contrasts one-tailed in the Y1 direction |
| `nestedCV_yearly.ipynb` | Within-Y1 nested CV (descriptive) + Y1→Y2 out-of-time generalization with full/reduced/baseline (inferential) |

## Order to run

```text
1.  Rscript aggregate_data_yearly.R         (or run from RStudio)
2.  Rscript bivariate_y1_discovery.R
3.  Rscript bivariate_y2_confirmation.R
4.  Rscript multivariate_y1_discovery.R
5.  Rscript multivariate_y2_confirmation.R
6.  Run nestedCV_yearly.ipynb cells in order
```

Steps 2 → 3 → 4 → 5 have a strict dependency on the previous step's output. Step 6 only depends on step 1.

## Outputs

All R outputs land in this folder unless otherwise specified.

| File | Contents |
|---|---|
| `../data/y1_subject_level.csv` | Y1 subject-level data set |
| `../data/y2_subject_level.csv` | Y2 subject-level data set |
| `../data/yearly_sample_log.txt` | Sample-size log for the two years |
| `y1_all_bivariate.csv` | Y1 bivariate estimates for all 288 pairs |
| `y1_significant_bivariate.csv` | Y1 Bonferroni-significant pairs |
| `y2_confirmation_bivariate.csv` | Y2 one-tailed Bonferroni tests on Y1 survivors, with replication flag |
| `y2_all_bivariate.csv` | Y2 two-tailed estimates for all 288 pairs (reference) |
| `y1_pathmodel_spec.csv` | Per-IV candidate DV and other DVs (frozen Y1 specification) |
| `y1_pathmodel_paths.csv` | Y1 regression paths from the path models |
| `y1_pathmodel_contrasts.csv` | Y1 candidate-vs-others contrasts (descriptive) |
| `y2_pathmodel_paths.csv` | Y2 regression paths |
| `y2_pathmodel_contrasts.csv` | Y2 candidate-vs-others contrasts with Y1 spec and replication flag |
| `cv_errors_yearly/` | Per-subject CV / Y1→Y2 squared errors for all 12 DVs and the summary CSV |

## What this pipeline does NOT change

* The 24-driving-variable / 12-outcome set is unchanged.
* The demographic covariate set and within-year standardization conventions are unchanged.
* Outlier flagging is inherited from `cleaned_monthly_driving_data.csv`, which already used `by = c('Site', 'Interval')`, so it is already per-year-stratified.
* The Ridge regression specification (alpha grid, KFold settings) is unchanged from the original notebook.

## Honest scope limits

* Y1 and Y2 are from the *same people*. This is temporal (within-person) replication, not an independent sample. We should describe it as such in the manuscript.
* Subjects' Y2 data may be missing where they skipped the 24-month visit or had no Interval 1 driving. The Y2 sample is therefore a subset of the Y1 sample on average. This is reported in `../data/yearly_sample_log.txt`.
* The path-model sample is smaller than any of its constituent bivariate samples because listwise on a 12-DV model is stricter than listwise on a 1-DV model. Per-model N is saved in the output CSVs so the drop is visible.
* Listwise can in principle introduce bias if missingness is MAR rather than MCAR. A sensitivity check using FIML or multiple imputation on the Y1 bivariates is a sensible supplement; both arguments are still available on `fit_pathmodel_oneIV()` if needed.
* The pipeline does not address sample-generalizability (R1 #7 - the "85.9% White" point), car-type / rurality confounding (R1 #6), or Ridge-vs-Elastic-Net (R1 minor #1). These are independent revisions.
