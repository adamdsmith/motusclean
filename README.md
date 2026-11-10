# motusclean

Prepare raw Motus detections for automated QA and human run-level review. The package retains an audit trail for rejected runs and structural exclusions and can map or export vetted hits.

## Install

Install the Natural Earth datasets used by the mapping functions, then install this package. The GitHub account used for installation needs access while the repository is private.

```r
install.packages("remotes")
install.packages(c("rnaturalearthdata", "rnaturalearthhires"), repos = c(
  "https://ropensci.r-universe.dev", "https://cloud.r-project.org"
))
remotes::install_github("adamdsmith/motusclean")
```

For reproducibility, specify the commit SHA as `ref` when installing. See the [Motus R package](https://github.com/MotusWTS/motus) for obtaining a project database connection with `motus::tagme()`.

## Workflow

1. Run `prepare_motus_detections(db)` and inspect the QA summary. PASS runs met the automated criteria. REVIEW runs need a decision. Structural EXCLUDE identifies detections unsuitable for tag-level validation under the configured criteria.
2. If REVIEW runs exist, call `create_validation_package()` and complete one KEEP or REJECT decision per run in the review CSV.
3. Call `apply_validation()` with the completed CSV, or with `review = NULL` when there are no REVIEW runs. The vetted detections include automated PASS and manually kept REVIEW hits.
4. Optionally use `create_detection_maps()` for per-individual PDFs and `export_vetted_detections()` for a versioned hit-level RDS artifact.

Detection map connections show successive observations and do not estimate movement paths.
