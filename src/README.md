# Scripts Guide

This folder contains most of the R analysis code for the project.

## Folder overview

- `ANOVA/`
  - Group-level ANOVA workflows on reaction-time derived measures.
  - Main files: `ANOVA.R`, `ANOVA_SCHZ.R`.

- `recording/`
  - Event-recording preprocessing and behavioral summaries.
  - Main files include reaction-time distributions, switch costs, correctness, and time-sequence analyses.

- `z_map/`
  - Connectivity map exploration and PCA interpretation scripts.
  - Includes both analytical scripts and some exported media artifacts.

- `barratt/`
  - Barratt scale exploration and impulsivity-related analyses.

- `fumo/`
  - Smoking-related preprocessing and reaction-time association scripts.

- Legacy scripts
  - Moved to `archive/legacy-scripts/` for clearer separation.
  - Treat that folder as archive/reference first, production workflow second.

- Top-level scripts
  - `accuracy.R`
  - `Linear Mixed Model Subject.R`
  - `interpreto_PC_2.R`

## Practical execution order (recommended)

1. `recording/` scripts for reaction-time derived variables.
2. `ANOVA/` or mixed-model scripts for group comparison.
3. `z_map/` scripts for connectivity and PCA interpretation.
4. `barratt/` and `fumo/` scripts for exploratory covariate analyses.

## Current caveats

- Several scripts contain hardcoded `setwd(...)` calls from old machines.
- Some scripts expect precomputed `.RData` files under `data/processed/workspaces/`.
- Plot generation is often interactive (`x11()`); batch-safe plotting is not always implemented.

## Light cleanup rules for future edits

- Prefer project-relative paths over absolute paths.
- Keep one analysis goal per script where possible.
- Add a short header to each script:
  - purpose
  - expected inputs
  - produced outputs
- Avoid editing files in `archive/legacy-scripts/` unless you are deliberately recovering legacy logic.
