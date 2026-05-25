# Schizophrenia Task-Switching Project

Applied Statistics course project (Politecnico di Milano, 2021/2022).

## Team
- Erica Bistacchia
- Costanza Cantalini
- Lorenzo Ferrara
- Scott Pesenti

## Project Summary
This repository contains an analysis of task-switching performance in participants diagnosed with schizophrenia (SCHZ) versus controls (CTRL), with additional exploration of brain connectivity maps.

The study investigates whether behavioral differences in reaction time and task-switching are associated with connectivity-level differences across brain regions.

## Experiment in One Paragraph
Participants performed a task-switching paradigm during fMRI acquisition. On each trial, they responded to visual stimuli by selecting either color or shape, and on a subset of trials (~25%) the active rule switched. The analysis compares reaction-time behavior, accuracy, and connectivity-derived features between CTRL and SCHZ groups.

## Main Analysis Themes
- Behavioral analysis from event-level recordings (reaction time, switch costs, congruency effects)
- Group comparisons (ANOVA and mixed-effects style workflows)
- Connectivity-map exploration and PCA-based interpretation
- Exploratory links with covariates (for example smoking and Barratt scales)

## Repository Layout
- `analysis/`: normalized workspace for current scripts/notebooks
- `data/`: normalized data area (`raw`, `processed`, `external`)
- `results/`: figures, tables, and presentations
- `docs/`: project details
- `references/`: papers and reference material
- `archive/`: legacy assets kept for traceability

Current canonical locations:
- `analysis/scripts/`: R analysis scripts (legacy + final variants)
- `data/raw/events-recording/`: event-level per-subject recordings
- `data/raw/covariates/`: phenotype/covariate files
- `data/processed/workspaces/`: saved `.RData` workspaces used by scripts
- `results/figures/legacy-plots/`: generated visual outputs

## Notes on Reproducibility
This is a historical academic project and scripts were written iteratively. Some files still contain local absolute paths from the original development environment.

A practical cleanup path is:
1. Start from scripts in `analysis/scripts/recording/`, `analysis/scripts/ANOVA/`, and `analysis/scripts/z_map/`.
2. Replace hardcoded `setwd(...)` calls with project-relative paths.
3. Save outputs in dedicated folders instead of relying on interactive plotting only.

## Where to Start
See the scripts guide at `analysis/scripts/README.md` for a folder-by-folder overview and a suggested reading/execution order.
