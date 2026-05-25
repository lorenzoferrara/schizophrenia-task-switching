# Repository Restructure Map (Phase 1 + Materiale Migration)

This document tracks the first cleanup migration so old paths remain discoverable.

## Goals
- Improve readability of top-level structure.
- Separate active work areas from archived material.
- Keep historical content available.

## Completed Moves

- `1 - project details/` -> `docs/project-details-legacy/`
- `3 - output/Final Presentation/` -> `results/presentations/final-presentation/`
- `3 - output/MidTerm Presentation/` -> `results/presentations/midterm-presentation/`
- `3 - output/Outline of the project.pdf` -> `results/presentations/outline-of-the-project.pdf`
- `3 - output/Poster.pdf` -> `results/presentations/poster.pdf`
- `4 - references/` contents -> `references/`
- `2 - materiale/script/old/` -> `archive/legacy-scripts/`
- `2 - materiale/script/` contents -> `analysis/scripts/`
- `2 - materiale/covariates/` -> `data/raw/covariates/`
- `2 - materiale/data/` -> `data/raw/project-data/`
- `2 - materiale/events recording/` -> `data/raw/events-recording/`
- `2 - materiale/workspaces/` -> `data/processed/workspaces/`
- `2 - materiale/plots/` -> `results/figures/legacy-plots/`

## New Top-Level Structure
- `analysis/`
- `data/`
- `results/`
- `docs/`
- `references/`
- `archive/`

## Compatibility Layer
- `2 - materiale/` is now a compatibility path made of links to the new canonical locations.
- Existing old script references can still resolve while cleanup continues.

## Suggested Next Phase
1. Continue replacing hardcoded absolute paths in scripts with helper-based project-relative paths.
2. Add a lightweight runbook for key analyses (recording, ANOVA, z_map) under `docs/`.
3. Optionally remove compatibility links under `2 - materiale/` once path migration is complete.
