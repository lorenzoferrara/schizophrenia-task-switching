# Shared path helpers for legacy scripts.
#
# Usage from a script located in this folder or a subfolder:
# source("../project_paths.R")            # from one level below
# source("project_paths.R")               # from this same folder
#
# Then:
# data_root <- project_path("data")
# events_dir <- project_path("data", "raw", "events-recording")

project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    readme <- file.path(current, "README.md")
    data_dir <- file.path(current, "data")
    analysis_dir <- file.path(current, "analysis")
    src_dir <- file.path(current, "src")

    if (file.exists(readme) && dir.exists(data_dir) && (dir.exists(analysis_dir) || dir.exists(src_dir))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find project root (expected README.md, 'data', and either 'analysis' or 'src').")
    }
    current <- parent
  }
}

project_path <- function(...) {
  file.path(project_root(), ...)
}
