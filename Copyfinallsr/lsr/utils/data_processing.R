# utils/data_processing.R

library(dplyr)





# ------------------------------------------------------------------------------
# Convert arm-level data to pairwise (contrast) format for netmeta
#
# If the input already looks like pairwise (i.e., does not contain all arm-level
# columns), the function returns the data unchanged.
#
# Expected arm-level columns: study, treatment, mean, sd, n
# Output columns (pairwise):  study, treat1, treat2, logHR, selogHR
#
# Note: We keep column names 'logHR' and 'selogHR' for compatibility with the rest
# of the app, even if the summary measure is not strictly HR in your dataset.
# ------------------------------------------------------------------------------

convert_to_pairwise <- function(data, sm = "MD") {
  required_cols <- c("study", "treatment", "mean", "sd", "n")
  
  # If not arm-level, assume it's already pairwise and return as is.
  if (!all(required_cols %in% names(data))) {
    return(data)
  }
  
  pw <- netmeta::pairwise(
    treat   = treatment,
    mean    = mean,
    sd      = sd,
    n       = n,
    studlab = study,
    data    = data,
    sm      = "MD"   # adjust if needed ("MD", "SMD", etc.)
  )
  
  # Normalize to the column names expected elsewhere in the app
  out <- data.frame(
    study   = pw$studlab,
    treat1  = pw$treat1,
    treat2  = pw$treat2,
    logHR   = pw$TE,     # using 'logHR' name for consistency
    selogHR = pw$seTE,
    stringsAsFactors = FALSE
  )
  return(out)  
}

# ------------------------------------------------------------------------------
# Summarize basic dataset characteristics (works for arm- or pairwise-level)
# ------------------------------------------------------------------------------

summarize_data <- function(data) {
  n_missing <- sum(is.na(data))
  total <- if (is.null(dim(data))) 0 else prod(dim(data))
  percent_missing <- if (total > 0) round(100 * n_missing / total, 2) else NA_real_
  
  if (all(c("treat1","treat2") %in% names(data))) {
    # pairwise layout
    all_treatments <- unique(c(data$treat1, data$treat2))
    n_studies <- length(unique(data$study))
    n_rows    <- nrow(data)
  } else if ("treatment" %in% names(data)) {
    # arm-level layout
    all_treatments <- unique(data$treatment)
    n_studies <- length(unique(data$study))
    n_rows    <- nrow(data)
  } else {
    # unknown/minimal layout
    all_treatments <- character(0)
    n_studies <- length(unique(data$study))
    n_rows    <- nrow(data)
  }
  
  list(
    n_studies = n_studies,
    n_arms = n_rows,
    n_treatments = length(all_treatments),
    missing_percent = percent_missing
  )
}

 

  