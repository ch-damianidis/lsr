# utils/data_processing.R

library(dplyr)





#' Convert arm-level data to pairwise format for NMA
#'
#' This function takes raw arm-level data (i.e., each row is a study arm)
#' and transforms it into a pairwise (contrast) dataset for NMA,
#' computing the log mean ratio and its standard error for each pair of arms within each study.
#' If the data is already in pairwise format (missing arm-level columns), it returns the data unchanged.
#'
#' @param data Data frame containing arm-level data with columns: study, treatment, mean, sd, n
#' @return Data frame with columns: study, treat1, treat2, logHR, selogHR




convert_to_pairwise <- function(data) {
  required_cols <- c("study", "treatment", "mean", "sd", "n")
  
  # If input data is NOT arm-level (does not have all required columns), return as is (assume already pairwise)
  if (!all(required_cols %in% colnames(data))) {
    return(data)
  }
  
  pairwise_list <- list()
  
  studies <- unique(data$study)
  
  # For each study, create all possible pairs of arms
  for (s in studies) {
    dat <- data %>% filter(study == s)
    
    if (nrow(dat) < 2) next  # Skip if only one arm
    
    combs <- combn(1:nrow(dat), 2)   # All unique arm pairs 
    
    for (i in 1:ncol(combs)) {
      idx1 <- combs[1, i]
      idx2 <- combs[2, i]
      
      arm1 <- dat[idx1, ]
      arm2 <- dat[idx2, ]
      
      # Compute log mean ratio (assumes means > 0)
      mean_diff <- log(arm1$mean / arm2$mean)
      
      # Standard error of log ratio, using Delta method (variance propagation)
      se_diff <- sqrt((arm1$sd^2 / (arm1$n * arm1$mean^2)) + (arm2$sd^2 / (arm2$n * arm2$mean^2)))
      
      # Store as a row in the list
      pairwise_list[[length(pairwise_list) + 1]] <- data.frame(
        study = s,
        treat1 = arm1$treatment,
        treat2 = arm2$treatment,
        logHR = mean_diff,
        selogHR = se_diff
      )
    }
  }
  # Combine all rows into a single data frame
  bind_rows(pairwise_list)
}


summarize_data <- function(data) {
  n_missing <- sum(is.na(data))
  total <- prod(dim(data))
  percent_missing <- round(100 * n_missing / total, 2)
  
  # Combine treat1 and treat2 to get all unique treatments present
  all_treatments <- unique(c(data$treat1, data$treat2))
  
  list(
    n_studies = length(unique(data$study)),
    n_arms = nrow(data),
    n_treatments = length(all_treatments),
    missing_percent = percent_missing
  )
}
 

  