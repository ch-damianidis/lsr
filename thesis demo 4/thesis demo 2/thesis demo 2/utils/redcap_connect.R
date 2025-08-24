# utils/redcap_connect.R
library(readxl)
get_data_from_redcap <- function() {
  readxl::read_excel("data/HR_data_pairs_interactions.xlsx")
}




