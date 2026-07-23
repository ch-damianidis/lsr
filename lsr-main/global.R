# ==============================
# Libraries
# ==============================
library(shiny)
library(netmeta)
library(shinythemes)
library(DT)
library(igraph)
library(meta)
library(ggplot2)
library(dplyr)
library(readxl)

# ==============================
# Project sources (helpers)
# ==============================
source("utils/netmeta_pipeline.R")
source("utils/redcap_connect.R")
source("utils/data_processing.R")

# ==============================
# Shiny modules
# ==============================
source("modules/mod_data.R")
source("modules/mod_merge.R")
source("modules/mod_results.R")
source("modules/mod_export.R")
