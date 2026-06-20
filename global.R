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

# ==============================
# Project sources (helpers)
# ==============================
source("utils/netmeta_pipeline.R")
source("utils/redcap_connect.R")
source("utils/data_processing.R")
