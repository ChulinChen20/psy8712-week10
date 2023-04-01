# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import and Cleaning
# download and convert JSON data to an R object and extract the table from it
gss_tbl <- read_sav("../data/GSS2016.sav")

gss_tbl %>%
  filter(is.na(HRS1) !=T ) %>%
  rename(workhours = HRS1) %>%
  rowwise() %>%
  filter(sum(is.na(c_across(cols=MAR1:COGRADTOUNDER)))/961<0.75)
  
