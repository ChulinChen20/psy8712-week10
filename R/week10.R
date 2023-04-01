# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import and Cleaning
# download and convert JSON data to an R object and extract the table from it
gss_tbl <- read_sav("../data/GSS2016.sav")

# visualization
gss_tbl  %>%
  #  All missing, don’t know, inapplicable, not answered has been marked as NAs already
  #  filter NAs in the number of working hours last week column
  filter(is.na(HRS1) != T) %>%
  rename(workhours = HRS1) %>%
  # select columns with <0.75 NAs 
  select(where(~mean(is.na(.)) < 0.75)) %>%
  ggplot(aes(workhours)) +
  geom_freqpoly()



  
