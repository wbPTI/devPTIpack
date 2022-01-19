# rethinking the weights page logic
library(tidyverse)
# library(devPTIpack)
devtools::load_all()
library(profvis)
library(DT)
library(shiny)


shp_dta <- "../other_countries/south_sudan/South_Sudan.rds" %>% read_rds() #devPTIpack::ukr_shps
imp_dta <- "../other_countries/south_sudan/South_Sudan--metadata-2021-11-29_v2.1.xlsx" %>%
  devPTIpack::fct_template_reader()


