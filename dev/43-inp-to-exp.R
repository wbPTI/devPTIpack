# Preparing data for export from the app.
library(tidyverse)
# library(devPTIpack)
devtools::load_all()
library(profvis)
library(DT)
library(shiny)


# shp_dta <- "../other_countries/south_sudan/South_Sudan.rds" %>% read_rds() #devPTIpack::ukr_shp
imp_dta <- 
  # "../other_countries/south_sudan/South_Sudan--metadata-2021-11-29_v2.1.xlsx" %>%
  # "../other_countries/somalia/mtdt-2021-11-08-00-53-06.xlsx" %>%
  "../other_countries/tzn/Tanzania--metadata-2022-01-28.xlsx" %>%
  devPTIpack::fct_template_reader()


fct_inp_for_exp(imp_dta)


