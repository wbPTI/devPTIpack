test_that("`fct_template_reader()` loads all metadata examples in `tests/tastthat/mtdts-to-test`", {
  
  # library(tidyverse)
  # library(here)
  
  # mtdt_files <-
  #   here::here("tests", "testthat", "mtdts-to-test") %>%
  #   list.files(pattern = "xlsx", full.names = T)
  # 
  # # expect_gt(length(mtdt_files), 0)
  # 
  # expect_tmplate_read <- function(file) {
  #   tmplt <- fct_template_reader(file)
  # 
  #   expect_gt(length(tmplt),
  #             0,
  #             label = str_c(basename(file), ": is incomlete or failed to read"))
  # 
  #   expect_true(#
  #     {
  #       !is.null(tmplt$metadata)
  #     },
  #     label = str_c(basename(file), ": metadata sheet is missing"))
  # 
  #   expect_gt({
  #     tmplt %>%
  #       names() %>%
  #       `[`(str_detect(., "admin\\d")) %>%
  #       length()
  #   }, 0,
  #   label = str_c(basename(file), ": no sheets with data (admin1_...)"))
  # 
  # }
  # 
  # mtdt_files %>% walk(expect_tmplate_read)
})




