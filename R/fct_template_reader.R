

#' Fucntion for reading xlsx files with the templates
#'
#' @export
#' @import dplyr purrr stringr readxl
#' @noRd
fct_template_reader <- function(...) {
  flpth <- file.path(...)
  # browser()
  tmplt <-
    flpth %>%
    readxl::excel_sheets() %>%
    purrr::set_names(., .) %>%
    purrr::map( ~ {
      readxl::read_xlsx(path = flpth,
                        sheet = .x,
                        guess_max = 10000)
    })
  # browser()
  allowed_vars <- tmplt$metadata %>% distinct(var_code, spatial_level)
  tmplt <-
    tmplt %>%
    imap( ~ {
      if (str_detect(.y, "admin\\d")) {
        allowed_here <- 
          allowed_vars %>% 
          # filter(spatial_level %in% .y) %>% 
          pull(var_code)
        .x <-
          .x %>%
          dplyr::select(contains("agg_"), matches("admin\\d"),
                 any_of(c("area", "year", allowed_here)))
      }
      .x
    }) %>% 
    imap(~{
      if (str_detect(.y, "admin\\d")) {
        short_df <-
          .x %>%
          dplyr::select(-contains("agg_"), -matches("admin\\d"), -any_of(c("area", "year")))
        if (length(short_df) == 0)
          return(NULL)
        else {
          .x
        }
      }  else {
        .x
      }
    })  %>% 
    keep(~!is.null(.))
  
  # Converting tables weights_table to weights_clean
  if (!is.null(tmplt$weights_table ) & 
      nrow(tmplt$weights_table) > 0) {
    tmplt$weights_clean <-
      tmplt$weights_table %>% 
      fct_convert_weight_to_clean()
  } else {
    tmplt$weights_clean <- NULL 
  }
  
  tmplt
}

#' Fucntion for reading xlsx files with the templates
#'
#' @import dplyr purrr stringr readxl
#' @noRd
fct_convert_weight_to_clean <- function(dta) {
  dta %>%
    names() %>%
    magrittr::extract(str_detect(., "ws\\d\\.\\.")) %>%
    str_extract("ws\\d\\.\\.") %>%
    str_replace("\\.\\.", "") %>%
    unique()  %>%
    map(~ {
      wtbl <-
        dta %>%
        dplyr::select(var_code, contains(.x)) %>%
        rename_all(list(~ str_replace_all(., "ws\\d\\.\\.", "")))
      set_names(list(wtbl %>% dplyr::select(-name)), wtbl$name %>% unique())
    }) %>%
    unlist(recursive = F)
}


#' Fucntion for reading xlsx files with the templates
#'
#' @import dplyr purrr stringr readxl
#' @noRd
fct_convert_clean_to_weight <- function(dta) {
  dta %>%
    list(., names(.), seq_along(.)) %>%
    pmap( ~ {
      name_add <- str_c("ws", ..3, "..")
      ..1 %>%
        rename_at(vars(!(var_code)), list( ~ str_c(name_add, .))) %>%
        mutate(!!str_c("ws", ..3, "..", "name") := ..2) %>%
        dplyr::select(var_code, contains("name"), contains(name_add))
      
    }) %>%
    reduce(full_join, by = "var_code")
}

