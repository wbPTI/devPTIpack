#' Derive Mapping table from boundaries hyerarchy
#' 
#' @param country_shapes geometries of the country-specific administrative levels
#'
#' @noRd 
#'
#' @importFrom purrr map reduce
#' @importFrom dplyr select contains full_join all_vars filter_all
#' @importFrom stringr str_detect
#' 
#' @export
get_mt <- function(country_shapes) {
  
  country_shapes %>% 
    purrr::map(~{.x %>% sf::st_drop_geometry()}) %>% 
    # spatial_aggs() %>%
    purrr::reduce(function(x, y) {
      x %>%
        dplyr::select(dplyr::contains("admin")) %>%
        dplyr::select(-dplyr::contains("Name", ignore.case = F)) %>%
        dplyr::full_join(y %>% dplyr::select(dplyr::contains("admin")),
                         by = names(x) %>% `[`(., stringr::str_detect(., "Pcod"))) 
    }) %>% 
    dplyr::select(dplyr::contains("Pcod")) %>%
    dplyr::filter_all(dplyr::all_vars(!is.na(.)))
}

#' returns a vector with IDs of admin levels
#' @noRd 
#'
#' @importFrom stringr str_extract
#' 
#' @export
get_adm_levels <- 
  function(dta) {
    dta %>% 
      names() %>%
      str_extract("admin\\d{1,2}") %>% 
      # set_names(x = str_extract(., "\\d") %>% as.integer(), nm = .) %>%
      sort() %>% 
      set_names(.)
  }


#' pivot/clean PTI data for PTI calculations
#' 
#' @noRd 
#'
#' @importFrom purrr map reduce imap
#' @importFrom dplyr select contains full_join all_vars filter_all
#' @importFrom stringr str_detect
#' 
#' @export
pivot_pti_dta <- function(input_dta, indicators_list) {
  # var_codes <- input_dta$indicators_list$var_code %>% unique()
  var_codes <- indicators_list$var_code %>% unique()
  input_dta %>%
    `[`(names(.) %>% str_detect("admin")) %>%
    purrr::imap( ~ {
      # browser()
      .x %>%
        dplyr::select(
          dplyr::contains("agg_"), 
          dplyr::matches("admin\\d"),
          dplyr::any_of(c("area", "year", var_codes))) %>%
        tidyr::pivot_longer(
          cols = any_of(var_codes),
          names_to = "var_code",
          values_to = "value",
          values_drop_na = TRUE
        ) %>%
        dplyr::distinct()
    })
}


#' clean the shapes from geometries leaving only idetifiers
#' 
#' 
#' @noRd 
#'
#' @importFrom purrr map reduce imap
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_extract str_replace
#' 
#' @export
clean_geoms <- function(country_shapes) {
  existing_shapes <-
    country_shapes %>% 
    purrr::map(~{sf::st_drop_geometry(.x) %>% dplyr::as_tibble()})
  names(existing_shapes) <- 
    names(existing_shapes) %>% 
    stringr::str_extract("^(.*?)_") %>%
    stringr::str_replace("_", "")
  existing_shapes  
}



#' Weight pti data by weights. 
#' 
#' @noRd
#' @importFrom tidyr replace_na
#' @importFrom purrr map
#' @importFrom dplyr left_join mutate select
#' 
#' @export
get_weighted_data <- function(wt_list, vars_dta_list, indicators_list) {
  wt_list %>% 
    purrr::map(~{
      ws <- .x %>% 
        dplyr::mutate(weight = tidyr::replace_na(weight, 0)) 
      if (!all(ws$weight == 0)) {
        ws <- 
          ws %>%
          filter(weight != 0, !is.na(weight)) 
      }
      vars_dta_list %>% 
        purrr::map(~{
          .x %>% 
            dplyr::inner_join(ws, by = "var_code") %>% 
            dplyr::mutate(value = value * weight) %>%
            dplyr::select(-contains("weight"))
        })
    })
  
}




#' Computes PTI scores: key function to change computation methodology or upgrade.
#' 
#' @noRd
#' 
#' @export
#' 
#' @importFrom purrr map
#' @importFrom dplyr group_by_at mutate ungroup
get_scores_data <- function(wt_dta_list) {
  wt_dta_list %>%
    purrr::map( ~ {
      .x %>%
        purrr::map( ~ {
          if (nrow(.x) > 0) {
            .x <-
              .x %>%
              dplyr::group_by_at(vars(any_of(c("year", "var_code")))) %>%
              dplyr::mutate(value = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
              dplyr::mutate(value = ifelse(is.nan(value), 0, value)) %>%
              dplyr::ungroup()
          }
          .x
        })
    })
  
}


