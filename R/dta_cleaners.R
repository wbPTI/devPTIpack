#' get the list of indicators from metadata
#' 
#' @noRd
#' @export
#' 
#' @importFrom magrittr not
get_indicators_list <- function(dta, fltr_var = "fltr_exclude_pti") {
  meta_dta <-
    dta$metadata  %>%
    filter(magrittr::not(!!sym(fltr_var))) %>%
    select(-contains("fltr")) %>%
    arrange(pillar_group, var_order)
  
  element_names <- dta %>% names()
  
  pillars <-
    meta_dta %>%
    distinct_at(vars(contains("pillar"))) %>%
    arrange(pillar_group, pillar_name, pillar_description) %>%
    group_by(pillar_name) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(pillar_group)
  
  spatial_levels_names <-
    element_names %>%
    magrittr::extract(str_detect(., "\\D{1,}\\d{1}_")) %>%
    str_split("_") %>%
    transpose() %>%
    set_names(c("admin_level", "admin_level_name")) %>%
    map(~ unlist(.x)) %>%
    as_tibble()
  
  used_vars <- meta_dta$var_code %>% unique()
  
  levels_with_data <-
    spatial_levels_names$admin_level %>%
    map_lgl( ~ {
      rows <-
        dta %>%
        magrittr::extract(names(.) %>% str_detect(regex(.x, ignore_case = T))) %>%
        magrittr::extract2(1) %>%
        select(any_of(c("year", used_vars))) %>%
        length()
      rows != 0
    })
  
  if (any(!levels_with_data)) {
    spatial_levels_names[!levels_with_data, ]$admin_level %>%
      walk(~ {
        # browser()
        remove <-
          dta %>%
          names() %>%
          magrittr::extract(str_detect(., .x))
        dta[[remove]] <- NULL
      })
    
    spatial_levels_names <-
      spatial_levels_names[levels_with_data,]
    
  }
  vars_admins <-
    spatial_levels_names$admin_level %>%
    map( ~ {
      dta %>%
        magrittr::extract(names(.) %>% str_detect(regex(.x, ignore_case = T))) %>%
        magrittr::extract2(1) %>%
        select(any_of(c("year", used_vars))) %>%
        tidyr::pivot_longer(
          cols = any_of(used_vars),
          names_to = "var_code",
          values_to = "val"
        ) %>%
        filter(!is.na(val)) %>%
        distinct_at(vars(any_of(c(
          "year", "var_code"
        )))) %>%
        mutate(admin_level = .x)
      
    }) %>%
    bind_rows() %>%
    left_join(spatial_levels_names, by = "admin_level") %>%
    group_by_at(vars(any_of(
      c("var_code", "admin_level", "admin_level_name")
    ))) %>%
    tidyr::nest() %>%
    mutate(years = map(data, ~ {
      as.vector(.x[[1]]) %>% sort() %>% unlist()
    })) %>%
    select(-data) %>%
    group_by(var_code) %>%
    tidyr::nest() %>%
    rename(admin_levels_years = data)
  
  meta_dta %>%
    select(-contains("pillar_group"),
           -contains("pillar_descr"),
           -contains("spatial")) %>%
    semi_join(vars_admins, by = "var_code") %>%
    left_join(vars_admins, by = "var_code") %>%
    left_join(pillars, by = "pillar_name")
  
}

