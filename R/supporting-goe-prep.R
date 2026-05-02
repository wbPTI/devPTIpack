#' Build a list of ggplot maps from PTI admin data
#'
#' @export
#' @noRd
gg_admin_list <- function(dta,
                          multiply = 1,
                          mt = zam_bounds_simple,
                          metadata = NULL) {
  spat_agg <-
    dta %>%
    names() %>%
    magrittr::extract(str_detect(., "admin\\d")) %>%
    str_extract("admin\\d") %>%
    `[[`(1)
  
  mtt <- mt[str_detect(names(mt), spat_agg)][[1]]
  # browser()
  if (is.null(metadata)) {
    metadata <-
      tibble(var_code = NA_character_, var_name = NA_character_)
  }
  # browser()
  plot_list <-
    dta %>%
    pivot_longer(any_of(names(.)[!str_detect(names(.), "admin\\d")]),
                 names_to = "var", values_to = "val") %>%
    dplyr::right_join(mtt) %>%
    left_join(metadata %>% select(var_code, var_name),
              by = c("var" = "var_code")) %>%
    mutate(var = ifelse(!is.na(var_name), var_name, var),
           val = val) %>%
    filter(!is.na(var)) %>%
    select(-var_name) %>%
    group_by(var) %>%
    nest() %>%
    rowwise() %>%
    pmap( ~ {
      # browser()
      ddta <- rlang::dots_list(...)
      ddta$data %>%
        ggplot() +
        aes(fill = val,  geometry = geometry) +
        geom_sf(colour = NA) +
        # scale_fill_viridis_b(
        #   option = "D",
        #   direction = -1,
        #   begin = 0.4,
        #   breaks = scales::breaks_pretty(12))+
        theme_minimal() +
        theme(legend.position = c(0.9, 0.2)) +
        labs(title = ddta$var)
    })
  plot_list
  
}


