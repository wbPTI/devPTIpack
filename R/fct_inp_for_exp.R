#' inp_for_exp 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @export
fct_inp_for_exp <- function(dta) {
  new_names <-
    list(
      dta %>% get_indicators_list("fltr_exclude_pti"),
      dta %>% get_indicators_list("fltr_exclude_explorer")
    ) %>%
    map_dfr(~ {
      .x %>% select(var_code, var_name)
    }) %>%
    distinct()
  
  new_names <-
    setNames(object = new_names$var_name, new_names$var_code)
  
  dta %>%
    `[`(str_detect(names(.), "general|admin\\d_|point_")) %>%
    imap( ~ {
      if (str_detect(.y, "admin\\d_")) {
        # browser()
        .x <-
          .x %>%
          rename_at(vars(any_of(names(new_names))), function(x) {
            new_names[x]
          }) %>%
          select(-matches("admin\\dPcod"))  
      }
      .x
    }) %>% 
    map(~{.x %>% filter(if_any(everything(), ~ !is.na(.x)))}) %>% 
    keep(.p = function(x) nrow(x) > 0)
}



#' convert internal weights to the export format
#' 
#' @noRd
#' @export
fct_internal_wt_to_exp <- function(weights_clean, indicators_list) {
  weights_clean %>% 
    imap_dfr(~{
      .x %>% mutate(weight_scheme = .y)
    }) %>% 
    left_join(indicators_list %>% select(var_code, var_name), by = "var_code") %>% 
    select(contains("var_"), contains("weight"))
}



#' convert exported weights to internal
#'  
#' @noRd
#' @export
fct_exp_wt_to_internal <- function(dta) {
  dta
}