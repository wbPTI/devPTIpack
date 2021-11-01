#' Server Functions
#'
#' @export
mod_drop_inval_adm <- function(id, dta, wt_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    adm_to_filter <- 
      eventReactive(#
        wt_dta()$indicators_list,
        {
          wt_dta()$indicators_list %>%
            get_vars_un_avbil(names(get_current_levels(dta()))) %>%
            get_min_admin_wght(wt_dta()$weights_clean)
        })
    
    observeEvent(
      adm_to_filter(),
      {
        
        do_notify <- 
          adm_to_filter() %>% 
          map_lgl(~length(.x) > 0) %>% 
          any()
        
        if (do_notify) {
          cur_lvls <- get_current_levels(dta())
          ws_dropped <-
            adm_to_filter() %>%
            keep(function(x) length(x) > 0) %>% 
            imap( ~ {
              mss <-
                cur_lvls[names(cur_lvls) %in% .x] %>%
                unname() %>%
                str_c(collapse = ", ")
              str_c(.y, " - ", mss, "; ")
            })
          
          shiny::showNotification(
            str_c(
              "Following administrative levels were ommited because no data are available there: ",
              ws_dropped
            ),
            id = "dropped-admin-levels",
            type = "default",
            duration = 10
          )
        }
        
      }, ignoreInit = F
    )
    
    eventReactive(#
      dta(),
      {
        if (isTruthy(adm_to_filter()) && isTruthy(dta()) && length(adm_to_filter()) > 0) {
          dta() %>% drop_inval_adm(adm_to_filter())
        } else {
          dta()
        }
      }, ignoreNULL = FALSE)
  })
}
    

#' @describeIn mod_drop_inval_adm derives administrative levels at which variables are not available and cant be extrapolated 
#' 
#' 
#' @importFrom tidyr unnest pivot_longer pivot_wider fill
#' @importFrom dplyr select group_by count ungroup arrange select filter
#' @export
get_vars_un_avbil <- function(ind_list, admin_levels = NULL) {
  
  ind_extend <- 
    ind_list %>%
    dplyr::select(var_code, admin_levels_years) %>%
    tidyr::unnest(cols = c(admin_levels_years)) %>%
    dplyr::group_by(var_code, admin_level) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>% 
    rename(value = n)
  
  if (is.null(admin_levels)) {
    admin_levels <- unique(ind_extend$admin_level) %>% sort()
  }
  
  expand.grid(
    var_code = unique(ind_extend$var_code), 
    admin_level = admin_levels) %>% 
    left_join(ind_extend, by = c("var_code", "admin_level")) %>%
    dplyr::arrange(var_code, admin_level) %>%
    dplyr::group_by(var_code) %>%
    mutate(any_larger = !is.na(lead(value)) | !is.na(lead(value, n = 2)) | !is.na(lead(value, n = 3))) %>% 
    mutate(value = ifelse(is.na(value & !any_larger), lag(value), value)) %>% 
    mutate(value = ifelse(is.na(value & !any_larger), lag(value), value)) %>% 
    mutate(value = ifelse(is.na(value & !any_larger), lag(value), value)) %>% 
    # tidyr::fill(value) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(is.na(value)) %>% 
    dplyr::select(-value)
}


#' @describeIn mod_drop_inval_adm Get list of admin levels that has to be excluded from the presentation
#' 
#' @importFrom purrr map
#' @importFrom dplyr pull inner_join filter
#' @export
get_min_admin_wght <- function(un_available_vars, wght_list) {
  wght_list %>% 
    map(~{
      inab_dta <- 
        .x %>% 
        dplyr::filter(weight != 0) %>% 
        dplyr::inner_join(un_available_vars, by = "var_code")
      
      if (nrow(inab_dta) > 0) {
        inab_dta <- 
          inab_dta %>% 
          dplyr::pull(admin_level) %>% 
          unique()
      } else {
        inab_dta <- NULL
      }
      inab_dta
    })
}

#' @describeIn mod_drop_inval_adm admin levels from the pre-plot data which should not be plotted.
#'  
#' @importFrom purrr map imap keep
#' @export
drop_inval_adm <- function(dta, adm_to_drom) {
  dta %>%
    imap(~{
      to_drop <- adm_to_drom[[.x$pti_codes]]
      if (!is.null(to_drop) && names(.x$admin_level) %in% to_drop) {
        return(NULL)
      } else {
        return(.x)
      }
    }) %>% 
    keep(function(x) !is.null(x))
}
