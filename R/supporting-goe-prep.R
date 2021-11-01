#' Build a ggplots for all layers in a data file
#' 
#' @export
#' @noRd
plot_admin_level_ggmap <-
  function(plot_admin_dta,
           geometries,
           mtdt = NULL,
           plot_var = NULL,
           n_groups = 7) {
    plot_dta <- plot_admin_dta
    
    if (is.null(plot_var)) {
      # browser()
      plot_var <-
        plot_dta %>%
        select(any_of(names(.)[!str_detect(names(.), "^area|^admin")])) %>%
        select_if(is.numeric) %>%
        names() %>%
        `[[`(1)
    }
    
    admin_level <-
      plot_dta %>%
      names() %>%
      str_extract("^admin\\d") %>%
      sort() %>%
      max()
    
    if (is.null(mtdt))
      mtdt <- tibble(var_code = NA_character_, var_name = NA_character_)
    
    mtt <-
      geometries[str_detect(names(geometries), admin_level)][[1]]
    
    plot_dta %>%
      select(-any_of("area")) %>%
      pivot_longer(any_of(plot_var),
                   names_to = "var",
                   values_to = "val") %>%
      dplyr::right_join(mtt) %>%
      left_join(mtdt %>% dplyr::select(var_code, var_name),
                by = c("var" = "var_code")) %>%
      mutate(var = ifelse(!is.na(var_name), var_name, var),
             val = val) %>%
      filter(!is.na(var)) %>%
      dplyr::select(-var_name) %>%
      plot_single_ggmap(n_groups = n_groups)
    
  }



#' @describeIn plot_admin_level_ggmap Another plotter the key one for metadata
#' 
#' @export
#' @noRd
plot_pti_plotslist <- 
  function(plot_data, bounds, plot_index = seq_along(plot_data), n_groups = 7, vars_plot = NULL) {
    
    if (is.null(vars_plot)) {
      # browser()
      vars_plot <-
        plot_data[[1]] %>%
        names() %>%
        `[`(!str_detect(., "^area$|admin\\d"))
    }
    
    vars_plot %>%
      walk( ~ {
        var <- .x
        out <-
          plot_data[plot_index] %>%
          imap( ~ {
            # browser()
            dplyr::select(.x, contains("admin"), any_of(var)) %>%
              plot_admin_level_ggmap(geometries = bounds, n_groups = n_groups)# + 
            # labs(tag = .y)
          }) %>%
          ggarrange(plotlist = ., ncol = length(plot_index))
        print(out)
      })
  }

#' @describeIn plot_admin_level_ggmap assists plotting the ggmaps
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


#' @describeIn plot_admin_level_ggmap Extracts intersection betwen two polygons and computes the area of overlap
#' 
#' @export
#' @noRd
get_intercestions <- function(from_bound, to_bound) {
  from_bound %>%
    st_make_valid() %>%
    st_intersection(to_bound %>% st_make_valid())  %>%
    mutate(area_under = as.numeric(st_area(.) / 1000 / 1000)) %>%
    st_drop_geometry() %>%
    as_tibble()
}



#' @describeIn plot_admin_level_ggmap Build a ggplot of out of a dataframe with the geometries layer.
#' 
#' @export
#' @noRd
plot_single_ggmap <- function(dta, n_groups = 7) {
  legend <-
    devPTIpack::legend_map_satelite(dta$val, legend_paras = NULL, n_groups = n_groups)
  
  dta %>%
    mutate(val = legend$recode_function_intervals(val) %>%
             factor(levels = legend$our_labels)) %>%
    ggplot() +
    aes(fill = val,  geometry = geometry) +
    geom_sf(colour = NA) +
    theme_minimal() +
    theme(legend.position = c(0.9, 0.2)) +
    labs(title = dta$var) +
    scale_fill_manual(values = legend$pal(legend$our_values))
}

