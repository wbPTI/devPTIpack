

# Functions
#' @noRd
agg_ward_stats <-
  function(ward_stats, bounds, sum_var) {
    bounds %>%
      imap(~ {
        base_tbl <-
          .x %>%
          st_drop_geometry() %>%
          as_tibble()
        
        group_var <- str_extract(.y, "^admin\\d")
        
        base_tbl %>%
          left_join(ward_stats %>%
                      group_by_at(vars(contains(group_var))) %>%
                      summarise(across(any_of(sum_var), sum, na.rm = TRUE)))
      })
  }



# Wrapping a function for such operations
#' @noRd
extrct_by_geo <- function(rast_img,
                          zam_bound,
                          var_name,
                          geo_level = "admin4_Wards",
                          fun_call = function(...) sum(...)) {
  
  # fun = 
  #   switch (fun, "sum" = action
  #   )
  fb_pop_wards <-
    zam_bound[[geo_level]] %>%
    mutate(nn = row_number()) %>%
    group_by(nn) %>%
    nest() %>%
    pull(data) %>%
    {
      dta <- .
      pbb <- progress::progress_bar$new(format = ":current / :total|:elapsedfull [:bar] :percent (:eta)",
                                        total = length(dta))
      dta %>%
        map_dfr(~ {
          out <- try({
            crpp <- raster::crop(x = rast_img,
                                 y =  st_cast(st_as_sf(pull(.x, geometry)), "POLYGON"),
                                 snap = "out"
                                 )
            rast_sum4 <- terra::extract(crpp, 
                                        st_cast(st_as_sf(pull(.x, geometry)), "POLYGON"), 
                                        fun = fun_call,
                                        na.rm = TRUE)
            .x %>%
              mutate({{var_name}} := rast_sum4[[1]], success = TRUE)
          })
          
          if ("try-error" %in% class(out)) {
            out <- try({
              cat("Fixing error \n")
              rast_sum4 <- terra::extract(rast_img, 
                                          st_cast(st_as_sf(pull(.x, geometry)), "POLYGON"),
                                          fun = fun_call, 
                                          na.rm = TRUE)
              
              .x %>% mutate({{var_name}} := rast_sum4[[1]], success = TRUE)
            })
            
            if ("try-error" %in% class(out)) {
              out <- .x %>%
                mutate({{var_name}} := NA_real_, success = FALSE)
            }
          }
          pbb$tick()
          out
        })
    } %>%
    st_drop_geometry() %>%
    as_tibble()
  
  # failed_rows <- nrow(filter(fb_pop_wards, !success))
  
  # if (failed_rows > 0) {
  #   cat(
  #     "Some statistics failed for ",
  #     failed_rows,
  #     " rows.",
  #     "\n It will be recombited from the full raster image."
  #   )
  # }
  # 
  # fb_pop_wards_extra <-
  #   zam_bound[[geo_level]] %>%
  #   filter(admin4Pcod %in% (fb_pop_wards %>% filter(!success) %>% pull(admin4Pcod))) %>%
  #   mutate(nn = row_number()) %>%
  #   group_by(nn) %>%
  #   nest() %>%
  #   pull(data) %>%
  #   {
  #     dta <- .
  #     pbb <-
  #       progress::progress_bar$new(format = ":current / :total|:elapsedfull [:bar] :percent (:eta)",
  #                                  total = length(dta))
  #     dta %>%
  #       map_dfr( ~ {
  #         out <- try({
  #           rast_sum4 <- terra::extract(rast_img, .x, fun = sum, na.rm = TRUE)
  #           .x %>% mutate(fb_pop = rast_sum4[[1]], success = TRUE)
  #         })
  #         
  #         if ("try-error" %in% class(out)) {
  #           out <- .x %>% mutate(fb_pop = NA_real_, success = FALSE)
  #         }
  #         pbb$tick()
  #         out
  #       })
  #   } %>%
  #   st_drop_geometry() %>%
  #   as_tibble()
  
  
  clean_fb_pop <-
    fb_pop_wards %>%
    # filter(success) %>%
    # bind_rows(fb_pop_wards_extra) %>%
    agg_ward_stats(bounds = zam_bound, var_name)%>% 
    map2(zam_bound, ~{.y %>% left_join(.x %>% dplyr::select(-area))})
  
  clean_fb_pop
}





#' @noRd
gg_admin_list <- function(dta,
                          multiply = 1,
                          mt = zam_bounds_simple,
                          metadata = NULL) {
  spat_agg <- 
    dta %>% 
    names() %>% 
    magrittr::extract(str_detect(., "admin\\d")) %>% 
    str_extract( "admin\\d") %>% 
    `[[`(1)
  
  mtt <- mt[str_detect(names(mt), spat_agg)][[1]]
  # browser()
  if (is.null(metadata)) {
    metadata <- tibble(var_code = NA_character_, var_name = NA_character_)
  }
  # browser()
  plot_list <-
    dta %>%
    pivot_longer(any_of(names(.)[!str_detect(names(.), "admin\\d")]),
                 names_to = "var", values_to = "val") %>%
    dplyr::right_join(mtt) %>%
    left_join(metadata %>% select(var_code, var_name), by = c("var" = "var_code")) %>% 
    mutate(
      var = ifelse(!is.na(var_name), var_name, var),
      val = val ) %>%
    filter(!is.na(var)) %>% 
    select(-var_name) %>% 
    group_by(var) %>%
    nest() %>%
    rowwise() %>%
    pmap(~ {
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


# Extracts intersection betwen two polygons and computes the area of overlap
#' @noRd
get_intercestions <- function(from_bound, to_bound) {
  from_bound %>%
    st_make_valid() %>%
    st_intersection(to_bound %>% st_make_valid())  %>%
    mutate(area_under = as.numeric(st_area(.) / 1000 / 1000)) %>%
    st_drop_geometry() %>%
    as_tibble()
}


#' Build a ggplot of out of a dataframe with the geometries layer.
#'
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

#' Build a ggplots for all layers in a data file
#' @noRd
plot_admin_level_ggmap <-
  function(plot_admin_dta, geometries, mtdt = NULL, plot_var = NULL, n_groups = 7) {
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
    
    if (is.null(mtdt)) mtdt <- tibble(var_code = NA_character_, var_name = NA_character_)
    
    mtt <- geometries[str_detect(names(geometries), admin_level)][[1]]
    
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
      plot_single_ggmap(n_groups=n_groups)
    
  }
