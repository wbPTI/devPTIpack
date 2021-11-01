#' calc_pti UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_calc_pti_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' calc_pti Server Function
#'
#' @noRd
mod_calc_pti_server <- function(input,
                                output,
                                session,
                                globr,
                                shapes_list,
                                edited_list,
                                pti_raw_list,
                                pti_clean_list,
                                value_transform =
                                  function(x) {
                                    if (length(x) == 1) {
                                      return(x)
                                    } else {
                                      (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
                                    }
                                  },
                                extrapolate_agg = function(x) {
                                  if (length(x) == 1) {
                                    return(x)
                                  } else {
                                    mean(x, na.rm = TRUE)
                                  }
                                }
                                
) {
  
  
  ns <- session$ns
  
  
  # Long-formated data from tables.
  long_vars <- reactiveVal()
  
  observeEvent(#
    globr[[edited_list]] %>% magrittr::extract(names(.) %>% str_detect("admin"))
    ,
    {
      req(globr[[edited_list]])
      # browser()
      possible_cols <- globr[[edited_list]]$metadata$var_code
      long_vars(globr[[edited_list]] %>%
                  magrittr::extract(names(.) %>%
                                      str_detect("admin")) %>%
                  imap( ~ {
                    # browser()
                    .x %>%
                      select(contains("agg_"), matches("admin\\d"), 
                             any_of(c("area", "year", possible_cols))) %>% 
                      tidyr::pivot_longer(
                        cols = any_of(possible_cols),
                        names_to = "var_code",
                        values_to = "value",
                        values_drop_na = TRUE
                      ) %>% 
                      # group_by(var_code) %>% 
                      # filter_at(vars(any_of("year")), all_vars(. = max(., na.rm = TRUE)))
                      distinct()
                  }))
    })
  
  # Preparing simplified spatial mapping tables
  spatial_aggs <- reactiveVal()
  
  shapes_country <- 
    reactive({
      req(globr[[edited_list]])
      req(globr[[shapes_list]])
    })
  
  observeEvent(#
    shapes_country()
    ,
    {
      req(globr[[shapes_list]])
      req(globr[[edited_list]]$general$country)
      # browser()
      # if (!globr[[edited_list]]$general$country %in% names(globr[[shapes_list]])) {
      #   shinyWidgets::show_alert(
      #     title =
      #       str_c(
      #         "Country (",
      #         globr[[edited_list]]$general$country ,
      #         ") specified in the data does not exist in the list or pre-defined countries."
      #       ),
      #     text = str_c(
      #       "Pre-defined countries are: ",
      #       str_c(names(globr[[shapes_list]]), collapse = ", "),
      #       ". If this error occurs due to a typo in the country name, modify it. ",
      #       "If the country, which you are looking for is missing, contact the support team."
      #     ), 
      #     type = "error"
      #   )
      # }
      # browser()
      existing_shapes <-
        globr[[shapes_list]] %>% 
        map(~{sf::st_drop_geometry(.x) %>% as_tibble()})
      names(existing_shapes) <- 
        names(existing_shapes) %>% 
        str_extract("^(.*?)_") %>% str_replace("_", "")
      spatial_aggs(existing_shapes)
    })
  
  # Step 1 Transf calc. weighted vars ====================================
  weighted_data <-
    eventReactive(#
      globr[[edited_list]]$weights_clean,
      {
        if(!isTruthy(globr[[edited_list]]$weights_clean)) return(NULL)
        req(globr[[edited_list]]$weights_clean)
        req(globr[[edited_list]]$indicators_list)
        req(long_vars())
        
        # 1.1 Calc based on weights and transformation =======================
        
        out_dta <- 
          globr[[edited_list]]$weights_clean %>%
          map( ~ {
            join_vars <-
              names(.x)[names(.x) %in% c("agg_level", "year", "var_code")]
            
            # browser()
            if (all(.x$weight == 0, na.rm = T)) {
              weight_scheme <- .x 
            } else {
              weight_scheme <- .x %>% filter(weight != 0)
            }
            
            weight_scheme <-
              weight_scheme %>% 
              semi_join(globr[[edited_list]]$indicators_list, by = join_vars) %>% 
              left_join(globr[[edited_list]]$indicators_list, by = join_vars) %>%
              mutate(var_name = ifelse(is.na(var_name), var_code, var_name)) %>%
              select(-contains("pillar")) %>% 
              tidyr::unnest(cols =  "admin_levels_years") %>% 
              select(-contains("year"), spatial_level = admin_level)
            # browser()
            # weight_scheme %>%
            weight_scheme %>% 
              pull(spatial_level) %>%
              unique() %>%
              set_names(.) %>%
              map( ~ {
                .x_sl <- .x
                one_spatial_level <-
                  weight_scheme %>%
                  filter(spatial_level == .x) %>%
                  # rename_at(vars(contains("year")), list(~ "year")) %>%
                  rename_at(vars(contains("spatial_level")), list( ~ "agg_level"))
                # browser()
                long_vars() %>%
                  magrittr::extract(str_detect(names(.), .x)) %>%
                  .[[1]] %>%
                  semi_join(one_spatial_level %>% select(any_of(c(join_vars, "weight"))),
                            by = join_vars) %>%
                  left_join(one_spatial_level %>% select(var_code:weight),
                            by = join_vars) %>%
                  group_by_at(vars(any_of(join_vars))) %>%
                  mutate_at(vars(value), list( ~ value_transform(value) * weight)) %>%
                  ungroup() %>%
                  select(-contains("weight"))
              })
            
          })
        
        # 1.2 Polygon-specific text  ======================================
        # browser()
        out_dta
        
      }, ignoreNULL = FALSE)
  
  # observe({
  #   weighted_data()
  #   browser()
  # })
  
  # Step 2 up and down extrapolation ======================================
  # extrapolated_raw_data <- 
  observeEvent(#
    weighted_data(),
    {
      if(!isTruthy(weighted_data())) globr[[pti_raw_list]] <- NULL
      req(weighted_data())
      
      # browser()
      
      map_tbl <-
        spatial_aggs() %>%
        reduce(function(x, y) {
          x %>%
            select(contains("admin")) %>%
            full_join(y %>%
                        select(contains("admin")),
                      by = names(x) %>%
                        magrittr::extract(., str_detect(., "Pcod"))) 
        }) %>% 
        select(contains("Pcod")) %>%
        filter_all(all_vars(!is.na(.)))
      
      # Step 2.2 Extrapolation  all weighting schemes ==========================
      # browser()
      all_ws_extrapolated <-
        weighted_data() %>%
        map(~{
          # browser()
          # if (!"admin4" %in% names(.x)) {
          #   # browser()
          #   .x[["admin4"]] <-
          #     spatial_aggs()[str_detect(names(spatial_aggs()), "admin4")][[1]] %>%
          #     select(-contains("admin0"), -contains("admin1"), -contains("admin2"), -contains("admin3")) %>%
          #     mutate(year = NA, var_code = "dummy_var", value = 0)
          # }
          
          # if (!"admin2" %in% names(.x)) {
          #   # browser()
          #   .x[["admin2"]] <-
          #     spatial_aggs()[str_detect(names(spatial_aggs()), "admin3")][[1]] %>%
          #     select(-contains("admin0"), -contains("admin1"), -contains("admin3")) %>%
          #     mutate(year = NA, var_code = "dummy_var", value = 0)
          # }
          extrapo_one_weight(.x, map_tbl, extrap_fun = extrapolate_agg)})
      globr[[pti_raw_list]] <- all_ws_extrapolated
      
      showNotification("PTIs are calculated and extrapolated", 
                       type = "default",
                       duration = 5)
      
    }, ignoreNULL = FALSE)
  
  
  
  # Step 3. Clean PTIs =================================
  observeEvent(#
    globr[[pti_raw_list]],
    {
      if (!isTruthy(globr[[pti_raw_list]])) {
        # browser()
        globr[[pti_clean_list]]  <- NULL
      }
      req(globr[[pti_raw_list]])
      req(globr[[edited_list]]$general$country)
      
      shapes <- globr[[shapes_list]]
      # browser()
      plt_dta_semi_raw <-
        globr[[pti_raw_list]] %>%
        imap(~ {
          # browser()
          pti_name <- .y
          .x %>%
            imap(~ {
              # browser()
              map_name <-
                spatial_aggs()[[ .y]] %>% 
                select(contains(.y)) %>% 
                rename_at(vars(matches("Name")), list(~"spatial_name"))
              nonsum_cols <-
                select(.x, matches("^admin\\d"), matches("^year$")) %>%
                names()
              .x %>%
                select(any_of(nonsum_cols)) %>%
                mutate(
                  pti_score = .x %>%
                    select(!any_of(nonsum_cols)) %>%
                    rowSums(na.rm = TRUE),
                  pti_name = pti_name
                ) %>% 
                left_join(map_name, by = nonsum_cols) %>% 
                as_tibble()
            })
        }) %>%
        transpose() %>%
        imap( ~ bind_rows(.x))
      
      # Polygo labels ================================================
      # Here is a placeholdes for developing data labels
      # browser()
      globr[[pti_clean_list]] <-
        plt_dta_semi_raw %>%
        imap( ~ {
          pti_codes <-
            .x$pti_name %>%
            unique() %>%
            set_names(nm = str_c("pti_ind_", seq_along(.)))
          # browser()
          pti_data <-
            .x %>%
            mutate(
              pti_label =
                glue::glue(
                  "<strong>{spatial_name}</strong>",
                  "<br/>Weighting scheme: <strong>{ifelse(is.na(pti_name), 'No data', pti_name)}</strong>",
                  "<br/>PTI score: <strong>{ifelse(is.na(pti_score), 'No data', scales::label_number(accuracy = 0.00001)(pti_score))}</strong>",
                  "<br/>"
                  # "<br/>Administrative level: {admin0Pcod}"
                )
            ) %>%
            # mutate(pti_label =  purrr::map(pti_label, ~ {
            #   if(is.na(.x) | is.null(.x)) NA else htmltools::HTML(.x)
            # })) %>%
            mutate(pti_code = NA_character_) %>%
            list() %>%
            append(as.list(pti_codes)) %>%
            reduce2(names(pti_codes), function(x, y, z) {
              x %>%
                mutate(pti_code = ifelse(pti_name == y, z, pti_code))
            }) %>%
            select(-pti_name) %>%
            tidyr::pivot_wider(
              names_from = "pti_code",
              values_from = c("pti_score", "pti_label"),
              names_sep = ".."
            )
          name_var <-
            shapes %>%
            magrittr::extract(str_detect(names(.), .y)) %>%
            magrittr::extract2(1) %>%
            names() %>% 
            magrittr::extract(str_detect(., "admin\\dName"))
          name_var <- sym(name_var)
          pti_data <-
            shapes %>%
            magrittr::extract(str_detect(names(.), .y)) %>%
            magrittr::extract2(1) %>%
            left_join(pti_data, by = names(.x) %>% magrittr::extract(str_detect(., "admin\\d"))) %>% 
            mutate_at(vars(contains("pti_label")),
                      # list(~ purrr::map(., ~ if(is.null(.x)) htmltools::HTML("No data") else htmltools::HTML(.x))))
                      list(~ purrr::map(., ~ if(is.null(.x)) ("No data") else (.x)))) %>%
            mutate(spatial_name = ifelse(is.na(spatial_name), !!name_var, spatial_name)) %>% 
            mutate_at(
              vars(contains("pti_label")), 
              list( ~ ifelse(is.na(.),
                             glue::glue(
                               "<strong>{spatial_name}</strong>",
                               "<br/>PTI score: <strong>No data</strong>",
                               "<br/>"
                               # "<br/>Administrative level: {admin0Pcod}"
                             ), .))
            )
          
          admin_level <-
            globr[[edited_list]] %>% names() %>% magrittr::extract(str_detect(., .y))
          
          set_names(
            str_extract(admin_level, "^(.*?)_") %>% str_replace("_", ""),
            str_extract(admin_level, "_(.*?)$") %>% str_replace("_", "")
          )
          
          list(
            #
            pti_data = pti_data,
            pti_codes = pti_codes,
            admin_level =  set_names(
              str_extract(admin_level, "^(.*?)_") %>% str_replace("_", ""),
              str_extract(admin_level, "_(.*?)$") %>% str_replace("_", "")
            )
          )
          
        })
      
    }, ignoreNULL = FALSE)
  
}

## To be copied in the UI
# mod_calc_pti_ui("calc_pti_ui_1")

## To be copied in the server
# callModule(mod_calc_pti_server, "calc_pti_ui_1")


#' @export
#' @noRd
extrapo_one_weight <-
  function(one_wscheme, map_tbl, extrap_fun) {
    # one_wscheme <- .x
    spatial_levels  <-
      names(one_wscheme) %>%
      set_names(x = str_extract(., "\\d") %>% as.integer(), nm = .) %>%
      sort()
    
    extrapol_data <-
      spatial_levels %>%
      imap(~ {
        # browser()
        sl_from <- spatial_levels[spatial_levels == .x]
        
        sl_from_data_generic <-
          one_wscheme[[.y]] %>%
          select(-contains("agg"), -contains('name'), -any_of("area")) #%>%
        # select_if(function(x) !all(is.na(x))) 
        
        to_rename <- sl_from_data_generic %>% pull(var_code) %>% unique()
        
        sl_to_data <- 
          sl_from_data_generic %>%
          # mutate(var_code = str_c(var_code, "._._.", names(sl_from))) %>%
          tidyr::pivot_wider(names_from = "var_code", values_from = "value")
        
        sl_from_data <- 
          sl_to_data %>% 
          rename_at(vars(any_of(to_rename)), list(~str_c(., "._._.", names(sl_from))))
        
        # browser()
        spatial_levels %>%
          imap( ~ {
            # browser()
            sl_to <-  spatial_levels[spatial_levels == .x]
            # sl_to_data <-
            #   one_wscheme[[.y]] %>%
            #   select(-contains("agg"), -contains('name'), -any_of("area")) %>%
            #   select_if(function(x)
            #     ! all(is.na(x))) %>%
            #   tidyr::pivot_wider(names_from = "var_code", values_from = "value")
            
            local_mt <-
              map_tbl %>%
              select(contains(names(sl_from)), contains(names(sl_to)))
            
            col_to <-
              map_tbl %>% select(contains(names(sl_to))) %>% names()
            
            col_from <-
              map_tbl %>% select(contains(names(sl_from))) %>% names()
            
            # s2.1 Upwards scaling -------------------------------------------------
            if (sl_from < sl_to) {
              # browser()
              out <-
                # full_join(sl_to_data %>% left_join(local_mt, by = col_to),
                #           sl_from_data,
                #           by = c(col_from, extracol)) %>%
                local_mt %>%
                distinct() %>%
                full_join(sl_from_data, by = c(col_from)) %>%
                filter_all(all_vars(!is.na(.))) %>% 
                select(contains(names(sl_to)),  everything()) %>%
                select(-contains(str_c(names(sl_from), "Pcod")))
            }
            
            # s2.2 No sacling - same data =========================================
            
            if (sl_from == sl_to){
              out <- sl_to_data
            }
            
            # s2.3 Downscaling from disaggregated to aggregated levels -------------
            if (sl_from > sl_to) {
              out <-
                # full_join(sl_to_data %>% left_join(local_mt, by = col_to),
                #           sl_from_data,
                #           by = col_from)
                
                local_mt %>%
                distinct() %>%
                full_join(sl_from_data, by = col_from) %>%
                select(contains(names(sl_to)), everything()) %>%
                select(-contains(str_c(names(sl_from), "Pcod"))) %>%
                group_by_at(vars(contains(names(sl_to)))) %>%
                summarise_all(list(~ extrap_fun(.)))
            }
            out
          })
      })
    
    # browser()
    # extrapol_data <-
    extrapol_data %>%
      transpose() %>%
      imap(~ {
        # browser()
        by_join <- str_c(.y, "Pcod")
        .x %>%
          reduce(function(x, y) {
            
            # if ("year" %in% names(x) &
            #     "year" %in% names(y)) {
            #   extracol <- "year"
            #   remove <-
            #     names(x) %>%
            #     magrittr::extract(!(str_detect(., by_join) |
            #                           str_detect(., "year")))
            #   out_1 <-
            #     x %>%
            #     left_join(y %>%
            #                 select(-any_of(remove)) , c(by_join, extracol))
            # } else {
            extracol <- NULL
            remove <-
              names(x) %>%
              magrittr::extract(!str_detect(., by_join))
            
            out_1 <-
              x %>%
              left_join(y %>%
                          select(-any_of(remove)) , c(by_join))
            # }
            out_1
          })
      })
    
  }
