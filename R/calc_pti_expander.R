#' extrapolates/expands PTI scores or original data numbers to other 
#'   administrative levels
#' 
#' @noRd
#' @importFrom purrr imap
#' @importFrom dplyr select contains all_of distinct pull rename_at vars any_of everything all_vars full_join filter_at group_by_at summarise_all
#' @importFrom stringr str_detect str_extract str_c
#' @importFrom tidyr pivot_wider
#' 
expand_adm_levels <- function(wtd_scrd_dta, mt) {
  
  adm_lvls <- mt %>% get_adm_levels() %>% set_names(.)
  
  purrr::imap(adm_lvls, ~ {
    adm_from <- .x
    adm_from_dta_full <-
      wtd_scrd_dta[stringr::str_detect(names(wtd_scrd_dta), adm_from)]
    
    if (length(adm_from_dta_full) == 1 &&
        nrow(adm_from_dta_full[[1]]) > 0) {
      adm_from_dta_full <-
        adm_from_dta_full[[1]] %>%
        dplyr::select(dplyr::contains(adm_from), dplyr::all_of(c("year", "var_code", "value"))) %>%
        dplyr::select(- dplyr::contains("Name"), -contains("year"))# %>%
      # dplyr::select_if(function(x) ! all(is.na(x)))
      
      to_rename <-
        adm_from_dta_full %>% dplyr::distinct(var_code) %>% dplyr::pull()
      
      adm_to_dta <- 
        adm_from_dta_full %>%
        # mutate(var_code = str_c(var_code, "._._.", adm_from)) %>%
        tidyr::pivot_wider(names_from = "var_code", values_from = "value")
      
      adm_from_dta <- 
        adm_to_dta %>% 
        dplyr::rename_at(dplyr::vars(dplyr::any_of(to_rename)), 
                         list(~ str_c(., "._._.", adm_from)))
      adm_lvls %>%
        purrr::imap( ~ {
          adm_to <-  .x
          # browser()
          
          local_mt <-
            mt %>% 
            dplyr::select(dplyr::contains(adm_from), dplyr::contains(adm_to)) %>%
            dplyr::distinct()
          col_to <- mt %>% names() %>% `[`(stringr::str_detect(., adm_to))
          col_from <- mt %>% names() %>% `[`(stringr::str_detect(., adm_from))
          sl_to <- stringr::str_extract(col_to, "\\d") %>% as.integer()
          sl_from <- stringr::str_extract(col_from, "\\d") %>% as.integer()
          
          # s2.1 Upwards scaling -------------------------------------------------
          if (sl_from < sl_to) {
            out <-
              local_mt %>%
              dplyr::distinct() %>%
              dplyr::full_join(adm_from_dta, by = col_from) %>%
              dplyr::select(dplyr::contains(adm_to), dplyr::everything()) %>%
              dplyr::select(-dplyr::contains(stringr::str_c(adm_from, "Pcod"))) %>%
              dplyr::filter_at(dplyr::vars(dplyr::contains(adm_to)), dplyr::all_vars(!is.na(.)))
          }
          
          # s2.2 No sacaling - same data =========================================
          if (sl_from == sl_to) {
            out <- adm_to_dta
          }
          
          # s2.3 Down scaling from dis aggregated to aggregated levels -------------
          if (sl_from > sl_to) {
            out <-
              local_mt %>%
              dplyr::distinct() %>%
              dplyr::full_join(adm_from_dta, by = col_from) %>%
              dplyr::select(dplyr::contains(adm_to), dplyr::everything()) %>%
              dplyr::select(-dplyr::contains(str_c(adm_from, "Pcod"))) %>%
              dplyr::filter_at(dplyr::vars(dplyr::contains(adm_to)), dplyr::all_vars(!is.na(.))) %>%
              dplyr::group_by_at(dplyr::vars(dplyr::contains(adm_to))) %>%
              dplyr::summarise_all(list(~ mean(., na.rm = TRUE)))
          }
          out
        })
    } else {
      adm_lvls %>% imap( ~ {NULL})
    }
    
  })
  
}


#' Join expanded admin levels
#' 
#' @noRd
#' 
merge_expandedn_adm_levels <- function(dta) {
  dta %>%
    transpose() %>%
    imap( ~ {
      by_join <- str_c(.y, "Pcod")
      
      .x %>%
        `[`(map_lgl(., ~ !is.null(.x))) %>%
        reduce(function(x, y) {
          remove <-
            names(x) %>%
            magrittr::extract(!str_detect(., by_join))
          x %>%
            left_join(y %>% select(-any_of(remove)), by_join)
        }) %>%
        select_if(function(x) !all(is.na(x))) 
    })
}

#' Aggregate PTI scores
#' 
#' @noRd
#' @importFrom purrr imap transpose
#' @importFrom tidyr separate
#' @import dplyr
#' 
agg_pti_scores <- function(extrap_dta, adm_ids, na_rm_pti2 = NULL) {
  
  na_rm_pti <- get_golem_options("na_rm_pti")
  if (is.null(na_rm_pti)) { na_rm_pti <- FALSE }
  if (!is.null(na_rm_pti2)) { na_rm_pti <- na_rm_pti2}
  
  extrap_dta  %>%
    purrr::imap( ~ {
      pti_name <- .y
      .x %>%
        purrr::imap( ~ {
          
          dta <- .x #%>% select_if(function(x) !all(is.na(x))) 
          
          map_name <-
            adm_ids[[.y]] %>%
            dplyr::select(dplyr::contains(.y)) %>%
            dplyr::rename_at(dplyr::vars(dplyr::matches("Name")), list( ~ "spatial_name"))
          
          nonsum_cols <-
            dplyr::select(dta, dplyr::matches("^admin\\d"), dplyr::matches("^year$")) %>%
            names()
          
          # making sure that we do not sum twice columns that already exist at this level of aggregation
          naitive_col <- names(dta) %>% 
            `[`(!(.) %in% nonsum_cols) %>% 
            `[`(!str_detect(., "\\.\\_\\.\\_\\.admin\\d"))
          
          foreign_coll_1 <-
            names(dta) %>%
            `[`(!(.) %in% nonsum_cols) %>%
            `[`(str_detect(., "\\.\\_\\.\\_\\.admin\\d")) %>%
            tibble(for_col = .) %>%
            tidyr::separate(
              for_col,
              into = c("col", "level"),
              sep = "\\.\\_\\.\\_\\.",
              remove = F
            ) %>% 
            filter(!col %in% naitive_col)
          
          if (nrow(foreign_coll_1) > 0) {
            foreign_coll <- 
              foreign_coll_1 %>% 
              group_by(col) %>% 
              filter(level == max(level, na.rm = T)) %>% 
              ungroup() %>% 
              pull(for_col)
          } else {
            foreign_coll <- character(0)
          }
          
          dta %>%
            dplyr::select(any_of(nonsum_cols)) %>%
            dplyr::mutate(
              pti_score = dta %>%
                dplyr::select(!any_of(nonsum_cols)) %>%
                dplyr::select(any_of(foreign_coll), naitive_col) %>%
                rowSums(na.rm = na_rm_pti),
              pti_name = pti_name
            ) %>%
            dplyr::left_join(map_name, by = nonsum_cols) %>%
            dplyr::as_tibble() %>%
            dplyr::filter_at(dplyr::vars(dplyr::contains(nonsum_cols)), dplyr::all_vars(!is.na(.)))
        })
    }) %>%
    purrr::transpose() %>%
    purrr::imap(~ dplyr::bind_rows(.x))
  
}

#' Label PTI observations in a generic way
#'
label_generic_pti <- function(dta, glue_expr = generic_pti_glue()) {
  dta  %>%
    map(~ {
      .x %>%
        mutate(pti_label =
                 glue::glue(glue_expr))
    })
  
}

#' @describeIn label_generic_pti glue string for labeling observations
#' 
#' 
generic_pti_glue <- function() {
  c(
    "<strong>{spatial_name}</strong>",
    "<br/>Weighting scheme: <strong>{ifelse(is.na(pti_name), 'No data', pti_name)}</strong>",
    "<br/>PTI score: <strong>{ifelse(is.na(pti_score), 'No data', scales::label_number(accuracy = 0.00001)(pti_score))}</strong>",
    "<br/>"
  ) %>%
    str_c(., collapse = "")
}



#' Restructure PTI data in the way acceptable for the mapping part of the app
#' 
#' @noRd
#' 
structure_pti_data <- function(dta, shp_dta) {
  
  dta %>%
    purrr::imap(~ {
      name_var <-  str_c(.y, "Name")
      name_var <- sym(name_var)
      pti_codes <-
        .x$pti_name %>%
        unique() %>%
        set_names(nm = str_c("pti_ind_", seq_along(.)))
      pti_codes2 <- tibble(pti_code = names(pti_codes), pti_name = pti_codes)
      
      .x  %>% left_join(pti_codes2, by = "pti_name")
      
      # pti_data <-
      #   .x %>%
      #   mutate(pti_code = NA_character_) %>%
      #   list() %>%
      #   append(as.list(pti_codes)) %>%
      #   reduce2(names(pti_codes), function(x, y, z) {
      #     x %>%
      #       mutate(pti_code = ifelse(pti_name == y, z, pti_code))
      #   }) %>%
      #   select(-pti_name) %>%
      #   tidyr::pivot_wider(
      #     names_from = "pti_code",
      #     values_from = c("pti_score", "pti_label"),
      #     names_sep = ".."
      #   )
      
      pti_data1 <- 
        .x  %>% 
        left_join(pti_codes2, by = "pti_name") %>%
        select(-pti_name) 
      
      var_match <- pti_data1 %>% select(matches("admin\\dPcod")) %>% names()
      var_match2 <- sym(var_match)
      var_to_expand <- c(var_match, "pti_code")
      var_to_distinct <- c(var_match, "spatial_name")
      
      pti_data2 <- 
        pti_data1 %>% 
        tidyr::expand(!!var_match2, pti_code) %>%
        left_join(distinct(select(pti_data1, all_of(var_to_distinct))), by = var_match) %>% 
        left_join(pti_data1, by = c(var_match, "spatial_name", "pti_code")) %>% 
        # filter(is.na(pti_label)) %>%
        mutate(
          pti_label = 
            ifelse(
              is.na(pti_label),
              str_c("<strong>",spatial_name, "</strong><br/>PTI score: <strong>No data</strong><br/>"),
              pti_label
              ) %>% 
            map(~htmltools::HTML(.x))
        ) %>% 
        mutate(pti_label = map(pti_label, ~htmltools::HTML(.x))) %>% 
        tidyr::pivot_wider(
          names_from = "pti_code",
          values_from = c("pti_score", "pti_label"),
          names_sep = ".."
        )
      
      pti_data0 <- 
        shp_dta %>%
        magrittr::extract(str_detect(names(.), .y)) %>%
        magrittr::extract2(1) %>% 
        left_join(pti_data2, by = names(.x) %>% magrittr::extract(str_detect(., "admin\\d")))
    
      
      # browser()
      # pti_data <-
      #   shp_dta %>%
      #   magrittr::extract(str_detect(names(.), .y)) %>%
      #   magrittr::extract2(1) %>%
      #   left_join(pti_data2, by = names(.x) %>% magrittr::extract(str_detect(., "admin\\d"))) %>%
      #   # mutate_at(vars(contains("pti_label")),
      #   #           # list(~ purrr::map(., ~ if(is.null(.x)) htmltools::HTML("No data") else htmltools::HTML(.x))))
      #   #           
      #   #           list( ~ifelse(is.null(.), "No data", .))
      #   #           ) %>%
      #   #           # list( ~ purrr::map(., ~ if (is.null(.x))
      #   #           #   ("No data")
      #   #           #   else
      #   #           #     (.x)
      #   #           # ))) %>%
      #   mutate(spatial_name = ifelse(is.na(spatial_name),!!name_var, spatial_name)) %>%
      #   mutate(
      #     across(#
      #       contains("pti_label"), #
      #       ~ purrr::map(., ~ {
      #         ifelse(
      #           is.na(.),
      #           glue::glue(
      #             "<strong>{spatial_name}</strong>",
      #             "<br/>PTI score: <strong>No data</strong>",
      #             "<br/>"
      #             # "<br/>Administrative level: {admin0Pcod}"
      #           ),
      #           .
      #         ) %>% htmltools::HTML(.)
      #       })
      #       )
      #     )
          
      admin_level <- shp_dta %>% names() %>% `[`(str_detect(., .y))
      admin_level <-
        set_names(
          str_extract(admin_level, "_(.*?)$") %>% str_replace("_", ""),
          str_extract(admin_level, "^(.*?)_") %>% str_replace("_", "")
        )
      
      list(pti_data = pti_data0,
           pti_codes = pti_codes,
           admin_level =  admin_level)
      
    })
  
}