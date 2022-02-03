
#' Converts weighted data for all admin levels a plottable data structure
#' 
#' @param wghtd_dta output of applying weights
#' 
#' @export
#' @importFrom purrr map imap set_names
#' @importFrom dplyr contains select rename_with matches
#' @importFrom stringr str_replace
preplot_reshape_wghtd_dta <- function(wghtd_dta) {
  wghtd_dta %>%
    purrr::map(~ {
      dta <- .x
      dta$pti_codes %>%
        purrr::imap(~ {
          from_name <- str_c("..", .y, "$")
          from_match <- str_c(.y, "$")
          list(
            pti_dta =
              dta$pti_data %>%
              dplyr::select(
                dplyr::contains("admin"),
                dplyr::contains("area"),
                dplyr::contains("geometry"),
                dplyr::contains("spatial_name"),
                dplyr::matches(from_match)
              ) %>% 
              dplyr::rename_with(
                ~ stringr::str_replace(., from_name, ""), .cols = matches(from_match)
                ),
            pti_codes = purrr::set_names(.x, .y),
            admin_level = dta$admin_level
          )
        })
    }) %>%
    unlist(recursive = FALSE)
}


#' Extract unique admin levels values form the plotting list
#' 
#' @noRd
#' @export
#' @importFrom purrr map as_vector 
get_current_levels <- function(dta) {
  dta %>%
    purrr::map("admin_level") %>%
    unname() %>%
    purrr::as_vector() %>%
    `[`(!duplicated(.))
}


#' Extract unique admin levels values form the plotting list
#' 
#' @noRd
#' @export
#' @importFrom purrr map as_vector keep
#' @importFrom shiny isTruthy
filter_admin_levels <- function(dta, to_fltr = "all") {
  
  out <- NULL
  if (shiny::isTruthy(to_fltr)) {
    
    if (any(str_detect(to_fltr, regex("all", ignore_case = T)))) {
      out <- dta 
    } 
    
    if (any(to_fltr %in% get_current_levels(dta)) | 
        any(to_fltr %in% names(get_current_levels(dta)))) {
      out <- dta %>% keep(function(x) {x$admin_level %in% to_fltr})
    }
         
  }
  
  return(out)

}


#' Add legend parameters to each data bit that is plotted as a separate layer
#' 
#' @noRd
#' @export
#' @importFrom purrr map
add_legend_paras <- function(dta, nbins = 5) {
  
  if (isTruthy(dta)) {
    dta <- 
      dta %>%
      map( ~ {
        .x$leg <-
          legend_map_satelite(
            .x$pti_dta$pti_score,
            n_groups = nbins,
            legend_paras = .x$legend_paras
          )
        .x
      })
  }
  
  dta
}



#' Adding final element of the PTI labels
#' 
#' @export
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom dplyr mutate
complete_pti_labels <- function(dta) {
  
  if (isTruthy(dta)) {
    dta %>%
      purrr::map( ~ {
        .x$pti_dta <-
          .x$pti_dta %>%
          dplyr::mutate(
            pti_label = stringr::str_c(
              pti_label,
              "<strong>",
              .x$leg$recode_function(pti_score),
              "</strong>"
            )
          )
        .x
      })
  }
  
  dta
}



#' Plots PTI polygons on the map and returns a leaflet object or prints the map 
#' 
#' @param leaf_map proxy or leaflet map
#' @param poly_dta standardize-type data for plotting non overlapping polygons of PTI.
#' 
#' @export
#' 
#' @importFrom purrr reduce
#' @importFrom stringr str_c
#' @importFrom leaflet addPolygons highlightOptions
plot_pti_polygons <- function(leaf_map, poly_dta) {
  if(isTruthy(poly_dta)) {
  leaf_map %>% 
    list() %>% 
    append(poly_dta) %>% 
    purrr::reduce(function(x, y) {
      id_var <- stringr::str_c(names(y$admin_level), "Pcod")
      # browser()
      x %>%
        leaflet::addPolygons(
          data = y$pti_dta,
          fillColor = ~ y$leg$pal(y$pti_dta$pti_score),
          label = y$pti_dta$pti_label, # map(y$pti_dta$pti_label, ~ shiny::HTML(.)),
          options = pathOptions(pane = "polygons"),
          group = str_c(y$pti_codes, " (", y$admin_level, ")"),
          # layerId = str_c(y$pti_dta[[id_var]], " ", y$pti_codes, " (", y$admin_level, ")"),
          color = "white",
          weight = 1,
          opacity = 1,
          dashArray = "2",
          fillOpacity = 1,
          highlight = leaflet::highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            # fillOpacity = 0.95,
            bringToFront = TRUE
          )
        )
    })
  } else {
    leaf_map
  }
}


#' @describeIn plot_pti_polygons Plots PTI polygons on the map and returns a leaflet object
#' 
#' @export
#' @importFrom purrr reduce
#' @importFrom stringr str_c
#' @importFrom leaflet addPolygons highlightOptions
clean_pti_polygons <- function(leaf_map, poly_dta) {
  
  if (isTruthy(poly_dta)) {
    leaf_map <-
      leaf_map %>%
      list() %>%
      append(poly_dta) %>%
      purrr::reduce(function(x, y) {
        id_var <- stringr::str_c(names(y$admin_level), "Pcod")
        x %>%
          leaflet::clearGroup(group = str_c(y$pti_codes, " (", y$admin_level, ")"))
          # leaflet::removeShape(
          #   layerId =stringr::str_c(y$pti_dta[[id_var]], " ", 
          #                           y$pti_codes, " (", y$admin_level, ")"))
      })
  }
  
  leaf_map
}


#' @describeIn plot_pti_polygons plot_pti_polygons Add panel with controls over layers to select on the map. 
#' 
#' @export
#' @importFrom leaflet hideGroup removeLayersControl
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
add_pti_poly_controls <- function(leaf_map, poly_dta, old_grps = NULL) {
  
  grps <- poly_dta %>%
    purrr::map_chr(~{ stringr::str_c(.x$pti_codes, " (", .x$admin_level, ")")}) %>%
    unname()
  
  adm_level <- get_golem_options("default_adm_level")
  
  leaf_map <- 
    leaf_map %>% 
    leaflet::removeLayersControl() %>%
    leaflet::addLayersControl(
      baseGroups = grps,
      # overlayGroups = NULL,
      position = "bottomright",
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  if (!isTruthy(old_grps) && isTruthy(adm_level) ) {
    grps_in <- poly_dta %>% purrr::map("admin_level") %>%  unname() %>% as_vector()
    out_show <- 
      grps %>% 
      `[`(str_detect(grps_in, regex(adm_level, ignore_case = T))|
            str_detect(names(grps_in), regex(adm_level, ignore_case = T)))
    if (isTruthy(out_show)) {
      leaf_map <- 
        leaf_map %>%
        leaflet::hideGroup(grps) %>%
        leaflet::showGroup(out_show[[1]])
    }
  }
  
  if (isTruthy(old_grps)) {
    grps2 <- check_existing_groups(grps, old_grps[[length(old_grps)]], adm_level)
    leaf_map <- 
      leaf_map %>%
      leaflet::hideGroup(grps2$out_hide) %>%
      leaflet::showGroup(grps2$out_show[[1]])
  }
  
  leaf_map
  
}


#' @describeIn plot_pti_polygons Cleans controls for non-overlaying polygons in leaflet proxy or simple object
#' 
#' @export
#' @importFrom leaflet hideGroup removeLayersControl
#' @importFrom purrr map_chr
clean_pti_poly_controls <- function(leaf_map, poly_dta) {
  
  grps <- 
    poly_dta %>%
    purrr::map_chr(~{ str_c(.x$pti_codes, " (", .x$admin_level, ")")}) %>% 
    unname()
  
  leaf_map <- 
    leaf_map %>% 
    leaflet::hideGroup(grps) %>% 
    leaflet::removeLayersControl() 
  
  leaf_map
}


#' @describeIn plot_pti_polygons Check if the new groups contain previously selected group and returns groups that are to be plotted
#' 
#' @export
#' @importFrom stringr str_replace str_trim str_c str_detect
check_existing_groups <- function(cur_grps, old_grps, priority_group) {
  check_this_too <-
    old_grps %>% 
    stringr::str_replace(" \\s*\\([^\\)]+\\)", "") %>% 
    stringr::str_trim() %>% 
    stringr::str_c(., " \\(")
  
  grps_in <- cur_grps %>% `[`((.) %in% old_grps)
  
  if (length(grps_in) == 0) {
    grps_in <- cur_grps %>% `[`( str_detect(., check_this_too)) 
  }
  
  if (length(grps_in) >= 2) {
    grps_in <- grps_in[[1]]
  }
  
  grps_out <- cur_grps %>% `[`(! ((.) %in% grps_in))
  
  out_hide <- NULL
  out_show <- NULL
  
  if (length(grps_in) > 0) {
    out_show <- grps_in
    out_hide <- grps_out 
  }
  
  if (length(grps_in) == 0 & length(grps_out) > 0) {
    # browser()
    out_show <- grps_out[[1]]
    out_hide <- grps_out[!grps_out%in%out_show]
  }
  
  list(out_show = out_show, out_hide = out_hide)
}