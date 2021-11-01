




#' legnd data generator for maps
#'
#' @description Prepare specific popu-up ggplot
#'
#' @param dta
#'
#' @noRd
#' @importFrom scales breaks_extended label_percent label_number
#' @importFrom leaflet colorBin 
#' @importFrom stringr str_c
#' 
#' @export
legend_map_satelite <- 
  function(leg_vals, 
           n_groups = 5, 
           no_data_label = "No data",
           legend_paras, 
           val_pal = c( "RdYlBu", "PRGn",  "BrBG", "RdGy", "RdBu"), 
           val_pal_id = 1,
           is_percent = FALSE, 
           is_categorical = FALSE,
           min_buble = 3,
           max_buble = 9) {
    
    # Global variables
    pal_to_use <- 
      val_pal %>% 
      rep(10) %>% 
      magrittr::extract2(val_pal_id)
    
    x_uni <- unique(leg_vals)
    x_uni_nona <- x_uni[!is.na(x_uni)]
    
    # Weather or not revert colors in pals
    revert_colours <- TRUE
    if (length(legend_paras) > 0) {
      if (!is.null(legend_paras$legend_revert_colours) && 
          isTRUE(legend_paras$legend_revert_colours)) {
        revert_colours <- FALSE
      }
    }
    
    # If not categorical ====================================================
    if (!is_categorical) {
      
      ## Step 1. Numeric. Define breaks number and values ===================
      if(n_groups < 1) n_groups <- 1
      n_breaks <- min(length(x_uni), n_groups)
      
      ## Step 1.2 quantiles for breaks number ==============================
      our_breaks <-
        quantile(leg_vals, seq(0, 1, 1 / (n_breaks)), na.rm = TRUE) %>%
        unique()
      
      ## Step 1.3 quantiles with increased breaks number ====================
      # If this does not work, we will do another round creating breaks based on the unique values only.
      if (length(our_breaks) <= n_breaks & length(our_breaks) != 1) {
        try({
          n_breaks <- n_breaks + 1
          our_breaks <-
            quantile(leg_vals, seq(0, 1, 1 / (n_breaks)), na.rm = TRUE) %>% 
            unique()
        }, silent = TRUE
        )
      }
      
      ## Step 1.3 If does not work, quantiles on unique values =============
      if (length(our_breaks) <= n_breaks & length(our_breaks) != 1) {
        try({
          n_breaks <- n_breaks + 1
          our_breaks <-
            quantile(x_uni, seq(0, 1, 1 / (n_breaks)), na.rm = TRUE) %>% 
            unique()
        }, silent = TRUE
        )
      }
      
      ## Step 2. When there are more than one unique value in data (including NA) =============
      if (length(x_uni) > 1) {
        
        ## Step 2.1 When unique values are only 0, 1, NA or up to 12 integers ===================
        if ((length(x_uni) %in% c(3) & 0 %in% x_uni & 1 %in% x_uni & NA %in% x_uni) | 
            (length(x_uni) %in% c(2) & ((0 %in% x_uni & NA %in% x_uni) | 
                                        (1 %in% x_uni & NA %in% x_uni) |
                                        (1 %in% x_uni & 0 %in% x_uni)))| 
            (length(x_uni_nona) < 12 & all(floor(x_uni_nona) == x_uni_nona))
        ) {
          
          our_labels <- unique(leg_vals) %>% magrittr::extract(!is.na(.)) %>% sort() 
          our_values <- our_labels
          pal <- colorFactor(pal_to_use, levels = our_values, 
                             reverse = revert_colours)
          
        } else {
          ## Step 2.1 When unique values are continuous ===========================
          lab_function <- scales::label_number(0.01, big.mark = " ")
          
          if (abs(max(x_uni, na.rm = T) / 10) > 1 & abs(max(x_uni, na.rm = T) / 100) < 1) {
            lab_function <- scales::label_number(0.1, big.mark = " ")
          } 
          
          if (abs(max(x_uni, na.rm = T) / 100) > 1 ) {
            lab_function <- scales::label_number(1, big.mark = " ")
          } 
          
          if (length(unique(lab_function(our_breaks))) < length(our_breaks)) {
            lab_function <- scales::label_number(0.01, big.mark = " ")
          }
          
          if (length(unique(lab_function(our_breaks))) < length(our_breaks)) {
            lab_function <- scales::label_number(0.001, big.mark = " ")
          }
          
          if (length(unique(lab_function(our_breaks))) < length(our_breaks)) {
            lab_function <- scales::label_number(0.0001, big.mark = " ")
          }
          
          if (length(unique(lab_function(our_breaks))) < length(our_breaks)) {
            lab_function <- scales::label_number(0.00001, big.mark = " ")
          }
          
          if (length(unique(lab_function(our_breaks))) < length(our_breaks)) {
            lab_function <- scales::label_number(0.000001, big.mark = " ")
          }
          
          # Generic case
          our_labels <-
            map2_chr(#
              our_breaks[1:length(our_breaks) - 1],
              our_breaks[2:length(our_breaks)], ~ {
                str_c(lab_function(.x), " : ", lab_function(.y))
              })
          
          our_values <- 
            our_breaks[1:length(our_breaks)-1] + 
            (our_breaks[2:length(our_breaks)] - our_breaks[1:length(our_breaks)-1]) / 2
          
          pal <- colorBin(pal_to_use, domain = leg_vals, bins = our_breaks, 
                          reverse = revert_colours)
          
        }
        
        # Step 2.3 Creating labels categories for all ===================================
        our_labels_category <- 
          str_c("Priority ", seq(1, length(our_labels))) %>% 
          rev()
        our_labels_category[[length(our_labels_category)]] <-
          str_c(our_labels_category[[length(our_labels_category)]], 
                " (highest priority)")
        our_radius_sizes <-
          seq(min_buble, max_buble,
              (max_buble - min_buble) / (length(our_values) - 1)) 
        
        # Step 2.4 Treating missing observations ===================================
        if (any(is.na(leg_vals))) {
          our_labels <- c(no_data_label, our_labels)
          our_labels_category <- c(no_data_label, our_labels_category)
          our_values <- c(NA_real_, our_values)
        }
        
      } else {
        our_labels <- unique(leg_vals) %>% as.character()
        our_labels_category <- str_c("Priority 1")
        our_values <-  unique(leg_vals)
        our_radius_sizes_nona <- min_buble
        our_radius_sizes <- our_radius_sizes_nona
        pal <- function(xx) RColorBrewer::brewer.pal(5, pal_to_use)[[2]] %>% rep(., length(xx))
      }
      
      
    } else {
      # Step 4. If categorical TBD ===================================================
      our_values <- levels(leg_vals)
      our_labels <-  levels(leg_vals)
      if (any(is.na(leg_vals))) {
        our_values <- c(NA_real_, our_values)
        our_labels <- c(no_data_label, our_labels)
      }
      pal <- leaflet::colorFactor(palette =  colours_pass, domain = leg_vals, reverse = TRUE)
      our_breaks <- NULL
    }
    
    # recording labels fn -----------------------------------------------------
    # recode_values <- recode_val_base(our_breaks, our_labels_category, no_data_label)
    # function()
    #   function(x) {
    #     if (length(our_breaks) > 1) {
    #       out <-
    #         cut(
    #           x,
    #           breaks = our_breaks,
    #           labels = our_labels_category[our_labels_category != no_data_label],
    #           include.lowest = T,
    #           ordered_result = FALSE
    #         ) %>%
    #         as.character()
    #       
    #       if (any(is.na(out))) {
    #         out[is.na(out)] <- no_data_label
    #       }
    #       return(out)
    #     } else if (length(our_breaks) == 1) {
    #       rep(our_labels_category, length(x))
    #     }
    #   }
    
    # recording labels to intervals fn -----------------------------------------------------
    recode_values_intervals <-
      function()
        function(x) {
          if (length(our_breaks) == 1) {
            our_breaks <- c(our_breaks, our_breaks + 1)
          }
          out <-
            cut(
              x,
              breaks = our_breaks,
              labels = our_labels[our_labels != no_data_label],
              include.lowest = T,
              ordered_result = FALSE
            ) %>%
            as.character()
          
          if (any(is.na(out))) {
            out[is.na(out)] <- no_data_label
          }
          return(out)
          
        }
    
    # recording radiuses fn -----------------------------------------------------
    radius_function <- 
      function() function(x) {
        if (length(our_breaks) > 1) {
          out <-
            cut(x, 
                breaks = our_breaks, 
                labels = our_radius_sizes, 
                include.lowest = TRUE) %>% 
            as.character() %>% 
            as.numeric()
          if (any(is.na(out))) out[is.na(out)] <-  0 #min_buble/2
          return(out)
        } else if (length(our_breaks) == 1) {
          rep(our_radius_sizes, length(x))
        }
      }
    
    our_labels_bubble <- as.character(our_labels)
    our_labels_bubble[is.na(our_labels_bubble)] <- "No data"
    
    list(
      pal = pal,
      recode_function = recode_val_base(our_breaks, our_labels_category, no_data_label),
      recode_function_intervals = recode_values_intervals(),
      radius_function = radius_function(),
      our_labels = rev(our_labels),
      our_labels_bubble = rev(our_labels_bubble),
      our_labels_category = rev(our_labels_category),
      our_values = rev(our_values),
      selected_groups = n_groups
    )
    
  }


#' Functional that returns a funciton for recoing labels in the data 
#' 
#' @noRd
recode_val_base <-
  function(our_breaks,
           our_labels_category,
           no_data_label) {
    function(x) {
      
      if (length(our_breaks) == 1) {
        our_breaks <- c(our_breaks, our_breaks + 1)
      }
      
      out <-
        cut(
          x,
          breaks = our_breaks,
          labels = our_labels_category[our_labels_category != no_data_label],
          include.lowest = T,
          ordered_result = FALSE
        ) %>%
        as.character()
      
      if (any(is.na(out))) {
        out[is.na(out)] <- no_data_label
      }
      
      return(out)
    }
  }


#' New
#' 
#' @noRd
#'
get_pal_lab_fn <- function(x_uni) {
  
  lab_function <- scales::label_number(0.01, big.mark = " ")
  
  if (abs(max(x_uni, na.rm = T) / 10) > 1 & abs(max(x_uni, na.rm = T) / 100) < 1) {
    lab_function <- scales::label_number(0.1, big.mark = " ")
  } 
  
  if (abs(max(x_uni, na.rm = T) / 100) > 1 ) {
    lab_function <- scales::label_number(1, big.mark = " ")
  } 
  
  lab_function
}


#' #' Circle shifter
#' #'
#' #' @description Circle shifter
#' #'
#' #' @noRd
#' #'
#' #' @importFrom purrr transpose map
#' #' @importFrom sf st_sfc st_transform
#' get_shift <- function(rad, .x) {
#'   if (.x >= 5 & .x <= 8) .x <- .x - 4
#'   if (.x >= 9 & .x <= 12) .x <- .x - 8
#'   if (.x >= 13 & .x <= 16) .x <- .x - 12
#'   if (.x >= 17 & .x <= 20) .x <- .x - 16
#'   
#'   if (.x == 1) type <- list(rad, rep(0, length(rad))) 
#'   if (.x == 2) type <- list(-rad, rep(0, length(rad))) 
#'   if (.x == 3) type <- list(rep(0, length(rad)), rad) 
#'   if (.x == 4) type <- list(rep(0, length(rad)), -rad) 
#'   type %>% 
#'     transpose %>% 
#'     map(~{unlist(.x) %>% 
#'         st_point()})  %>% 
#'     st_sfc(., crs = 3857) %>% 
#'     st_transform(crs = 4326)
#' }

#' Legend helpers
#'
#' @noRd
#'
make_shapes <- function(colors, sizes, borders, shapes = "circle") {
  shapes <- gsub("circle", "50%", shapes)
  shapes <- gsub("square", "0%", shapes)
  paste0(
    colors,
    "; width:",
    sizes,
    "px; height:",
    sizes,
    "px; border:0.5px solid ",
    borders,
    "; border-radius:",
    ifelse(!is.na(sizes), shapes, "0%" )
  )
}

#' Legend helpers
#'
#' @noRd
#'
make_labels <- function(sizes, labels) {
  paste0(
    "<div style='display: inline-block;height: ",
    sizes,
    "px;margin-top: 4px;line-height: ",
    sizes,
    "px;'>",
    labels,
    "</div>"
  )
}



#' Breaks helpers 7
#'
#' @noRd
#' 
zeros_after_period <- function(x) {
  if (isTRUE(all.equal(round(x), x)))
    return (rep(0, length(x)))
  out_number <-
    log10(abs(x) - floor(abs(x))) %>%
    abs() %>%
    ceiling(.)
  ifelse(is.infinite(out_number), 0, out_number)
} 





#' #' Generic pallet funciton
#' #'
#' #' @noRd
#' #' 
#' generic_pal <- function(x, nbins, pal_to_use = "RdYlBu") {
#'   # browser()
#'   
#'   x_uni <- unique(x)
#'   n_breaks <- min(length(unique(x)), nbins)
#'   
#'   out_pal <- legend_map_satelite(x, n_groups = n_breaks, val_pal = pal_to_use)
#'   pal_fun <- out_pal$pal
#'   
#'   # pal_fun <- colorQuantile(pal_to_use, domain = x_uni, n = n_breaks)
#'   # if (length(x_uni) == 1) {
#'   #   pal_fun <- function(xx) {
#'   #     rep("darkred", length(xx))
#'   #   }
#'   # }
#'   list(recode_function = out_pal$recode_function,
#'        pal = pal_fun,
#'        selected_groups = n_breaks,
#'        out_pal = out_pal)
#'   
#' }

