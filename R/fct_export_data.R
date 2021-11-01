


rename_one <- function(out, var, name) {
  if (var %in% names(out)) {
    out <-
      out %>%
      rename_at(vars(one_of(var)), list( ~ name))
  }
  out
}


prepare_tibble_to_export <- function(out) {
  
  out %>% 
    rename_one("REG", "Region") %>% 
    rename_one("COD_REG", "Region code") %>% 
    rename_one("year", "Year") %>% 
    rename_one("var_name", "Variable") %>% 
    rename_one("values", "Value") %>% 
    rename_one("var_units", "Units") %>% 
    rename_one("flag", "Flag")%>% 
    rename_one("point_title", "Point title")%>% 
    rename_one("point_group", "Point group")%>% 
    rename_one("point_value", "Point value")%>% 
    rename_one("point_extra", "Point extra")%>% 
    rename_one("flag", "Flag")
  
}


prepare_DT_export <- function(.data, file_title, digits_number = 3) {
  .data %>%
    DT::datatable(
      .,
      rownames = FALSE,
      extensions = c("Buttons", "Scroller"),
      options = list(
        pageLength = 25,
        dom = c("Bfrtip"),
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        buttons = list(
          list(extend = "copy", text = "Copy clipboard"),
          list(
            extend = "excel",
            text = "Download table in Excel",
            title = file_title
          )
        )
      )
    ) %>% 
    DT::formatRound(columns = names(.data)[sapply(.data, is.numeric)], 
                    digits = digits_number)
}


dt_wrap_exported <- 
  function(iterator, globr, check_globr = FALSE, data_from = "data_2_indicators") {
    
    if (check_globr) {
      validate(need(isTruthy(globr[[data_from]]$data),
                    "Please select at least one indicator on the 'Map' page."))
      
    }
    
    req(globr[[data_from]]$data)
    # browser()
    if (length(globr[[data_from]]$data)>=iterator) {
      .x <- globr[[data_from]]$data[[iterator]]
      if (! .x$var_privacy) {
        out <- 
          .x$var_dta %>%
          mutate(var_name = .x$var_name,
                 var_units = .x$var_units) %>%
          prepare_tibble_to_export() %>%
          mutate(Year = as.character(Year),
                 `Region code` = as.character( `Region code`),
                 Source = as.character(.x$var_source),
                 Description = as.character(.x$var_description)) %>% 
          select(-one_of("area")) %>% 
          prepare_DT_export(file_title = .x$var_code, digits_number = 2) 
      } else {
        out <- NULL
      }
      
    } else {
      
      out <- NULL
    }
    out 
  }
