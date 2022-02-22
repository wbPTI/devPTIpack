#' Compile guide for lunch PTI
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom htmltools HTML
#' @importFrom cicerone Cicerone
guide_launch_pti <- function(
  tabpanel_id = "main_sidebar", 
  ptitab_id = "PTI",
  comparetab_id = "PTI comparison",
  exploretab_id = "Data explorer") {
  
  out_cice <- 
    Cicerone$
    new(#
      id = "apps_guide",
      opacity = 0.35)$
    step(
      el = "step_1_name",
      title = "Specify PTI name",
      description =
        glue(
          # tags$image(src = "www/010-name.gif", class = "popoverimg") %>% as.character(),
          "If you've just loaded the app, provide here the name for your first PTI.") %>%
        htmltools::HTML(),
      # position = "right-top",
      show_btns = TRUE,
      on_highlight_started = str_c('function() {$("#', tabpanel_id, ' a[data-value=\'', ptitab_id, '\']").tab(\'show\');}')
      )$ #
    step(
      el = "step_5_modify_weights",
      is_id = TRUE,
      title = "Modify weights",
      description =
        glue(
          #tags$image(src = "www/018-change-weights.gif", class = "popoverimg") %>% as.character(),
          "Modify weights one-by-one or uses the shortcuts.") %>%
        htmltools::HTML(),
      # position = "top-center",
      show_btns = TRUE
    )$
    step(
      el = "step_3_save", 
      title = "Save PTI",
      description = 
        glue(
          #tags$image(src="www/010-name.gif", class = "popoverimg") %>% as.character(),
          "<br/><br/>As soon as a PTI name is specified and some weihgts are not zero, ",
          "\"Save\" button will be activated. ",
          # tags$image(src="www/guide_save.GIF", class = "smallimg") %>% as.character(),
          "<br/>Press it to save your PTI. ",
          "Only after saving weights under certain name, the map will be updated."
        ) %>% 
        htmltools::HTML(),
      # position = "right-top",
      show_btns = TRUE
    )$
    step(
      el = "step_4_delete", 
      title = "Delete/Reset PTI",
      description = 
        glue("You may delete the current weighting scheme or reset PTI any time.") %>% 
        htmltools::HTML(),
      # position = "right-top",
      show_btns = TRUE
    )$
    step(
      el = "step_8_map_inspection1",
      title = "Inspect the Map",
      description =
        glue("Inspect the map") %>%
        htmltools::HTML(),
      position = "mid-center",
      show_btns = TRUE
    )$
    step(
      el = "step_5_downalod_upload1",
      title = "Download PTI",
      description =
        glue("You may download various components of the PTI.") %>% 
        htmltools::HTML(),
      # position = "right-top",
      show_btns = TRUE
      )$
    step(
      el = tabpanel_id,
      title = "Switch between tabs",
      description =
        glue("Use tabs panel to switch between tabs.") %>%
        htmltools::HTML(),
      position = "bottom-center",
      show_btns = TRUE,
      on_highlight_started = 
        str_c('function() { $("#', tabpanel_id, ' a[data-value=\'', ptitab_id, '\']").tab(\'show\');}')
      )
  
  
  
  if (isTruthy(comparetab_id)) {
    out_cice <-
      out_cice$
      step(
        el = str_c("#", tabpanel_id, " li:nth-child(3)"),
        is_id = FALSE,
        title = "Compare multiple PTIs side-by-side.",
        description =
          glue("When two or more PTIs are specified, they could be compared side-by-side.") %>% 
          htmltools::HTML(),
        position = "bottom-left",
        show_btns = TRUE,
        on_highlight_started = str_c(
          'function() { $("#', tabpanel_id,
          ' a[data-value=\'', comparetab_id,
          '\']").tab(\'show\'); }'
        )
      )$
      step(
        el = "compare_left_1",
        is_id = TRUE,
        title = "Select different PTIs and bins numbers.",
        description =
          tags$image(src = "www/030-compare.gif", class = "popoverimg") %>%
          as.character() %>%
          htmltools::HTML(),
        position = "mid-center",
        show_btns = TRUE
      )
    
  }
  
  
  if (isTruthy(exploretab_id)) {
    out_cice <- 
      out_cice$
      step(
        el = str_c("#", tabpanel_id, " li:nth-child(4)"),
        is_id = FALSE,
        title = "Explore actual data behind PTI",
        description =
          glue(
          "On the explorer tab, one can visualise data that is used for", 
          " construction PTIs.<br/>", 
          "It is also possible to show locations of different projects."
          ) %>%
          htmltools::HTML(),
        position = "bottom-left",
        show_btns = TRUE
      )$
      step(
        el = "explorer_1",
        is_id = TRUE,
        title = "Explore data",
        description =
          tags$image(src = "www/035-explorer.gif", class = "popoverimg") %>%
          as.character() %>%
          htmltools::HTML(),
        position = "mid-center",
        show_btns = TRUE,
        on_highlight_started = 
          str_c(
            'function() { $("#', tabpanel_id, 
            ' a[data-value=\'', exploretab_id ,
            '\']").tab(\'show\');}'
            )
        )
  }
  
  out_cice
  
  
}
