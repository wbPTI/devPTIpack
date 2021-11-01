
#' load_shapes Server Function
#'
#' @noRd 
mod_load_shapes_server <- function(input, output, session, globr, shapes_list){
  ns <- session$ns
  
  shapes_fldr <- "app-shapes/"
  globr[[shapes_list]] <-
    list.files(shapes_fldr, "rds") %>%
    stringr::str_replace_all("\\.rds", "") %>% 
    purrr::map(~ {
      glue::glue("{shapes_fldr}{.x}.rds") %>% 
        readr::read_rds() %>% 
        list() %>% 
        purrr::set_names(.x)
    }) %>%
    unlist(recursive = F) 
  
  # reactive({
  #   req(globr[[shapes_list]]) %>% 
  #     return()
  # })
}

# New get shape module 
mod_get_shape_srv <-
  function(id,
           shapes_fldr = "app-shapes/",
           shape_country = "Country name",
           shape_path = NULL,
           shape_dta = NULL) {
    moduleServer(#
      id,
      function(input, output, session) {
        ns <- session$ns
        reactive({
          get_shape(shapes_fldr, shape_country, shape_path, shape_dta)
        })
      })
  }

#' @export
get_shape <- function(shapes_fldr, shape_country, shape_path = NULL, shape_dta = NULL) {
  
  if (!is.null(shape_dta)) return(shape_dta)
  if (is.null(shape_path) || !file.exists(shape_path)) {
    list.files(shapes_fldr, "rds") %>%
      magrittr::extract(stringr::str_detect(., shape_country)) %>%
      stringr::str_replace_all("\\.rds", "") %>%
      stringr::str_c(shapes_fldr, ., ".rds") %>%
      readr::read_rds() %>%
      return()
  } else {
    shape_path %>%
      readr::read_rds() %>%
      return()
  }
}
    
## To be copied in the UI
# mod_load_shapes_ui("load_shapes_ui_1")
    
## To be copied in the server
# callModule(mod_load_shapes_server, "load_shapes_ui_1")
 
