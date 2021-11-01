
mod_fetch_data_srv <- function(id, data_fldr, data_path = NULL, data_dta = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      reactive({
        # browser()
        # req(length(list.files(data_fldr, "*.xlsx")) > 0)
        
        if (!is.null(data_dta)) {
          data_dta
        } else if (is.null(data_path) || !file.exists(data_path)) {
          fct_template_reader(data_fldr, list.files(data_fldr, "*.xlsx")[[1]])
        } else {
          fct_template_reader(data_path)
        }
      })
    }
  )
}
