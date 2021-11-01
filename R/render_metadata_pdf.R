#' @export
#' @noRd
#' @importFrom rmarkdown render
#' @importFrom here here
render_metadata <- 
  function(data_path, geometries_path, output_file) {
    rmarkdown::render(
      here(".", "inst", "pti-metadata-pdf.Rmd"), 
      output_file = output_file,
      params = list(mtdt_path = data_path, bond_path = geometries_path) ,
      knit_root_dir = here(".")
    )
  }