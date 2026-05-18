#' devPTIpack: Priority Targeting Index analysis toolkit
#'
#' Computes, visualises, and explores Priority Targeting Indices
#' (PTI) over administrative geographies. The package ships a Shiny
#' application launched via [launch_pti()] together with the
#' Shiny-free pipeline orchestrator [run_pti_pipeline()].
#'
#' @keywords internal
#' @importFrom grDevices dev.off pdf png
#' @importFrom stats quantile setNames
#' @importFrom utils str
#' @importFrom shiny outputOptions hr
"_PACKAGE"

# Quiet R CMD check NOTE about NSE column-name bindings used inside
# dplyr / tidyselect / glue verbs throughout the package.
utils::globalVariables(c(
  ".",
  "admin0Pcod",
  "admin9Name",
  "admin9Pcod",
  "admin_level",
  "admin_levels_years",
  "area",
  "colours_pass",
  "data",
  "fltr_exclude_explorer",
  "fltr_exclude_pti",
  "fltr_overlay_explorer",
  "fltr_overlay_pti",
  "geometry",
  "legend_revert_colours",
  "line",
  "pillar_description",
  "pillar_group",
  "pillar_name",
  "pti_name",
  "pti_score",
  "pti_score_category",
  "spatial_level",
  "tooltip_text",
  "ttip_id",
  "type",
  "val",
  "value",
  "var",
  "var_adm_levels",
  "var_code",
  "var_description",
  "var_name",
  "var_order",
  "var_years",
  "width"
))
