#' Dev/debug UI placeholder for the random-weights generator
#'
#' Internal placeholder UI used during development to host buttons
#' that drive `get_rand_weights()` / `get_all_weights_combs()`. Not
#' wired into the production app surface.
#'
#' @param id Character. Shiny module namespace ID.
#'
#' @return A `shiny.tag` -- a single `uiOutput` slot.
#'
#' @importFrom shiny NS uiOutput
#' @noRd
mod_weights_rand_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("rand_weights_ui"))

}

#' Generate a random list of weighting schemes
#'
#' Builds between 1 and 5 weighting schemes (uniform random count),
#' each named with a random 5-letter prefix and a sequential index,
#' and each weight drawn from `runif(-2, 2)` then rounded to the
#' nearest integer. Useful for smoke-testing custom PTI configurations
#' with realistic-shaped (but meaningless) weights.
#'
#' @param indicators_list Tibble with at least a `var_code` column,
#'   typically the output of `get_indicators_list()`.
#'
#' @return A named list of tibbles. Each element is a tibble with
#'   columns `var_code` and `weight`; element names follow the pattern
#'   `"<5-letter-prefix> <index>"`.
#'
#' @importFrom dplyr select mutate
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom stringr str_c
#' @export
#'
#' @examples
#' data(ukr_mtdt_full)
#' set.seed(1)
#' wts <- get_rand_weights(ukr_mtdt_full$metadata)
#' length(wts) >= 1 && length(wts) <= 5
#' all(vapply(wts, function(x) all(c("var_code", "weight") %in% names(x)),
#'            logical(1)))
get_rand_weights <- function(indicators_list) {
  1:(sample(2:5, 1)) %>%
    map(~ {
    list_nm <-
      str_c(sample(letters, 5), collapse = "", sep = "") %>%
      str_c(" ", .x)
    wght <- indicators_list %>%
      select(var_code) %>%
      mutate(weight = runif(nrow(.), -2, 2) %>% round(.))
    set_names(list(wght), list_nm)
  }) %>%
    unlist(recursive = F)
}


#' Enumerate all weighting-scheme combinations of a given size
#'
#' For each `n_combo` in `n_items`, enumerates every `combn(var_codes,
#' n_combo)` and returns one weighting scheme per combination, with
#' weight `1` on the variables in the combination and the others
#' omitted entirely. Useful for exhaustive batch analyses (e.g. all
#' 3-of-N PTIs).
#'
#' @param var_codes Character vector of variable codes to draw
#'   combinations from.
#' @param n_items Integer or integer vector. Combination sizes to
#'   enumerate. Defaults to `3` (all 3-of-N combinations).
#'
#' @return A named list of tibbles. Each element is a tibble with
#'   columns `var_code` and `weight`; element names follow the pattern
#'   `"Wght of <n_combo> comb no. <index>"`. Total length is
#'   `sum(choose(length(var_codes), n_items))`.
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map map2
#' @importFrom rlang set_names
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' data(ukr_mtdt_full)
#' codes <- ukr_mtdt_full$metadata$var_code[1:5]
#' combos <- get_all_weights_combs(codes, n_items = 2)
#' length(combos)
#' choose(5, 2)
get_all_weights_combs <- function(var_codes, n_items = 3) {
  n_items %>%
    map(~{
      n_combo <- .x

      combn(var_codes, n_combo, simplify = F) %>%
        map2(seq_along(.), ~{
          wt_nm <- str_c("Wght of ", n_combo, " comb no. ", .y)
          tibble(var_code = var_codes[var_codes %in% unlist(.x)]) %>%
            mutate(weight = 1) %>%
            list() %>%
            set_names(wt_nm)
        }) %>%
        unlist(recursive = F)
    }) %>%
    unlist(recursive = F)
}
