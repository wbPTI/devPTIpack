# Testing: PTI Calculation Pipeline

> Extracted from `arch-02-testing.md` § 1.1.
> This document is the single source of truth for calculation pipeline tests.

---

## Pipeline Overview

The PTI calculation is a linear chain of pure functions. No Shiny dependencies.

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│  INPUT                     CORE PIPELINE                          OUTPUT     │
│  ─────                     ─────────────                          ──────     │
│                                                                              │
│  shp_dta  ──┬── get_mt() ───────────────────────────────────────────┐        │
│             │                                                       │        │
│             ├── clean_geoms() ──────────────────────────────────┐   │        │
│             │                                                   │   │        │
│  input_dta ─┼── pivot_pti_dta() ──┐                             │   │        │
│             │                     │                             │   │        │
│             └── get_adm_levels()  │                             │   │        │
│                                   ▼                             │   │        │
│  weights_clean ──► get_weighted_data() ──► get_scores_data()    │   │        │
│                                                    │            │   │        │
│                                                    ▼            │   │        │
│                                       expand_adm_levels() ◄─────┼───┘        │
│                                                    │            │            │
│                                                    ▼            │            │
│                                  merge_expandedn_adm_levels()   │            │
│                                                    │            │            │
│                                                    ▼            │            │
│                                         agg_pti_scores() ◄──────┘            │
│                                                    │                         │
│                                                    ▼                         │
│                                        label_generic_pti()                   │
│                                                    │                         │
│                                                    ▼                         │
│                                       structure_pti_data() ──► FINAL OUTPUT  │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘
```

**Source files:** `R/calc_pti_helpers.R`, `R/calc_pti_expander.R`

---

## Test Strategy: Three Levels

| Level              | Scope                        | Purpose                                    |
| ------------------ | ---------------------------- | ------------------------------------------ |
| **A. Unit**        | Each function in isolation   | Correct I/O contracts, edge cases          |
| **B. Integration** | Adjacent function pairs      | Data flows correctly between steps         |
| **C. End-to-end**  | Full pipeline as single call | Business logic produces valid final output |

All three levels are Tier 1 (pure functions, no Shiny).

---

## Orchestrator Function (Proposed)

To simplify end-to-end testing, wrap the pipeline in a single callable:

```r
#' Run the full PTI calculation pipeline
#' @param weights_clean Named list of weight tibbles
#' @param input_dta Metadata list (with admin sheets)
#' @param indicators_list Tibble from get_indicators_list()
#' @param shp_dta Country shapes (list of sf)
#' @param na_rm Logical: ignore NAs in score aggregation?
#' @return Named list per admin level with $pti_data (sf), $pti_codes, $admin_level
#' @export
run_pti_pipeline <- function(weights_clean, input_dta, indicators_list, shp_dta, na_rm = FALSE) {
  long_vars       <- pivot_pti_dta(input_dta, indicators_list)
  existing_shapes <- clean_geoms(shp_dta)
  mt              <- get_mt(shp_dta)

  weights_clean |>
    get_weighted_data(long_vars, indicators_list) |>
    get_scores_data() |>
    purrr::imap(~ expand_adm_levels(.x, mt) |> merge_expandedn_adm_levels()) |>
    agg_pti_scores(existing_shapes, na_rm_pti2 = na_rm) |>
    label_generic_pti() |>
    structure_pti_data(shp_dta)
}
```

This gives tests a single entry point while individual functions remain independently testable.

---

## Level A: Unit Tests (per function)

### A.1 `get_mt(country_shapes)`

| #   | Test Case                  | Input                                    | Expected                                                                        |
| --- | -------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------- |
| 1   | Normal 3-level shapes      | `ukr_shp`                                | Tibble with `admin0Pcod`, `admin1Pcod`, `admin2Pcod`; nrow = nrow(admin2 layer) |
| 2   | Single admin level         | List with 1 sf                           | Single-column tibble                                                            |
| 3   | Mismatched Pcods (orphans) | Modified shapes with extra Pcod in child | Orphan rows excluded (filter_all removes NA)                                    |
| 4   | Output class               | Any                                      | `tbl_df`                                                                        |
| 5   | No duplicates              | Any                                      | All `admin{X}Pcod` × row combos unique                                          |
| 6   | Column naming              | Any                                      | All columns match `admin\d+Pcod` pattern                                        |
| 7   | Empty shapes list          | `list()`                                 | Errors (`purrr::reduce` of empty list with no `.init`)                          |
| 8   | Single sf, multiple Pcods  | sf with admin0Pcod + admin1Pcod          | Returns those columns; reduce no-ops over a length-1 list                       |
| 9   | List names ignored         | sf list with arbitrary names             | Names don't affect output; only column names drive the join                     |

> **What it does:** Builds a mapping table that links every admin-level Pcod to its parents.
>
> **Mechanism:** Drops geometry from each sf via `st_drop_geometry()`, then `purrr::reduce`s the list with a custom binary op: at each step it (a) selects from the accumulator only columns containing `"admin"`, (b) drops accumulator columns whose names contain `"Name"` (case-sensitive), and (c) `full_join`s the right-hand layer on the accumulator columns whose names contain `"Pcod"`. After the reduce, keeps `contains("Pcod")` and applies `filter_all(all_vars(!is.na(.)))` to drop rows with any NA Pcod (orphans). The function never touches the **list element names** — only column names matter.
>
> **Input — `country_shapes`:** Named list of sf objects (or anything supporting `st_drop_geometry`). Each layer must contain an `admin{N}Pcod` column (character). Child layers must also carry parent Pcod columns so that the join keys exist (e.g. admin2 layer needs `admin0Pcod`, `admin1Pcod`, `admin2Pcod`). Minimum: 1 element. **List element names are not consumed** by `get_mt()` (they are only consumed by `clean_geoms()` and `structure_pti_data()`).
>
> **Output:** A tibble (class `tbl_df`) with one column per admin level (`admin0Pcod`, `admin1Pcod`, …). Row count equals the number of finest-level units that have a complete chain of parent Pcods (no NAs after join). No geometry. No duplicate rows.

### A.2 `get_adm_levels(dta)`

| #   | Test Case            | Input                     | Expected                                        |
| --- | -------------------- | ------------------------- | ----------------------------------------------- |
| 1   | Normal mapping table | `get_mt(ukr_shp)`         | `c("admin0", "admin1", "admin2")`               |
| 2   | No admin columns     | `tibble(x = 1)`           | `character(0)`                                  |
| 3   | Names match values   | Any                       | `names(result) == result`                       |
| 4   | Lexicographic sort        | Jumbled column order      | Sorted by `sort()` on character — works for `admin0..admin9`     |
| 5   | High admin number alone   | Tibble with `admin12Pcod` | Extracts `"admin12"` correctly                                   |
| 6   | **Mixed single + double-digit (KNOWN ISSUE)** | Cols `admin1, admin12, admin2` | Result is `c("admin1","admin12","admin2")` — **lexicographic, not numeric** |
| 7   | Duplicate admin levels    | Two cols both `admin1Pcod`+`admin1Name` | After `sort` + `set_names`, result keeps duplicates       |
| 8   | Non-admin columns mixed in | `tibble(x=1, admin1Pcod=1)` | NA from non-matching col is dropped by default `na.last = NA`   |

> **What it does:** Extracts sorted admin-level identifiers from column names.
>
> **Mechanism:** Takes column names of a tibble, applies `str_extract("admin\\d{1,2}")` to each, calls `sort()` (which removes NAs by default — `na.last = NA`), and returns the result with `set_names(.)` so names equal values.
>
> **Input — `dta`:** Any tibble whose column names may contain `admin{N}Pcod` patterns. Typically the mapping table from `get_mt()`, but works on any tibble. If no columns match, returns `character(0)`.
>
> **Output:** Named character vector, e.g. `c(admin0 = "admin0", admin1 = "admin1", admin2 = "admin2")`. **Sorted lexicographically (not numerically)**, so it is correct for `admin0..admin9` but produces `admin1 < admin10 < admin2` for two-digit admin numbers. Length equals the number of distinct admin levels found. Empty vector if no admin columns present.
>
> ⚠️ The downstream code (`expand_adm_levels`, `agg_pti_scores`) compares levels by `as.integer(str_extract(name, "\\d"))` — i.e. only the **first digit** — so the same lexicographic-vs-numeric tension exists across the pipeline. Tests should pin behavior, not pretend it sorts numerically.

### A.3 `clean_geoms(country_shapes)`

| #   | Test Case              | Input     | Expected                                   |
| --- | ---------------------- | --------- | ------------------------------------------ |
| 1   | Drops geometry         | `ukr_shp` | All elements are plain tibbles (not sf)    |
| 2   | Names shortened        | Any       | Names match `admin\d+` pattern (no suffix) |
| 3   | Preserves Pcod columns | Any       | Each element has `admin{X}Pcod` column     |
| 4   | Preserves Name columns | Any       | `admin{X}Name` columns retained            |
| 5   | List length preserved  | Any       | `length(output) == length(input)`          |
| 6   | **Name without `_`**   | Element named `admin1` (no suffix) | Output name becomes `NA` — `str_extract("^(.*?)_")` returns NA |
| 7   | Element with no Name col | sf without `admin{X}Name`         | Result tibble simply has no Name column; no error           |

> **What it does:** Strips sf geometry from shape layers, converting them to plain tibbles.
>
> **Mechanism:** Maps `st_drop_geometry() %>% as_tibble()` over each list element. Renames list elements from `admin{N}_{Label}` to just `admin{N}` by `str_extract("^(.*?)_") %>% str_replace("_", "")`. **Caveat:** if a list element name has no underscore, `str_extract` returns NA and that element ends up with name `NA` — pipelines downstream that look up `existing_shapes[[adm_level]]` will then fail to match.
>
> **Input — `country_shapes`:** Same named list of sf objects as `get_mt()`. Names must follow `admin{N}_{Label}` convention.
>
> **Output:** Named list of tibbles, same length as input. Each tibble retains all non-geometry columns (`admin{N}Pcod`, `admin{N}Name`, etc.) but has no `geometry`/`sfc` column. List names are shortened (e.g. `admin1_Oblast` → `admin1`). Used downstream by `agg_pti_scores()` which looks up `spatial_name` from these tibbles.

### A.4 `pivot_pti_dta(input_dta, indicators_list)`

| #   | Test Case                  | Input                            | Expected                                             |
| --- | -------------------------- | -------------------------------- | ---------------------------------------------------- |
| 1   | Normal pivot               | `ukr_mtdt_full`, its indicators  | Non-empty list of long tibbles                       |
| 2   | Output columns             | Any                              | Each tibble has `var_code`, `value`, admin Pcod cols |
| 3   | NA rows dropped            | Data with NA values              | No NA in `value` column                              |
| 4   | Missing var_code in data   | indicators_list with extra var   | Those vars absent from output                        |
| 5   | Empty indicators_list      | `tibble(var_code = character())` | List of 0-row tibbles                                |
| 6   | List length = admin levels | Any                              | One element per admin sheet in input_dta             |
| 7   | Distinct rows              | Duplicate source rows            | No duplicates in output                              |
| 8   | Year column preserved      | Data with `year` column          | `year` appears in output tibbles                     |
| 9   | **`agg_*` columns retained** | Sheet with column `agg_pop`    | `agg_pop` survives the select and is retained alongside `var_code`/`value` |
| 10  | Element-name filter is `"admin"` not `"admin\d"` | List with element `"administrative"` | Element is **incorrectly included** — filter is loose |
| 11  | `year` required downstream | Output without `year`            | `expand_adm_levels` will later error via `all_of("year")` |

> **What it does:** Reshapes each admin-level data sheet from wide format (one column per indicator) to long format (one row per indicator × region).
>
> **Mechanism:** Filters `input_dta` to elements whose **names contain the literal string `"admin"`** (`str_detect("admin")` — note: this also matches names like `"administrative"`, not just `admin\d`). For each kept element, selects columns matching: `contains("agg_")` (auxiliary aggregation columns — kept but not pivoted), `matches("admin\\d")` (admin identifier columns), and `any_of(c("area", "year", var_codes))`. Then `pivot_longer(cols = any_of(var_codes), names_to = "var_code", values_to = "value", values_drop_na = TRUE)` and `distinct()`.
>
> **Input — `input_dta`:** Named list (the metadata object). Must contain elements whose names contain `"admin"` (typically `admin1_Region`, `admin2_District`). Each element is a tibble in wide format with columns: `admin{N}Pcod`, optionally `admin{N}Name`, optionally `area`, optionally `agg_*` columns, optionally `year`, plus one column per indicator whose name matches a `var_code`.
>
> **Input — `indicators_list`:** Tibble with at least a `var_code` column (character vector of indicator identifiers). Only variables listed here are pivoted; others are silently dropped by the upstream `select(any_of(var_codes))`.
>
> **Output:** Named list of long tibbles (one per matching sheet). Each tibble has columns: `var_code` (character), `value` (numeric, no NAs — dropped by `values_drop_na`), admin Pcod columns, plus any retained `area`/`year`/`agg_*` columns. List length equals the number of input list elements whose names contain `"admin"`.
>
> ⚠️ **Pipeline gotcha:** `expand_adm_levels` later requires a `year` column via `all_of(c("year", "var_code", "value"))`. So although `pivot_pti_dta` itself accepts data without `year`, **the rest of the pipeline does not**. Any fixture without a `year` column will crash at `expand_adm_levels`.

### A.5 `get_weighted_data(wt_list, vars_dta_list, indicators_list)`

| #   | Test Case             | Input                           | Expected                                                               |
| --- | --------------------- | ------------------------------- | ---------------------------------------------------------------------- |
| 1   | Normal weighting      | Default weights + pivoted data  | Nested list; `value` = original × weight                               |
| 2   | All weights = 0       | Zero-weights list               | Keeps all rows, value × 0 = all zeros                                  |
| 3   | All weights = NA      | NA-weights list                 | Treated as weight = 0 (replace_na logic)                               |
| 4   | var_code mismatch     | Weights for non-existent vars   | Inner join → 0-row tibbles                                             |
| 5   | Negative weights      | Weights with -1                 | `value` sign flipped                                                   |
| 6   | Single weight scheme  | 1-element wt_list               | 1-element outer list                                                   |
| 7   | Weight column removed | Any                             | Output tibbles have no `weight` column                                 |
| 8   | Structure preserved   | Any                             | Outer list length = `length(wt_list)`, inner = `length(vars_dta_list)` |
| 9   | Non-zero filter       | Weights with mix of 0 and non-0 | Only non-zero var_codes retained (filtered by inner_join after filter) |
| 10  | Fractional weights    | Weight = 0.5                    | `value` halved                                                         |

> **What it does:** Multiplies indicator values by user-assigned weights.
>
> **Mechanism:** Iterates over each weight scheme in `wt_list`. First calls `replace_na(weight, 0)` to coerce NA weights to 0. Then, if not all weights are zero, filters to only non-zero weight rows (`filter(weight != 0, !is.na(weight))`; the NA filter is defensive — `replace_na` has already run). For each admin-level tibble in `vars_dta_list`, performs `inner_join(ws, by = "var_code")`, computes `value = value * weight`, then drops every column whose name **contains** `"weight"` via `select(-contains("weight"))` (so e.g. an upstream `weighted_avg` column would also be dropped — minor sharp edge). The inner join means only var_codes present in both weights and data survive; mismatched codes produce 0-row tibbles.
>
> **Input — `wt_list`:** Named list of tibbles, one per weight scheme. Each tibble must have columns `var_code` (character) and `weight` (numeric). Names become scheme identifiers downstream. Weights can be any real number (positive, negative, fractional, zero, NA).
>
> **Input — `vars_dta_list`:** Named list of long tibbles from `pivot_pti_dta()`. Each must have `var_code` and `value` columns.
>
> **Input — `indicators_list`:** Currently unused inside the function but passed for API consistency.
>
> **Output:** Doubly-nested list. Outer list: one element per weight scheme (length = `length(wt_list)`). Inner list: one tibble per admin level (length = `length(vars_dta_list)`). Each tibble has the same columns as the input minus `weight`. The `value` column now holds `original_value × weight`. Special case: if all weights are zero, the zero-weight rows are kept (not filtered) so the output still has rows, but all values are 0.

### A.6 `get_scores_data(wt_dta_list)`

| #   | Test Case                 | Input                        | Expected                             |
| --- | ------------------------- | ---------------------------- | ------------------------------------ |
| 1   | Z-score correct           | Known values `[1, 2, 3]`     | `[-1, 0, 1]` (mean=2, sd=1)          |
| 2   | **Single observation (KNOWN ISSUE)** | 1-row group     | `value = NA` — `sd()` of length-1 returns NA, not NaN; `is.nan` filter misses it |
| 3   | All-identical values (multi-row) | Constant column     | `value = 0` (sd = 0 → NaN → caught) |
| 4   | Empty tibble              | 0-row element                | Returned unchanged (0-row)           |
| 5   | Groups by year × var_code | Multi-year data              | Independent z-scores per group       |
| 6   | NaN replaced              | Any producing NaN            | No NaN in output, replaced with 0    |
| 7   | Structure preserved       | Same nested structure in/out | Outer × inner list lengths unchanged |
| 8   | Large variance            | Values `[0, 1000]`           | Correct z-scores, no overflow        |

> **What it does:** Z-standardizes weighted indicator values so that all indicators become comparable regardless of original scale.
>
> **Mechanism:** Iterates through the doubly-nested list structure. For each tibble with > 0 rows, groups by `year` and `var_code` using `group_by_at(vars(any_of(c("year", "var_code"))))` — note `any_of`, so if `year` is absent the grouping silently degrades to `var_code` alone (cross-year scoring). Then computes `value = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)`. Any resulting NaN (which occurs when `sd = 0`, i.e. all values in the group are identical, or `sd = NA`) is replaced with 0 via `ifelse(is.nan(value), 0, value)`. Note this only catches NaN — true NA (from `sd` of a 1-row group with `na.rm = TRUE`) is NOT replaced. Then ungroups.
>
> **Input — `wt_dta_list`:** Doubly-nested list from `get_weighted_data()`. Structure: outer = weight schemes, inner = admin levels. Each inner tibble must have `value` (numeric), `var_code` (character), and optionally `year`. Tibbles with 0 rows are passed through unchanged.
>
> **Output:** Same doubly-nested structure, same dimensions. The `value` column now holds z-scores (mean ≈ 0, sd ≈ 1 per group). No NaN values in output. Groups with a single observation or constant values get `value = 0`.

### A.7 `expand_adm_levels(wtd_scrd_dta, mt)`

| #   | Test Case              | Input                         | Expected                                                  |
| --- | ---------------------- | ----------------------------- | --------------------------------------------------------- |
| 1   | Upward expansion       | Data at admin1, target admin2 | admin2 rows get parent admin1 values                      |
| 2   | Downward aggregation   | Data at admin2, target admin1 | admin1 rows get mean of children                          |
| 3   | Same level             | Data at admin1, target admin1 | Identity (no suffix)                                      |
| 4   | Single admin           | 1-level data                  | Single entry with no expansion                            |
| 5   | Missing Pcods in mt    | Orphan regions                | Filtered out (all_vars !is.na filter)                     |
| 6   | Foreign-column suffix format | Any foreign level         | Column names get `._._.admin{X}` suffix (**no space** — literal `._._.` followed by level name) |
| 7   | Output structure       | Any                           | Nested list: outer = source levels, inner = target levels |
| 8   | Empty input tibble     | 0-row element at one level    | All target outputs are NULL                               |
| 9   | **No element matches level** | `wtd_scrd_dta` missing admin1 entry | Source-loop branch returns nested NULLs (bypassed)  |
| 10  | **>1 element matches level** | Names `admin1_a`, `admin1_b` both contain `"admin1"` | `length(...) == 1` fails → returns nested NULLs |
| 11  | Year required, then dropped | Tibble missing `year`     | `all_of(c("year",...))` errors                            |
| 12  | Numeric precision      | Aggregation of many children  | Mean is accurate to machine precision                     |

> **What it does:** Cross-level extrapolation — broadcasts scored data from each source admin level to every target admin level.
>
> **Mechanism:** Gets all admin levels from `mt` via `get_adm_levels()`. For each source level `adm_from` (outer loop), it picks list elements whose names **contain** `adm_from` via `str_detect(names(wtd_scrd_dta), adm_from)`. **Only proceeds if exactly one element matches AND that tibble has > 0 rows** — otherwise the entire source-loop iteration returns a nested list of `NULL`s. When proceeding, it `select(contains(adm_from), all_of(c("year","var_code","value")))` (`year` is required to exist), then drops `contains("Name")` and `contains("year")` (so year is required-then-discarded), pivots wide to one column per `var_code`, and creates a renamed copy where indicator columns get a `._._.{adm_from}` suffix (literal `._._.` + level name, **no space**). For each target level (inner loop), levels are compared by `as.integer(str_extract(col_name, "\\d"))` — note this only takes the FIRST digit, so `admin12` → 1, conflating with `admin1`. Three branches:
> - **Upward expansion** (`sl_from < sl_to`, e.g. admin1 → admin2): `full_join` `local_mt` (the dedup'd subset of `mt` with both Pcod columns) and the suffixed wide data on `col_from`, so each child row inherits the parent's values. Then drops the source Pcod column and filters rows with NA in any target Pcod column.
> - **Same level** (`sl_from == sl_to`): Identity — returns `adm_to_dta`, the wide tibble **without** the suffix.
> - **Downward aggregation** (`sl_from > sl_to`, e.g. admin2 → admin1): `full_join` `local_mt` and the suffixed wide data, drop source Pcod, filter NA target Pcods, `group_by_at(contains(adm_to))`, `summarise_all(mean(., na.rm = TRUE))`.
>
> **Input — `wtd_scrd_dta`:** A single weight scheme's scored data — named list of long tibbles, as produced by the inner list of `get_scores_data()`. Each tibble must have `var_code`, `value`, `year`, and the relevant `admin{N}Pcod` column. **`year` is required** even though it is dropped before the pivot. If a level's tibble has 0 rows OR no name matches OR multiple names match, all its target outputs are NULL.
>
> **Input — `mt`:** Mapping table from `get_mt()`. Must have all `admin{X}Pcod` columns. Used to determine parent-child relationships and to join across levels.
>
> **Output:** Doubly-nested list: outer indexed by source level, inner by target level. Each leaf is a wide tibble. Foreign-level indicator columns carry the `._._.admin{X}` suffix; same-level columns do not. The exact column set differs by branch: upward/downward branches include the target Pcod column plus all `local_mt` columns retained by `select(contains(adm_to), everything())` (minus the source Pcod) plus indicator columns; the same-level branch returns the un-suffixed wide tibble verbatim. Leaves are `NULL` when the source level had no matching data element, more than one matching element, or the matching element had 0 rows.

### A.8 `merge_expandedn_adm_levels(dta)`

| #   | Test Case              | Input                                | Expected                                  |
| --- | ---------------------- | ------------------------------------ | ----------------------------------------- |
| 1   | Normal merge           | Output of `expand_adm_levels`        | List of wide tibbles (1 per target admin) |
| 2   | All-NA columns removed | Data with fully-missing foreign vars | Those columns absent                      |
| 3   | NULL elements handled  | List with NULLs                      | Skipped gracefully                        |
| 4   | Join key correct       | Any                                  | Joined on `admin{X}Pcod`                  |
| 5   | No duplicate columns   | Multi-source merge                   | Each column appears once                  |

> **What it does:** Joins all source-level expansions into a single wide tibble per target admin level.
>
> **Mechanism:** Transposes the doubly-nested list (source × target → target × source) via `purrr::transpose()`. For each target admin level, iterates over the source-level tibbles: filters out NULLs, then `reduce`s with `left_join` by `admin{X}Pcod`. Before each join, removes columns from the right-hand side that already exist in the left-hand side (except the join key) to avoid duplicates. Finally, drops any column that is entirely NA via `select_if(function(x) !all(is.na(x)))`.
>
> **Input — `dta`:** Doubly-nested list from `expand_adm_levels()`. Structure: `source_level → target_level → tibble`. Some leaves may be NULL (when a source level had no data).
>
> **Output:** Named list of wide tibbles, one per target admin level. Each tibble has the target's `admin{X}Pcod` column plus all indicator columns from all source levels (native columns without suffix, foreign columns with `._._.admin{X}` suffix). All-NA columns are removed. Column names are unique.

### A.9 `agg_pti_scores(extrap_dta, adm_ids, na_rm_pti2)`

| #   | Test Case                    | Input                              | Expected                                                |
| --- | ---------------------------- | ---------------------------------- | ------------------------------------------------------- |
| 1   | Normal aggregation           | Full pipeline output               | `pti_score` = rowSums of score columns                  |
| 2   | `na_rm = FALSE` (default)    | Data with NAs                      | `pti_score` = NA where any input NA                     |
| 3   | `na_rm = TRUE`               | Data with NAs                      | `pti_score` computed, NAs ignored                       |
| 4   | All scores NA + `na_rm=TRUE` | All NA row                         | `pti_score = 0` (rowSums of empty)                      |
| 5   | Duplicate foreign vars       | Same base name from 2 admin levels | Keeps highest admin level only                          |
| 6   | Output columns               | Any                                | `pti_score`, `pti_name`, `spatial_name` present         |
| 7   | Multiple weight schemes      | 3-scheme input                     | Output transposed: list by admin, rows bound per scheme |
| 8   | Native vs foreign            | Vars at own level + foreign vars   | Both contribute to sum                                  |
| 9   | pti_name values              | 3 schemes named "A", "B", "C"      | `pti_name` column has those exact values                |

> **What it does:** Aggregates all indicator score columns into a single composite `pti_score` per row, adds scheme names and spatial names, then restructures the output by admin level.
>
> **Mechanism:** First resolves the `na_rm` flag with precedence **`na_rm_pti2` argument > `get_golem_options("na_rm_pti")` > `FALSE`**. Iterates over weight schemes (outer) then admin levels (inner). For each admin-level tibble: separates columns into non-summable (`matches("^admin\\d")`, `matches("^year$")`) vs. summable. Among summable, distinguishes "native" (no `._._.admin\d` suffix) from "foreign" (with the suffix). Foreign columns whose **base name** (split on `._._.`) collides with a native column are **dropped first** (not summed twice). Among the remaining foreign columns, deduplicates by base name keeping only the row with `level == max(level, na.rm = TRUE)` — note this max is **lexicographic on the level string** (`"admin2" > "admin1"` works; `"admin12"` would beat `"admin2"` only because lexicographic compares char-by-char and `"1" < "2"`, so actually `"admin12" < "admin2"` — yet another two-digit gotcha). Computes `pti_score = rowSums(select(any_of(foreign_coll), naitive_col), na.rm = na_rm_pti)` (note source uses the typo `naitive_col`). Adds `pti_name = .y` (the weight scheme name) and `left_join`s `spatial_name` from `adm_ids[[admin_level]]` (the cleaned-geom tibble) by the non-summable columns. Finally `filter_at(vars(contains(nonsum_cols)), all_vars(!is.na(.)))` drops any row with NA in a Pcod-or-year column, then `transpose() %>% imap(~ bind_rows(.x))` flips scheme × admin → admin and row-binds schemes within each admin level.
>
> **Input — `extrap_dta`:** Named list of expanded+merged results, one per weight scheme. Each element is itself a named list per admin level containing wide tibbles. This is the output of `imap(scored, ~ expand_adm_levels(.x, mt) |> merge_expandedn_adm_levels())`.
>
> **Input — `adm_ids`:** Named list of tibbles from `clean_geoms()`. Must contain a `Name` column per level (used for `spatial_name`). Names are `admin0`, `admin1`, etc.
>
> **Input — `na_rm_pti2`:** Logical or NULL. **Default: `NULL`** (not FALSE). If non-NULL, takes precedence and is used directly. If NULL, falls back to `get_golem_options("na_rm_pti")`; if that is also NULL, defaults to `FALSE`. When `TRUE`, `rowSums` ignores NAs; when `FALSE`, any NA in an indicator column produces `pti_score = NA`.
>
> **Output:** Named list by admin level (not by scheme). Each element is a tibble with columns: `pti_score` (numeric), `pti_name` (character — the scheme name), `spatial_name` (character — human-readable region name), plus Pcod and optionally year columns. Multiple schemes are row-bound within each admin level.

### A.10 `label_generic_pti(dta)`

| #   | Test Case                | Input                       | Expected                                                |
| --- | ------------------------ | --------------------------- | ------------------------------------------------------- |
| 1   | Normal labeling          | Data with scores            | `pti_label` column added with HTML                      |
| 2   | NA pti_score             | Row with NA score           | Label contains "No data"                                |
| 3   | Special characters       | `spatial_name = "Luhans'k"` | Properly handled in output (glue doesn't break)         |
| 4   | HTML structure           | Any                         | Contains `<strong>`, `<br/>` tags                       |
| 5   | Score formatting         | `pti_score = 1.23456789`    | Formatted to 5 decimal places                           |
| 6   | List structure preserved | List of tibbles             | Output is same-length list of tibbles with extra column |

> **What it does:** Generates an HTML label string for each row, combining the region name, weight scheme name, and formatted score.
>
> **Mechanism:** Maps over the list of tibbles. For each row, evaluates a `glue` expression (from `generic_pti_glue()`) that interpolates `spatial_name`, `pti_name`, and `pti_score` into an HTML string with `<strong>` and `<br/>` tags. Scores are formatted to 5 decimal places via `scales::label_number(accuracy = 0.00001)`. NA scores render as the string `"No data"`. NA scheme names also render as `"No data"`.
>
> **Input — `dta`:** Named list of tibbles (one per admin level), as produced by `agg_pti_scores()`. Each tibble must have columns: `spatial_name` (character), `pti_name` (character), `pti_score` (numeric, may be NA).
>
> **Input — `glue_expr`:** Optional. A glue format string. Defaults to `generic_pti_glue()`. Can be overridden for custom label formats.
>
> **Output:** Same list of tibbles with one added column: `pti_label`. **Class is `glue` (inherits from `character`)**, not plain `character`; this matters for `expect_type` (`"character"`) but `expect_s3_class("glue")` distinguishes it. Each value is an HTML string. The list structure, row count, and all existing columns are preserved. The label is not wrapped in `htmltools::HTML()`.

### A.11 `structure_pti_data(dta, shp_dta)`

| #   | Test Case                           | Input                   | Expected                                                       |
| --- | ----------------------------------- | ----------------------- | -------------------------------------------------------------- |
| 1   | Normal structuring                  | Scored data + shapes    | Named list with `$pti_data` (sf), `$pti_codes`, `$admin_level` |
| 2   | Admin unit in shapes but not scores | Extra polygon           | Gets "No data" label                                           |
| 3   | Output is sf                        | Any                     | `$pti_data` inherits "sf"                                      |
| 4   | Wide format                         | Multiple weight schemes | `pti_score..pti_ind_N` columns                                 |
| 5   | pti_codes mapping                   | 2 schemes "A", "B"      | `$pti_codes = c(pti_ind_1 = "A", pti_ind_2 = "B")`             |
| 6   | All regions covered                 | Any                     | `nrow($pti_data) == nrow(shape layer)` (after pivot_wider, one row per Pcod) |
| 7   | Geometry preserved                  | Any                     | `$pti_data` has geometry column from shapes                    |

> **What it does:** Final restructuring — converts the labeled per-admin-level tibbles into the format the mapping/plotting layer expects.
>
> **Mechanism:** For each admin level: assigns sequential `pti_code` identifiers (`pti_ind_1`, `pti_ind_2`, …) to each unique weight scheme name. Joins `pti_code` onto the data, drops `pti_name`. Expands the data to ensure every region × scheme combination exists (using `tidyr::expand`), filling missing combinations with a "No data" label. Then pivots to wide format with `pivot_wider(names_from = pti_code, values_from = c(pti_score, pti_label), names_sep = "..")`, producing columns like `pti_score..pti_ind_1`, `pti_label..pti_ind_1`. Finally, left-joins the geometry back from `shp_dta` by matching on the Pcod column, so the output is an sf object. Regions present in shapes but absent from scored data get their "No data" label from the expand step.
>
> **Input — `dta`:** Named list of tibbles (one per admin level) from `label_generic_pti()`. Each must have: `pti_score`, `pti_label`, `pti_name`, `spatial_name`, and an `admin{N}Pcod` column.
>
> **Input — `shp_dta`:** The original named list of sf objects (same as pipeline input). Used to join geometry and to extract `admin_level` metadata from the list names.
>
> **Output:** Named list, one element per admin level. Each element is a list with three slots:
> - `$pti_data` — sf object (tibble + geometry). Columns include `admin{N}Pcod`, `spatial_name`, `pti_score..pti_ind_N`, `pti_label..pti_ind_N` for each scheme, plus the geometry column inherited from the shape layer (the join is `shape %>% left_join(pti_data2)` so the sf side is the LHS — row count equals the polygon count).
> - `$pti_codes` — Named character vector mapping `pti_ind_N` → scheme name. Built from `unique(.x$pti_name)` so order = first-appearance order in the data, NOT the order of `weights_clean`.
> - `$admin_level` — Named character vector, e.g. `c(admin1 = "Region")`, extracted from the shape list names. **Will be `c(<NA> = NA)` if a shape list element has no underscore in its name.**

---

## Level B: Integration Tests (adjacent pairs)

These verify that the output of step N is valid input for step N+1.

### B.1 `pivot_pti_dta` → `get_weighted_data`

```r
test_that("pivoted data feeds directly into weighting", {
  pivoted <- pivot_pti_dta(ukr_mtdt_full, test_indicators)
  
  result <- get_weighted_data(
    ukr_mtdt_full$weights_clean,
    pivoted,
    test_indicators
  )
  
  # Structure: outer list (schemes) × inner list (admin levels)
  expect_type(result, "list")
  expect_equal(length(result), length(ukr_mtdt_full$weights_clean))
  walk(result, ~ expect_equal(length(.x), length(pivoted)))
  
  # Values are modified (not identical to input)
  walk2(result[[1]], pivoted, ~ {
    if (nrow(.x) > 0 && nrow(.y) > 0) {
      expect_false(identical(.x$value, .y$value))
    }
  })
})
```

### B.2 `get_weighted_data` → `get_scores_data`

```r
test_that("weighted data feeds into scoring without error", {
  scored <- get_scores_data(test_weighted)
  
  # Same structure

  expect_equal(length(scored), length(test_weighted))
  walk(scored, ~ expect_equal(length(.x), length(test_weighted[[1]])))
  
  # Values are z-standardized (mean ≈ 0 per group)
  walk(scored, ~ walk(.x, ~ {
    if (nrow(.x) > 0) {
      group_means <- .x |>
        group_by(var_code) |>
        summarise(m = mean(value, na.rm = TRUE), .groups = "drop")
      expect_true(all(abs(group_means$m) < 1e-10))
    }
  }))
})
```

### B.3 `get_scores_data` → `expand_adm_levels`

```r
test_that("scored data expands across admin levels", {
  # expand_adm_levels expects a single scheme's data (inner list)
  expanded <- expand_adm_levels(test_scored[[1]], test_mt)
  
  # Output is nested: source_level → target_level
  expect_type(expanded, "list")
  expect_equal(length(expanded), length(test_adm_levels))
  walk(expanded, ~ expect_equal(length(.x), length(test_adm_levels)))
})
```

### B.4 `expand_adm_levels` → `merge_expandedn_adm_levels`

```r
test_that("expanded data merges into wide format", {
  expanded <- expand_adm_levels(test_scored[[1]], test_mt)
  merged <- merge_expandedn_adm_levels(expanded)
  
  # Output: one tibble per target admin level
  expect_type(merged, "list")
  expect_equal(length(merged), length(test_adm_levels))
  
  # Each tibble is wide (has columns from multiple sources)
  walk2(merged, names(merged), ~ {
    pcod_col <- str_c(.y, "Pcod")
    expect_true(pcod_col %in% names(.x))
    # More columns than just the Pcod
    expect_gt(ncol(.x), 1)
  })
})
```

### B.5 `merge → agg_pti_scores`

```r
test_that("merged data aggregates into scores", {
  # agg_pti_scores expects the full multi-scheme expanded+merged structure
  expanded_all <- test_scored |>
    imap(~ expand_adm_levels(.x, test_mt) |> merge_expandedn_adm_levels())
  
  agged <- agg_pti_scores(expanded_all, test_clean_geoms)
  
  # Output: list by admin level, with pti_score column
  expect_type(agged, "list")
  walk(agged, ~ {
    expect_true("pti_score" %in% names(.x))
    expect_true("pti_name" %in% names(.x))
    expect_true("spatial_name" %in% names(.x))
  })
})
```

### B.6 `agg_pti_scores` → `label_generic_pti`

```r
test_that("aggregated data receives labels", {
  expanded_all <- test_scored |>
    imap(~ expand_adm_levels(.x, test_mt) |> merge_expandedn_adm_levels())
  agged <- agg_pti_scores(expanded_all, test_clean_geoms)
  labeled <- label_generic_pti(agged)
  
  walk(labeled, ~ {
    expect_true("pti_label" %in% names(.x))
    expect_true(all(nchar(.x$pti_label) > 0))
  })
})
```

### B.7 `label_generic_pti` → `structure_pti_data`

```r
test_that("labeled data structures into final output", {
  expanded_all <- test_scored |>
    imap(~ expand_adm_levels(.x, test_mt) |> merge_expandedn_adm_levels())
  agged <- agg_pti_scores(expanded_all, test_clean_geoms)
  labeled <- label_generic_pti(agged)
  structured <- structure_pti_data(labeled, ukr_shp)
  
  walk(structured, ~ {
    expect_true("pti_data" %in% names(.x))
    expect_true("pti_codes" %in% names(.x))
    expect_true("admin_level" %in% names(.x))
    expect_s3_class(.x$pti_data, "sf")
  })
})
```

---

## Level C: End-to-End Tests

### C.1 Full pipeline — happy path

```r
test_that("run_pti_pipeline produces valid output end-to-end", {
  result <- run_pti_pipeline(
    weights_clean   = ukr_mtdt_full$weights_clean,
    input_dta       = ukr_mtdt_full,
    indicators_list = get_indicators_list(ukr_mtdt_full),
    shp_dta         = ukr_shp
  )
  
  expect_type(result, "list")
  expect_true(length(result) > 0)
  
  walk(result, ~ {
    expect_s3_class(.x$pti_data, "sf")
    expect_true("pti_score..pti_ind_1" %in% names(.x$pti_data) ||
                "pti_score" %in% names(.x$pti_data))
    expect_true(nrow(.x$pti_data) > 0)
  })
})
```

### C.2 All-zero weights

```r
test_that("pipeline handles all-zero weights gracefully", {
  zero_wts <- list(
    zero_scheme = tibble(
      var_code = get_indicators_list(ukr_mtdt_full)$var_code,
      weight = 0
    )
  )
  
  result <- run_pti_pipeline(
    weights_clean   = zero_wts,
    input_dta       = ukr_mtdt_full,
    indicators_list = get_indicators_list(ukr_mtdt_full),
    shp_dta         = ukr_shp
  )
  
  # All scores should be 0
  walk(result, ~ {
    scores <- .x$pti_data$`pti_score..pti_ind_1`
    non_na <- scores[!is.na(scores)]
    expect_true(all(non_na == 0))
  })
})
```

### C.3 Single indicator

```r
test_that("pipeline works with a single indicator", {
  ind_list <- get_indicators_list(ukr_mtdt_full) |> slice(1)
  single_wt <- list(single = tibble(var_code = ind_list$var_code, weight = 1))
  
  result <- run_pti_pipeline(
    weights_clean   = single_wt,
    input_dta       = ukr_mtdt_full,
    indicators_list = ind_list,
    shp_dta         = ukr_shp
  )
  
  expect_type(result, "list")
  walk(result, ~ expect_s3_class(.x$pti_data, "sf"))
})
```

### C.4 NA propagation (na_rm = FALSE)

```r
test_that("NAs propagate when na_rm = FALSE", {
  result <- run_pti_pipeline(
    weights_clean   = ukr_mtdt_full$weights_clean,
    input_dta       = ukr_mtdt_full,
    indicators_list = get_indicators_list(ukr_mtdt_full),
    shp_dta         = ukr_shp,
    na_rm           = FALSE
  )
  
  # At least some NAs should exist (partial data coverage)
  has_na <- map_lgl(result, ~ any(is.na(.x$pti_data$`pti_score..pti_ind_1`)))
  expect_true(any(has_na))
})
```

### C.5 NA removal (na_rm = TRUE)

```r
test_that("NAs are removed when na_rm = TRUE", {
  result <- run_pti_pipeline(
    weights_clean   = ukr_mtdt_full$weights_clean,
    input_dta       = ukr_mtdt_full,
    indicators_list = get_indicators_list(ukr_mtdt_full),
    shp_dta         = ukr_shp,
    na_rm           = TRUE
  )
  
  # pti_score should have no NAs (aside from regions with zero data)
  walk(result, ~ {
    scores <- .x$pti_data$`pti_score..pti_ind_1`
    # Regions with spatial_name but no data are allowed NA
    # Regions that went through calc should not be NA
    labeled_rows <- !is.na(.x$pti_data$`pti_label..pti_ind_1`) &
      !str_detect(.x$pti_data$`pti_label..pti_ind_1`, "No data")
    if (any(labeled_rows)) {
      expect_true(all(!is.na(scores[labeled_rows])))
    }
  })
})
```

### C.6 Multiple weight schemes

```r
test_that("pipeline handles multiple weight schemes", {
  ind_list <- get_indicators_list(ukr_mtdt_full)
  multi_wts <- list(
    scheme_a = tibble(var_code = ind_list$var_code, weight = 1),
    scheme_b = tibble(var_code = ind_list$var_code, weight = c(1, rep(0, nrow(ind_list) - 1))),
    scheme_c = tibble(var_code = ind_list$var_code, weight = rev(seq_len(nrow(ind_list))))
  )
  
  result <- run_pti_pipeline(
    weights_clean   = multi_wts,
    input_dta       = ukr_mtdt_full,
    indicators_list = ind_list,
    shp_dta         = ukr_shp
  )
  
  # Each admin level should have 3 pti_ind columns
  walk(result, ~ {
    score_cols <- names(.x$pti_data) |> str_subset("pti_score\\.\\.pti_ind")
    expect_equal(length(score_cols), 3)
    expect_equal(length(.x$pti_codes), 3)
  })
})
```

### C.7 Idempotency — same input → same output

```r
test_that("pipeline is deterministic", {
  args <- list(
    weights_clean   = ukr_mtdt_full$weights_clean,
    input_dta       = ukr_mtdt_full,
    indicators_list = get_indicators_list(ukr_mtdt_full),
    shp_dta         = ukr_shp
  )
  
  r1 <- do.call(run_pti_pipeline, args)
  r2 <- do.call(run_pti_pipeline, args)
  
  walk2(r1, r2, ~ {
    expect_equal(
      sf::st_drop_geometry(.x$pti_data),
      sf::st_drop_geometry(.y$pti_data)
    )
  })
})
```

### C.8 Performance baseline

```r
test_that("pipeline completes in reasonable time", {
  skip_on_cran()
  
  elapsed <- system.time({
    run_pti_pipeline(
      weights_clean   = ukr_mtdt_full$weights_clean,
      input_dta       = ukr_mtdt_full,
      indicators_list = get_indicators_list(ukr_mtdt_full),
      shp_dta         = ukr_shp
    )
  })["elapsed"]
  
  # Should complete in under 30 seconds on any machine

  expect_lt(elapsed, 30)
})
```

---

## Edge Case Tests (cross-cutting)

### E.1 Single admin level (no expansion needed)

```r
test_that("pipeline works with single admin level", {
  # Use only admin2 shapes
  single_shp <- ukr_shp[str_detect(names(ukr_shp), "admin2")]
  
  # Need matching input_dta with only admin2 sheet
  single_dta <- ukr_mtdt_full[str_detect(names(ukr_mtdt_full), "admin2|metadata|weights")]
  
  ind_list <- get_indicators_list(single_dta)
  
  if (nrow(ind_list) > 0) {
    result <- run_pti_pipeline(
      weights_clean   = single_dta$weights_clean,
      input_dta       = single_dta,
      indicators_list = ind_list,
      shp_dta         = single_shp
    )
    expect_type(result, "list")
  }
})
```

### E.2 Indicator available at only one admin level

```r
test_that("indicator at one level expands to others", {
  ind_list <- get_indicators_list(ukr_mtdt_full)
  # Pick indicator only at admin1
  admin1_only <- ind_list |>
    filter(map_lgl(admin_levels_years, ~ nrow(.x) == 1))
  
  if (nrow(admin1_only) > 0) {
    single_wt <- list(test = tibble(
      var_code = admin1_only$var_code[1],
      weight = 1
    ))
    
    result <- run_pti_pipeline(
      weights_clean   = single_wt,
      input_dta       = ukr_mtdt_full,
      indicators_list = admin1_only[1, ],
      shp_dta         = ukr_shp
    )
    
    # Should still produce output for all admin levels
    expect_true(length(result) > 0)
  }
})
```

### E.3 Negative weights (policy: allowable)

```r
test_that("negative weights invert scores", {
  ind_list <- get_indicators_list(ukr_mtdt_full)
  
  pos_wt <- list(pos = tibble(var_code = ind_list$var_code[1], weight = 1))
  neg_wt <- list(neg = tibble(var_code = ind_list$var_code[1], weight = -1))
  
  r_pos <- run_pti_pipeline(pos_wt, ukr_mtdt_full, ind_list[1,], ukr_shp)
  r_neg <- run_pti_pipeline(neg_wt, ukr_mtdt_full, ind_list[1,], ukr_shp)
  
  # Scores should be negated
  walk2(r_pos, r_neg, ~ {
    s1 <- sf::st_drop_geometry(.x$pti_data)$`pti_score..pti_ind_1`
    s2 <- sf::st_drop_geometry(.y$pti_data)$`pti_score..pti_ind_1`
    valid <- !is.na(s1) & !is.na(s2)
    if (any(valid)) {
      expect_equal(s1[valid], -s2[valid], tolerance = 1e-10)
    }
  })
})
```

### E.4 Very large weight values

```r
test_that("large weights don't cause overflow", {
  ind_list <- get_indicators_list(ukr_mtdt_full)
  big_wt <- list(big = tibble(var_code = ind_list$var_code[1], weight = 1e6))
  
  result <- run_pti_pipeline(big_wt, ukr_mtdt_full, ind_list[1,], ukr_shp)
  
  walk(result, ~ {
    scores <- .x$pti_data$`pti_score..pti_ind_1`
    expect_true(all(is.finite(scores[!is.na(scores)])))
  })
})
```

---

## Test Data Setup

### Principle: Purpose-Built Fixtures over Production Data

The bundled `ukr_shp` / `ukr_mtdt_full` datasets are **real-world data** with 3 admin levels (~600 admin2 units, 25 oblasts, 9 indicators). They're useful for happy-path tests but too complex and too "clean" for edge-case testing — too many rows hide subtle bugs, and every level has data.

The strategy: **create small, synthetic fixtures** where every row is hand-verifiable and specific edge cases are baked in.

### Fixture Inventory

All fixtures live in `tests/testthat/fixtures/`. They are `.rds` files generated once by a helper script and committed to version control.

| Fixture File           | Description                                                                                                                                                                                              | Admin Levels | Indicators | Rows (finest) | Purpose                                                                       |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :----------: | :--------: | :-----------: | ----------------------------------------------------------------------------- |
| `fx_shp_3lvl.rds`      | **Tiny 3-level shapes.** 1 country, 2 regions, 4 districts. Simple polygons (squares).                                                                                                                   |   0, 1, 2    |     —      |       4       | Default for most tests. Small enough to verify every row by hand.             |
| `fx_shp_1lvl.rds`      | **Single admin level.** Only admin1 with 3 regions.                                                                                                                                                      |      1       |     —      |       3       | Tests `expand_adm_levels` when no cross-level logic is needed.                |
| `fx_shp_4lvl.rds`      | **Deep hierarchy.** admin0 → admin1 → admin2 → admin3 (2 units each).                                                                                                                                    |  0, 1, 2, 3  |     —      |       8       | Tests multi-hop expansion and aggregation.                                    |
| `fx_mtdt_full.rds`     | **Matching metadata for `fx_shp_3lvl`.** 3 indicators: `ind_a` (at admin1+2), `ind_b` (admin2 only), `ind_c` (admin1 only, with NAs). Includes `metadata` sheet, `weights_clean`, and admin data sheets. |   0, 1, 2    |     3      |       4       | Default data for most pipeline tests.                                         |
| `fx_mtdt_1lvl.rds`     | **Metadata for `fx_shp_1lvl`.** 2 indicators, both at admin1.                                                                                                                                            |      1       |     2      |       3       | Single-level tests.                                                           |
| `fx_mtdt_allna.rds`    | **Metadata where all indicator values are NA** at admin2 (admin1 has valid data).                                                                                                                        |   0, 1, 2    |     2      |       4       | Tests NA propagation through the pipeline.                                    |
| `fx_weights_zero.rds`  | **All weights = 0** for all indicators in `fx_mtdt_full`.                                                                                                                                                |      —       |     3      |       —       | Tests zero-weight path.                                                       |
| `fx_weights_mixed.rds` | **Mixed weights:** `ind_a = 1`, `ind_b = 0`, `ind_c = -0.5`.                                                                                                                                             |      —       |     3      |       —       | Tests filtering, negative weights, fractional weights in one fixture.         |
| `fx_weights_multi.rds` | **3 weight schemes** with different configurations.                                                                                                                                                      |      —       |     3      |       —       | Tests multi-scheme pipeline and `structure_pti_data` wide output.             |
| `fx_shp_orphan.rds`    | **Shapes with mismatched Pcods.** admin2 has a district whose parent doesn't exist in admin1.                                                                                                            |   0, 1, 2    |     —      | 5 (1 orphan)  | Tests `get_mt()` orphan filtering and `expand_adm_levels` with missing links. |

### Fixture Generator Script

Create `tests/testthat/fixtures/generate-fixtures.R`:

```r
# ============================================================
# Run this script ONCE to (re)generate all test fixtures.
# Commit the resulting .rds files to version control.
# ============================================================
library(sf)
library(tibble)
library(dplyr)

fixture_dir <- "tests/testthat/fixtures"
if (!dir.exists(fixture_dir)) dir.create(fixture_dir, recursive = TRUE)

# --- fx_shp_3lvl: tiny 3-level shapes -------------------------
make_box <- function(xmin, ymin, xmax, ymax) {
  st_polygon(list(matrix(c(
    xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin
  ), ncol = 2, byrow = TRUE)))
}

admin0 <- st_sf(
  admin0Pcod = "C0",
  admin0Name = "Country",
  geometry = st_sfc(make_box(0, 0, 10, 10), crs = 4326)
)

admin1 <- st_sf(
  admin0Pcod = c("C0", "C0"),
  admin1Pcod = c("R1", "R2"),
  admin1Name = c("Region North", "Region South"),
  geometry = st_sfc(
    make_box(0, 5, 10, 10),
    make_box(0, 0, 10, 5),
    crs = 4326
  )
)

admin2 <- st_sf(
  admin0Pcod = rep("C0", 4),
  admin1Pcod = c("R1", "R1", "R2", "R2"),
  admin2Pcod = c("D1", "D2", "D3", "D4"),
  admin2Name = c("District NW", "District NE", "District SW", "District SE"),
  geometry = st_sfc(
    make_box(0, 5, 5, 10), make_box(5, 5, 10, 10),
    make_box(0, 0, 5, 5),  make_box(5, 0, 10, 5),
    crs = 4326
  )
)

fx_shp_3lvl <- list(
  admin0_Country = admin0,
  admin1_Region  = admin1,
  admin2_District = admin2
)
saveRDS(fx_shp_3lvl, file.path(fixture_dir, "fx_shp_3lvl.rds"))

# --- fx_shp_1lvl: single admin level -------------------------
fx_shp_1lvl <- list(
  admin1_Region = st_sf(
    admin1Pcod = c("A", "B", "C"),
    admin1Name = c("Area A", "Area B", "Area C"),
    geometry = st_sfc(
      make_box(0, 0, 3, 10), make_box(3, 0, 7, 10), make_box(7, 0, 10, 10),
      crs = 4326
    )
  )
)
saveRDS(fx_shp_1lvl, file.path(fixture_dir, "fx_shp_1lvl.rds"))

# --- fx_shp_4lvl: deep hierarchy -----------------------------
admin3 <- st_sf(
  admin0Pcod = rep("C0", 8),
  admin1Pcod = rep(c("R1", "R2"), each = 4),
  admin2Pcod = rep(c("D1", "D2", "D3", "D4"), each = 2),
  admin3Pcod = paste0("S", 1:8),
  admin3Name = paste0("Sub ", 1:8),
  geometry = st_sfc(lapply(1:8, function(i) {
    x0 <- ((i - 1) %% 4) * 2.5
    y0 <- ifelse(i <= 4, 5, 0)
    make_box(x0, y0, x0 + 2.5, y0 + 5)
  }), crs = 4326)
)

fx_shp_4lvl <- list(
  admin0_Country  = admin0,
  admin1_Region   = admin1,
  admin2_District = admin2,
  admin3_Sub      = admin3
)
saveRDS(fx_shp_4lvl, file.path(fixture_dir, "fx_shp_4lvl.rds"))

# --- fx_shp_orphan: mismatched Pcods --------------------------
admin2_orphan <- admin2
admin2_orphan <- bind_rows(admin2_orphan, st_sf(
  admin0Pcod = "C0",
  admin1Pcod = "R_MISSING",  # parent doesn't exist in admin1
  admin2Pcod = "D5",
  admin2Name = "Orphan District",
  geometry = st_sfc(make_box(10, 0, 12, 5), crs = 4326)
))
fx_shp_orphan <- list(
  admin0_Country  = admin0,
  admin1_Region   = admin1,
  admin2_District = admin2_orphan
)
saveRDS(fx_shp_orphan, file.path(fixture_dir, "fx_shp_orphan.rds"))

# --- fx_mtdt_full: matching metadata for fx_shp_3lvl ----------
# Metadata sheet
metadata <- tibble(
  var_code = c("ind_a", "ind_b", "ind_c"),
  var_name = c("Indicator A", "Indicator B", "Indicator C"),
  var_order = 1:3,
  pillar_name = c("Econ", "Econ", "Social"),
  pillar_group = c(1, 1, 2),
  pillar_description = c("Economic", "Economic", "Social"),
  fltr_exclude_pti = c(FALSE, FALSE, FALSE),
  fltr_exclude_explorer = c(FALSE, FALSE, FALSE)
)

# Admin data sheets
# ind_a: at admin1 and admin2
# ind_b: admin2 only
# ind_c: admin1 only, with one NA
# NOTE: `year` is REQUIRED by expand_adm_levels (`all_of(c("year","var_code","value"))`).
# Without it the pipeline errors. Always include it in fixtures.
admin1_data <- tibble(
  admin0Pcod = c("C0", "C0"),
  admin1Pcod = c("R1", "R2"),
  admin1Name = c("Region North", "Region South"),
  year       = 2024L,
  ind_a = c(10, 20),
  ind_c = c(5, NA)   # <-- NA edge case
)

admin2_data <- tibble(
  admin0Pcod = rep("C0", 4),
  admin1Pcod = c("R1", "R1", "R2", "R2"),
  admin2Pcod = c("D1", "D2", "D3", "D4"),
  admin2Name = c("District NW", "District NE", "District SW", "District SE"),
  year       = 2024L,
  ind_a = c(11, 12, 21, 22),
  ind_b = c(100, 200, 300, 400)
)

weights_clean <- list(
  default = tibble(
    var_code = c("ind_a", "ind_b", "ind_c"),
    weight   = c(1, 1, 1)
  )
)

fx_mtdt_full <- list(
  metadata       = metadata,
  admin1_Region  = admin1_data,
  admin2_District = admin2_data,
  weights_clean  = weights_clean
)
saveRDS(fx_mtdt_full, file.path(fixture_dir, "fx_mtdt_full.rds"))

# --- fx_mtdt_1lvl: metadata for fx_shp_1lvl ------------------
fx_mtdt_1lvl <- list(
  metadata = tibble(
    var_code = c("ind_x", "ind_y"),
    var_name = c("Indicator X", "Indicator Y"),
    var_order = 1:2,
    pillar_name = c("P1", "P1"),
    pillar_group = c(1, 1),
    pillar_description = c("Pillar", "Pillar"),
    fltr_exclude_pti = c(FALSE, FALSE),
    fltr_exclude_explorer = c(FALSE, FALSE)
  ),
  admin1_Region = tibble(
    admin1Pcod = c("A", "B", "C"),
    admin1Name = c("Area A", "Area B", "Area C"),
    year       = 2024L,
    ind_x = c(1, 2, 3),
    ind_y = c(10, 20, 30)
  ),
  weights_clean = list(
    equal = tibble(var_code = c("ind_x", "ind_y"), weight = c(1, 1))
  )
)
saveRDS(fx_mtdt_1lvl, file.path(fixture_dir, "fx_mtdt_1lvl.rds"))

# --- fx_mtdt_allna: all NA at admin2 --------------------------
fx_mtdt_allna <- fx_mtdt_full
fx_mtdt_allna$admin2_District$ind_a <- NA_real_
fx_mtdt_allna$admin2_District$ind_b <- NA_real_
saveRDS(fx_mtdt_allna, file.path(fixture_dir, "fx_mtdt_allna.rds"))

# --- Weight fixtures ------------------------------------------
fx_weights_zero <- list(
  zero = tibble(var_code = c("ind_a", "ind_b", "ind_c"), weight = c(0, 0, 0))
)
saveRDS(fx_weights_zero, file.path(fixture_dir, "fx_weights_zero.rds"))

fx_weights_mixed <- list(
  mixed = tibble(var_code = c("ind_a", "ind_b", "ind_c"), weight = c(1, 0, -0.5))
)
saveRDS(fx_weights_mixed, file.path(fixture_dir, "fx_weights_mixed.rds"))

fx_weights_multi <- list(
  all_equal    = tibble(var_code = c("ind_a", "ind_b", "ind_c"), weight = c(1, 1, 1)),
  a_only       = tibble(var_code = c("ind_a", "ind_b", "ind_c"), weight = c(1, 0, 0)),
  weighted_mix = tibble(var_code = c("ind_a", "ind_b", "ind_c"), weight = c(2, 1, 0.5))
)
saveRDS(fx_weights_multi, file.path(fixture_dir, "fx_weights_multi.rds"))

cat("All fixtures generated in", fixture_dir, "\n")
```

### Why These Fixtures Are Better Than `ukr_shp` / `ukr_mtdt_full` for Testing

| Problem with bundled data                        | How fixtures solve it                                                                         |
| ------------------------------------------------ | --------------------------------------------------------------------------------------------- |
| 600+ admin2 units — can't verify individual rows | 4 districts — every row is checkable by hand                                                  |
| All 3 levels always have data                    | `fx_mtdt_1lvl` has only 1 level; `ind_c` is admin1-only; `fx_mtdt_allna` has all-NA at admin2 |
| No orphan Pcods                                  | `fx_shp_orphan` has a district with a non-existent parent                                     |
| Only 3 admin levels                              | `fx_shp_4lvl` tests deep hierarchies; `fx_shp_1lvl` tests single level                        |
| Weights are always sensible                      | `fx_weights_zero`, `fx_weights_mixed` (with negative), `fx_weights_multi`                     |
| Z-score verification impossible                  | With `ind_x = [1, 2, 3]` — expected z-scores are exactly `[-1, 0, 1]`                         |
| Hard to trace expansion logic                    | 2 regions × 2 districts each — can verify upward/downward by hand                             |

### Shared Helper: `tests/testthat/helper-test-data.R`

```r
# Loaded automatically by testthat before each test file

# ── Bundled data (for happy-path / real-world tests) ──────────
data("ukr_shp", package = "devPTIpack")
data("ukr_mtdt_full", package = "devPTIpack")

# ── Synthetic fixtures (for edge-case tests) ──────────────────
fx_shp_3lvl     <- readRDS(test_path("fixtures", "fx_shp_3lvl.rds"))
fx_shp_1lvl     <- readRDS(test_path("fixtures", "fx_shp_1lvl.rds"))
fx_shp_4lvl     <- readRDS(test_path("fixtures", "fx_shp_4lvl.rds"))
fx_shp_orphan   <- readRDS(test_path("fixtures", "fx_shp_orphan.rds"))
fx_mtdt_full    <- readRDS(test_path("fixtures", "fx_mtdt_full.rds"))
fx_mtdt_1lvl    <- readRDS(test_path("fixtures", "fx_mtdt_1lvl.rds"))
fx_mtdt_allna   <- readRDS(test_path("fixtures", "fx_mtdt_allna.rds"))
fx_wt_zero      <- readRDS(test_path("fixtures", "fx_weights_zero.rds"))
fx_wt_mixed     <- readRDS(test_path("fixtures", "fx_weights_mixed.rds"))
fx_wt_multi     <- readRDS(test_path("fixtures", "fx_weights_multi.rds"))

# ── Derived objects from bundled data (used by integration/e2e tests) ──
test_indicators  <- get_indicators_list(ukr_mtdt_full)
test_mt          <- get_mt(ukr_shp)
test_adm_levels  <- get_adm_levels(test_mt)
test_clean_geoms <- clean_geoms(ukr_shp)
test_pivoted     <- pivot_pti_dta(ukr_mtdt_full, test_indicators)
test_weighted    <- get_weighted_data(
  ukr_mtdt_full$weights_clean,
  test_pivoted,
  test_indicators
)
test_scored      <- get_scores_data(test_weighted)

# ── Derived objects from fixtures (used by unit tests) ────────
fx_indicators    <- get_indicators_list(fx_mtdt_full)
fx_mt            <- get_mt(fx_shp_3lvl)
fx_adm_levels    <- get_adm_levels(fx_mt)
fx_clean_geoms   <- clean_geoms(fx_shp_3lvl)
fx_pivoted       <- pivot_pti_dta(fx_mtdt_full, fx_indicators)
fx_weighted      <- get_weighted_data(
  fx_mtdt_full$weights_clean,
  fx_pivoted,
  fx_indicators
)
fx_scored        <- get_scores_data(fx_weighted)
```

### Which Tests Use Which Data

| Test Level           | Primary Data                                | Why                                                |
| -------------------- | ------------------------------------------- | -------------------------------------------------- |
| A. Unit tests        | `fx_*` fixtures                             | Small, deterministic, hand-verifiable              |
| B. Integration tests | Mix of `fx_*` and `ukr_*`                   | Verify contracts with both synthetic and real data |
| C. End-to-end tests  | `ukr_*` bundled data                        | Proves pipeline works on real-world data           |
| E. Edge case tests   | `fx_shp_orphan`, `fx_mtdt_allna`, `fx_wt_*` | Purpose-built for specific failure modes           |

---

## Test File: `tests/testthat/test-calc-pipeline.R`

Organized by level:

```r
# --- Level A: Unit tests ---
# test_that("get_mt: normal 3-level shapes", { ... })
# test_that("get_mt: single admin level", { ... })
# ...

# --- Level B: Integration tests ---
# test_that("pivoted data feeds into weighting", { ... })
# test_that("weighted data feeds into scoring", { ... })
# ...

# --- Level C: End-to-end tests ---
# test_that("run_pti_pipeline produces valid output", { ... })
# test_that("pipeline handles all-zero weights", { ... })
# ...

# --- Edge cases ---
# test_that("negative weights invert scores", { ... })
# ...
```

---

## Critical Assessment: What's Actually Testable Today

Reading the source against this plan, the testing capability of each function is constrained by both **missing scaffolding** and **latent ambiguity in the implementation**. Below is a function-by-function honest read of what is testable now, what would require new helpers, and what behavior the tests would actually pin down (vs. what they would mask).

### Cross-cutting issues that affect every level

1. **`get_golem_options("na_rm_pti")` global state.** `agg_pti_scores` reads it whenever `na_rm_pti2` is `NULL`. Any test that doesn't pass `na_rm_pti2` is implicitly coupled to the test runner's golem options. Either always pass `na_rm_pti2` explicitly in tests, or wrap each test in `withr::with_options(list(golem.app.prod = …))` plus a `golem::with_golem_options()` shim. Without this, CI behavior diverges from local.
2. **Numeric-vs-lexicographic admin ordering.** `get_adm_levels` sorts strings; `expand_adm_levels` and `agg_pti_scores` extract only the first digit (`str_extract("\\d")`). The pipeline is silently incorrect for `admin10+`. A `fx_shp_doubledigit.rds` (e.g. `admin0..admin12`) would expose this — but the test must decide whether to **pin current (buggy) behavior** or **assert correct numeric ordering** (which currently fails). Recommend: add a `test_that` marked `skip("documents known issue: lexicographic admin sort")` so the failure is recorded without breaking CI.
3. **Element-name matching is `str_detect`, not equality.** `pivot_pti_dta` (`str_detect("admin")`) and `expand_adm_levels` (`str_detect(names, adm_from)`) both use substring matching. Two test cases worth adding: an element named `"administrative_summary"` (incorrectly picked up by `pivot_pti_dta`), and a list with both `admin1_a` and `admin1_b` (kills the `expand_adm_levels` branch via the `length == 1` guard).
4. **`weights_clean` is a list, not a tibble.** Several proposed tests pass `tibble(...)` directly where the pipeline expects `list(scheme = tibble(...))`. Production usage in `mod_calc_pti2.R` and `validate_metadata.R` confirms the list-of-tibbles structure. The fixtures are correct here; some prose around them is loose.
5. **`run_pti_pipeline` does not exist yet.** The orchestrator is **proposed**, not real. All Level C and Edge tests depend on it. Either commit the orchestrator as a first PR, or rewrite the e2e tests to inline the same pipe (which mirrors `mod_calc_pti2.R` lines 57–76 verbatim). Until then those tests cannot run.

### Per-function testability assessment

| Function | What's testable now (no new code) | What requires new helpers | Latent gotchas a good test must cover |
| --- | --- | --- | --- |
| `get_mt` | All 9 cases | none | Behavior on `list()` is **error**, not "0-row tibble". Element names ignored. Reduce no-ops on length-1 list. |
| `get_adm_levels` | Single-digit cases | `fx_doubledigit` fixture | Lexicographic sort failure for ≥10 levels. NA dropping by `sort` default. |
| `clean_geoms` | All cases | none | Names without `_` become NA — test it. |
| `pivot_pti_dta` | Mostly testable | Fixture with `agg_*` columns; fixture with non-`admin\d` element name like `administrative` | `agg_*` columns are silently retained; `year` is "optional" here but required downstream. |
| `get_weighted_data` | All 10 cases | none | `select(-contains("weight"))` is broader than dropping just `weight` — any `weighted_*` column gets dropped too. |
| `get_scores_data` | Most cases | none | 1-row group → `value = NA`, NOT 0 (test currently asserts the wrong thing). `na_rm = TRUE` means a 1-row-with-NA group also yields NA. |
| `expand_adm_levels` | Single-scheme paths | A fixture exercising the `>1 element matches` branch; one missing `year` to confirm it errors | The `length == 1 && nrow > 0` guard silently nullifies whole branches; tests must assert NULL outputs explicitly, not just "no error". |
| `merge_expandedn_adm_levels` | All cases | none | Removes ALL of `x`'s non-key columns from `y`, not only collisions — affects column-count assertions. |
| `agg_pti_scores` | Most cases | A test that passes `na_rm_pti2` explicitly to bypass golem | Default arg is `NULL`, not `FALSE`. Final `filter_at` drops rows with NA Pcod/year — easy to overlook. |
| `label_generic_pti` | All cases | none | Output column is `glue` class, not character — `expect_type(.x$pti_label, "character")` succeeds (glue inherits) but `class()` differs. |
| `structure_pti_data` | All cases | none | `pti_codes` order = first-appearance in scored data, not order of `weights_clean`. Multi-scheme name collisions (two schemes share a name) collapse to one `pti_ind_N`. |

### Recommended test additions (beyond what the doc currently lists)

These fall into three buckets:

**(a) Pin known-issue behavior** — failing or skipped tests that document the bug:

```r
test_that("KNOWN: get_adm_levels sorts lexicographically, not numerically", {
  dta <- tibble(admin1Pcod = 1, admin2Pcod = 1, admin12Pcod = 1)
  result <- get_adm_levels(dta)
  # Currently: c("admin1","admin12","admin2"). Numeric correct: c("admin1","admin2","admin12").
  expect_equal(unname(result), c("admin1", "admin12", "admin2"))
})

test_that("KNOWN: get_scores_data leaves 1-row groups as NA, not 0", {
  one_row <- list(s = list(a = tibble(var_code = "x", value = 5, year = 2024)))
  out <- get_scores_data(one_row)
  expect_true(is.na(out$s$a$value))
})
```

**(b) Boundary tests not yet enumerated:**

| Function | Missing test | Why |
| --- | --- | --- |
| `pivot_pti_dta` | Element named `"administrative_x"` is picked up | Documents the `str_detect("admin")` over-match |
| `pivot_pti_dta` | Sheet contains `agg_population`; survives to output | Documents undocumented column retention |
| `expand_adm_levels` | `wtd_scrd_dta` has both `admin1_a` and `admin1_b` | Both present → `length == 1` fails → all NULLs |
| `expand_adm_levels` | Tibble missing `year` column | `all_of()` errors — proves year is *required* downstream |
| `agg_pti_scores` | Pass `na_rm_pti2 = NULL` and set `golem` options | Verifies precedence ladder |
| `agg_pti_scores` | A native column `ind_a` collides with foreign `ind_a._._.admin1` | Verifies foreign is dropped, native counted once |
| `structure_pti_data` | Two schemes named `"x"` (collision) | Verifies behavior when scheme names are not unique |
| `structure_pti_data` | Shape layer with no `_` in name | Verifies `admin_level` slot becomes `c(NA = NA)` |

**(c) Property-based tests** (one block, using `hedgehog` or hand-rolled generators):

```r
test_that("PROPERTY: pti_score equals manual rowSums for any weight vector", {
  for (i in 1:20) {
    n_inds <- sample(1:5, 1)
    wts <- runif(n_inds, -2, 2)
    # ... run pipeline, recompute score by hand from raw indicators
    # ... expect equal within tolerance
  }
})

test_that("PROPERTY: scaling all weights by k scales all pti_scores by k (after re-z-scoring)", {
  # Note: z-scoring is invariant to positive scaling of inputs, so output should be IDENTICAL
  # for k > 0, NEGATED for k < 0. This catches any drift in score normalization.
})
```

### Recommended scaffolding to commit before tests

1. **`run_pti_pipeline()` orchestrator** in `R/calc_pti_helpers.R` — copy-paste from `mod_calc_pti2.R:57–76`. This unblocks Level C entirely.
2. **`tests/testthat/helper-pipeline-options.R`** — wraps any `agg_pti_scores` call in `local_options(golem.app.prod = NULL)` plus a deterministic `na_rm_pti` setter. Without this, tests are flaky between local and CI.
3. **`tests/testthat/fixtures/generate-fixtures.R` ↔ `.github/workflows/regen-fixtures.yml`** — ensure fixtures regenerate cleanly and are not silently stale. Add a `digest::digest()` check in a startup test that fails loudly when fixture content drifts from the generator script.
4. **A snapshot test for `agg_pti_scores`** (`testthat::expect_snapshot_value(...)`) on a small fixture — locks the *exact* numeric output, surfacing any pipeline regression even if all unit tests pass.

### Tier-2 (out of scope here, but worth noting)

The proposed plan stops at the calculation pipeline. Two adjacent tests would dramatically increase coverage with minimal work:

- **`get_indicators_list()` (in `R/dta_cleaners.R`)** — feeds the entire pipeline. If this returns the wrong rows, every downstream test still "passes" against incorrect input. Worth a dedicated unit test file even though it's not part of `calc_pti_*.R`.
- **`validate_metadata()` (in `R/fct_validate_metadata.R`)** — already a smoke test for the entire pipeline; converting it from `testthat::test_that` (which currently runs at function-call time, not in the test suite) into a proper `testthat` file that points at the bundled `ukr_*` data would give a free e2e regression test.

---

## Summary: Total Test Cases

| Level          | Count                         | Focus                     |
| -------------- | ----------------------------- | ------------------------- |
| A. Unit        | ~52 cases across 11 functions | Contracts, edge cases     |
| B. Integration | 7 pair tests                  | Data flow correctness     |
| C. End-to-end  | 8 scenario tests              | Business logic validation |
| E. Edge cases  | 4+ specialized                | Boundary conditions       |
| **Total**      | **~71**                       |                           |

All runnable in console with `testthat::test_file("tests/testthat/test-calc-pipeline.R")`.
