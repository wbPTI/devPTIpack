# arch-10 — Step 1 Shapefiles: Enhancement, `make_hex_grid()`, and `make_admin_lookup()`

> **Status:** Planning. Owner: EBukin (with Claude).
>
> **Trigger:** Cross-reading `vignettes/articles/build-pti-1-shapefiles.qmd` against
> `vignettes/articles/dataprep.qmd` revealed documentation gaps and missing
> reference to two functions — `make_hex_grid()` and `make_admin_lookup()` —
> that do not yet exist but are required for a self-consistent, developer-facing
> Step 1 workflow.
>
> **Scope:** three interlocking work items:
> 1. Fix existing documentation gaps in Step 1 (factual errors, missing rules).
> 2. Specify and implement `make_hex_grid()` — fast H3 grid construction.
> 3. Specify and implement `make_admin_lookup()` — validated parent-child cascade builder.
> 4. Propagate `shapes.rds` as the canonical source into Steps 2–5.

---

## 1. Documentation gaps in `build-pti-1-shapefiles.qmd`

These are factual errors or omissions relative to `dataprep.qmd` that can cause
hard-to-debug runtime failures.

### 1.1 `area` computed in m² not km² (high impact)

**Problem:** §C and §E both compute:

```r
area = as.numeric(st_area(geometry))
```

`sf::st_area()` returns m² (with a `units` attribute dropped by `as.numeric()`).
The requirements table and reference §1.5 both specify km². The app legend will
show values 1,000,000× too large.

**Fix:** replace both `mutate()` examples with:

```r
area = as.numeric(units::set_units(sf::st_area(geometry), "km^2"))
```

Note: the project standard CRS is **EPSG:4326**. Compute `area` directly in
EPSG:4326 using `sf::st_area()` with s2 enabled — no UTM reprojection step.
The 1–5% approximation error is acceptable for PTI applications.

### 1.2 `admin0_Country` is mandatory but the simple example omits it (high impact)

**Problem:** `dataprep.qmd` §1.2 states `admin0_Country` is mandatory even if
never displayed in the UI. Step 1's §C–§D simple example builds `my_shp` with
only `admin1_Province`. The country polygon appears silently in §E without
explanation.

**Fix:** add a callout in §A stating the rule. Update the §C–§D simple example
to include `admin0_Country` in `my_shp`.

### 1.3 `saveRDS()` missing `compress = "gz"` (low impact)

**Problem:** §D save snippet omits `compress = "gz"` (reference §1.6 specifies it).

**Fix:** `saveRDS(my_shp, "app-data/shapes.rds", compress = "gz")`.

### 1.4 Non-contiguous level numbers not documented (medium impact)

**Problem:** `dataprep.qmd` §1.1 notes that level numbers need not be contiguous
(e.g. 0, 1, 2, 9). Step 1 never states this. A deployer adding `admin9_Hexagon`
alongside `admin2_District` may assume they are violating a convention.

**Fix:** add a one-sentence note in §A or §E.

### 1.5 `admin<N>Name` uniqueness and no-NA rules missing (medium impact)

**Problem:** The requirements table in §A says Pcod must be unique but says
nothing about Name uniqueness or the NA rule. Both are checked by
`validate_geometries()` (reference §1.7).

**Fix:** add both rules to the requirements table.

### 1.6 No spaces or colons in `<HumanName>` slot not documented (medium impact)

**Problem:** `dataprep.qmd` §1.2 specifies one word, no spaces, no colons in
the `admin<N>_<HumanName>` list slot name. Step 1 never states this. `admin1_East
Region` or `admin2_Sub:District` will silently fail to parse.

**Fix:** add the constraint to the naming note in §A.

### 1.7 `validate_geometries()` blind spots not mentioned (medium impact)

**Problem:** Step 1 tells users to run `validate_geometries()` and trust a
"pass", but never states what the validator does *not* check (reference §1.8):
CRS inconsistency between layers, whether `area` is m² vs km², topological
validity, coverage gaps.

**Fix:** add a short note after the `validate_geometries()` call, or a
collapsible block, listing the main blind spots and suggesting
`sf::st_is_valid()` separately.

---

## 2. New function: `make_hex_grid()`

### 2.1 Purpose

Create an H3 hexagonal grid covering a country boundary and return it as a
standard `sf` tibble conforming to the `admin9_Hexagon` layer contract. The
layer passes `validate_geometries()` without exceptions.

### 2.2 Signature

```r
make_hex_grid(country_polygon, resolution = 6)
```

| Argument          | Type        | Default | Description                                                                                                                                                   |
| ----------------- | ----------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `country_polygon` | `sf` object | —       | Any `sf` object. If multi-row, all rows are unioned with `sf::st_union()` before proceeding. The user may pass `admin1_Province` instead of `admin0_Country`. |
| `resolution`      | integer     | `6`     | H3 resolution for fine grid. Acceptable values: 5, 6, 7. Coarse pre-filter resolution = `resolution - 2` (derived internally, not exposed).                   |

**`resolution` should always be supplied from `HEX_RESOLUTION` in `00-master.R`** — that is the single place a deployer controls what resolution ships with the app. The template Step 1 (`01-shapes.qmd`) reads it as:

```r
my_shp$admin9_Hexagon <- make_hex_grid(my_shp$admin0_Country, resolution = HEX_RESOLUTION)
```

### 2.3 Algorithm

Three steps — only two involve spatial operations:

1. **Coarse spatial filter** (spatial): find all H3 cells at `resolution - 2`
   that intersect the country polygon. Operates on a small number of large cells.

2. **H3 child expansion** (pure H3 math, no spatial operations): expand each
   coarse cell to all of its `resolution` children deterministically via the H3
   hierarchy. No spatial computation.

3. **Centroid filter** (spatial): compute centroids of all fine H3 cells from
   step 2, retain only those whose centroid falls within the country polygon.
   Point-in-polygon is fast even at tens of thousands of points.

This avoids polygon-polygon intersection at fine resolution entirely.

### 2.4 s2 fallback

Both spatial steps (1 and 3) are wrapped in an s2 fallback:

```r
s2_state <- sf::sf_use_s2()
on.exit(sf::sf_use_s2(s2_state), add = TRUE)

result <- tryCatch(
  <spatial_operation_with_s2_enabled>,
  error = function(e) {
    sf::sf_use_s2(FALSE)
    <spatial_operation_with_s2_disabled>
  }
)
```

`on.exit()` guarantees s2 state is restored even if the retry fails.

### 2.5 Output columns

The returned `sf` tibble must satisfy the column contract for `admin9_Hexagon`:

| Column       | Type        | Value                                                                                                                                                               |
| ------------ | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `admin0Pcod` | character   | Inherited from `country_polygon`'s `admin0Pcod` column, or `NA` if absent (warn).                                                                                   |
| `admin9Pcod` | character   | H3 index string at `resolution` (e.g. `"862a1072fffffff"`). Globally unique.                                                                                        |
| `admin9Name` | character   | Same as `admin9Pcod` — hexagons have no human name.                                                                                                                 |
| `area`       | numeric     | Area in km². H3-6 cells are ~36 km², H3-7 ~5.16 km², H3-5 ~252 km². Near-constant within a resolution — this is expected; the column is still required by contract. |
| `geometry`   | sfc_POLYGON | H3 cell polygon                                                                                                                                                     |

**Note on area:** deployers should be made aware that near-uniform hex areas are
expected, not a data error. Document this in Step 1 §F (new hex section).

### 2.6 Naming convention

The function returns the layer unlabelled. The caller assigns it to
`my_shp$admin9_Hexagon`:

```r
my_shp$admin9_Hexagon <- make_hex_grid(my_shp$admin0_Country)
```

### 2.7 Package dependency

The H3 package dependency (`h3jsr` or `h3o`) is **deferred** — decided when the
function is implemented and benchmarked. Step 1 references `make_hex_grid()` as
the canonical API, with a note that the underlying H3 package will be declared
in `DESCRIPTION` at implementation time.

---

## 3. New function: `make_admin_lookup()`

### 3.1 Purpose

Given a named list of `sf` layers (any number, any levels including
`admin9_Hexagon`), build the full parent-child P-code cascade, validate it, and
return the enriched `my_shp` list with all parent Pcod columns populated on
every layer. The output is ready for `saveRDS()`.

### 3.2 Signature

```r
make_admin_lookup(shp_list)
```

| Argument   | Type                       | Description                                                                      |
| ---------- | -------------------------- | -------------------------------------------------------------------------------- |
| `shp_list` | named list of `sf` objects | Same structure as `my_shp`. Names must follow `admin<N>_<HumanName>` convention. |

### 3.3 Algorithm

1. **Parse order**: extract the `<N>` digit from each slot name and sort layers
   numerically ascending (coarsest → finest). Level numbers need not be
   contiguous. Order of the input list does not matter.

2. **Pre-flight validation** per layer:
   - `admin<N>Pcod` exists and is unique (no duplicates, no NAs).
   - `admin<N>Name` exists and is unique (no duplicates, no NAs).
   - Layer is an `sf` object.
   - `area` column exists and is numeric. If absent, emit a warning naming the
     layer and compute it in-place as
     `as.numeric(units::set_units(sf::st_area(geometry), "km^2"))` in the
     layer's existing CRS (assumed EPSG:4326). All layers must already be in
     EPSG:4326 — `make_admin_lookup()` does not re-project.

3. **Centroid-in-polygon join** per adjacent level-pair (parent → child):
   - Compute centroids of child layer.
   - Spatial join child centroids to parent polygons.
   - s2 fallback (same pattern as `make_hex_grid()`) wrapping both this step
     and any retry.
   - **Tie-break**: if a centroid falls exactly on a boundary and matches
     multiple parents, pick one at random and emit a warning listing the count
     of affected polygons.

4. **Many-to-one validation** per level-pair: after joining, verify every child
   Pcod appears exactly once in the lookup. Zero matches = orphan (error). More
   than one match after tie-breaking = implementation error (error).

5. **Populate parent Pcod columns**: join the lookup back onto each child layer,
   adding the parent's Pcod column. Repeat up the hierarchy so each layer
   carries all ancestor Pcods.

6. **Return enriched list**: same structure as input, same slot names, same `sf`
   tibbles — but now every sub-admin layer carries all parent Pcod columns,
   satisfying the cascade rule.

### 3.4 Hexagon layers

`admin9_Hexagon` is passed alongside regular admin layers and receives parent
Pcod columns via centroid-in-polygon join, the same as any other layer. There
are no exceptions to the cascade rule — hexagons carry `admin0Pcod`,
`admin1Pcod`, `admin2Pcod`, etc., with border hexagons assigned to the parent
whose polygon contains their centroid.

### 3.5 s2 fallback

Same `on.exit()` guard pattern as `make_hex_grid()`. Applied to all spatial
operations inside the function.

### 3.6 Return value

The enriched `my_shp` list. The internal lookup table is an implementation
detail and is not returned.

### 3.7 Step 1 workflow integration

Step 1 §E replaces the manual centroid-join code with:

```r
# HEX_RESOLUTION is set in 00-master.R -- the single control point for
# what H3 resolution ships with the app.

# Step 1: assemble raw layers (without parent Pcods)
my_shp <- list(
  admin0_Country  = adm0,
  admin1_Province = adm1,
  admin2_District = adm2,
  admin9_Hexagon  = make_hex_grid(adm0, resolution = HEX_RESOLUTION)
)

# Step 2: build and validate cascade; populate all parent Pcod columns
my_shp <- make_admin_lookup(my_shp)

# Step 3: validate structure
validate_geometries(my_shp)

# Step 4: save
saveRDS(my_shp, "app-data/shapes.rds", compress = "gz")
```

---

## 4. `admin9_Hexagon` and `validate_geometries()`

Hexagons pass the same structural checks as any other admin layer:

| Check                                                   | Hexagon behaviour                          |
| ------------------------------------------------------- | ------------------------------------------ |
| Layer name parses to `admin<digit>_<name>`              | `admin9_Hexagon` — passes                  |
| Element is an `sf` object                               | Yes                                        |
| `admin9Pcod` exists and is unique                       | H3 index — globally unique by construction |
| `admin9Name` exists and is unique                       | Same as Pcod — unique                      |
| No NAs in Pcod/Name                                     | True by construction                       |
| All parent Pcod columns exist                           | Yes — populated by `make_admin_lookup()`   |
| All parent Pcod values appear in parent layer (cascade) | Yes — validated by `make_admin_lookup()`   |
| Geometries are `POLYGON` or `MULTIPOLYGON`              | H3 cells are polygons                      |

No validator exceptions for the hexagon layer.

---

## 5. Downstream step changes (Steps 2–5)

### 5.1 Step 2 — Zonal stats

**Current problem:** Step 2 loads raw `rwa_adm2.geojson` and references
`shapeID` as the Pcod column. This can diverge from `admin2Pcod` in `shapes.rds`.

**Fix:**

- Replace raw file loading with:
  ```r
  my_shp <- readRDS("app-data/shapes.rds")
  adm2 <- my_shp$admin2_District
  ```
- Use `admin2Pcod` (not `shapeID`) as the join key throughout.
- Add a hex-level extraction example:
  ```r
  adm9 <- my_shp$admin9_Hexagon
  adm9$mean_value <- exact_extract(r, adm9, fun = "mean")
  ```
- Add a note: H3-6 hex cells have near-uniform area (~36 km²), so
  population-weighted extraction differs from area-weighted. Choose the
  aggregation function deliberately.

### 5.2 Step 3 — Metadata Excel

- All admin sheet examples load from `app-data/shapes.rds`.
- Add a callout: if `admin9_Hexagon` was built in Step 1, the workbook must
  contain a sheet named `admin9_Hexagon` with `spatial_level = "admin9_Hexagon"`
  in the `metadata` sheet.

### 5.3 Step 4 — HEX data

Step 4 is now specified in arch-11. Key resolution-related points:

- the `admin9Pcod` values in `shapes.rds` are H3 indices at whatever resolution
  was set via `HEX_RESOLUTION` in `00-master.R` and passed to `make_hex_grid()`
  in Step 1. That is the **single place** a deployer controls what resolution
  ships with the app.
- `fetch_hex_data()` reads the resolution automatically from the H3 index
  strings in `hex_ids` — no separate configuration needed in Step 4.
- If `HEX_RESOLUTION = 5` (H5 grid) and the data source provides H6,
  `fetch_hex_data()` transparently expands H5→H6, fetches, and aggregates back.
- If `HEX_RESOLUTION = 7`, `fetch_hex_data()` errors with an actionable
  message pointing back to `HEX_RESOLUTION` in `00-master.R`.

### 5.4 Step 5 — Compile & finalise

- `admin9_Hexagon` is included in `shapefiles.zip` by default (one GeoJSON per
  layer, same as all other layers → `admin9_Hexagon.geojson`).
- Add a note: for large countries at resolution 6, the hex GeoJSON may be
  several MB. File size is controlled only by resolution choice at Step 1.
  `st_simplify()` is not meaningful for regular hex cells.

---

## 6. New section in Step 1: §F — Hexagon grid (optional)

A new section added to `build-pti-1-shapefiles.qmd` after the existing §E:

```
## F. Optional: build an H3 hexagon grid

Why hexagons? For indicators available only as raster surfaces (population,
night-time lights, accessibility), hexagons provide a uniform spatial unit that
avoids the modifiable areal unit problem introduced by irregular admin polygons.
The `admin9_Hexagon` layer in `shapes.rds` is the spatial join key that Step 4
(HEX data) uses to match API-returned values to your app.

Resolution guide:
| Resolution  | Approx. cell area | Typical use                         |
| ----------- | ----------------- | ----------------------------------- |
| 5           | ~252 km²          | Very large countries, coarse view   |
| 6 (default) | ~36 km²           | Most country-level PTI apps         |
| 7           | ~5.2 km²          | Small countries or high-detail apps |

All hex cells at a given resolution have near-identical area — this is expected
and correct. The `area` column will appear nearly constant; that is not a bug.

[code example using make_hex_grid() and make_admin_lookup() as primary workflow]
```

---

## 7. Function file locations

| Function / Script       | File                                        | Export |
| ----------------------- | ------------------------------------------- | ------ |
| `make_hex_grid()`       | `R/fct_make_hex_grid.R`                     | yes    |
| `make_admin_lookup()`   | `R/fct_make_admin_lookup.R`                 | yes    |
| Rwanda data rebuild     | `data-raw/generate-rwa-package-data.R`      | no     |

`data-raw/generate-rwa-package-data.R` must be updated as part of this work (see Decision 19). The Rwanda template (`inst/template_pti/01-shapes.qmd`) is the source the script mirrors — both must stay in sync.

---

## 8. Tests required

| Test file                                 | Key expectations                                                                                                                                                                                                                                            |
| ----------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `tests/testthat/test-make-hex-grid.R`     | Returns `sf` with `admin9Pcod`, `admin9Name`, `area`, `geometry`. Pcods are unique. Area is numeric. Passes with `rwa_shp$admin0_Country` as input. Passes with `rwa_shp$admin1_Province` (triggers union). s2 fallback path exercised by mocking s2 error. |
| `tests/testthat/test-make-admin-lookup.R` | Returns enriched list with all parent Pcod columns populated. Validates many-to-one. Errors on orphan children. Tie-break warning emitted when centroids are ambiguous. Works with `admin9_Hexagon` included. s2 fallback path exercised. Warning emitted and `area` computed when a layer is missing it. |

---

## 9. Decisions recorded

| #   | Decision                                                                                                |
| --- | ------------------------------------------------------------------------------------------------------- |
| 1   | `make_hex_grid()` default resolution = 6. User may set 5 or 7.                                          |
| 2   | Coarse pre-filter resolution = `resolution - 2` (derived internally).                                   |
| 3   | Coarse-to-fine child expansion uses pure H3 math — no spatial operations.                               |
| 4   | Centroid filter for inclusion test. Not polygon-polygon intersection.                                   |
| 5   | `admin9Pcod` = H3 index string. `admin9Name` = same.                                                    |
| 6   | `make_hex_grid()` unions all input rows — user may pass any admin layer.                                |
| 7   | `make_admin_lookup()` takes named list, infers order from `admin<N>_` prefix.                           |
| 8   | Tie-break on ambiguous centroid assignment: random sample + warning with count.                         |
| 9   | Many-to-one validation enforced after each level-pair join.                                             |
| 10  | `admin9_Hexagon` carries all parent Pcods — no cascade exceptions.                                      |
| 11  | s2 fallback with `on.exit()` guard in both functions.                                                   |
| 12  | `make_admin_lookup()` returns enriched `my_shp` list (not the internal lookup table).                   |
| 13  | H3 package dependency (`h3jsr` or `h3o`) decided at implementation time.                                |
| 14  | Rwanda (`rwa_shp`) used as guide example throughout. Ukraine `admin4_Hexagon` not referenced.           |
| 15  | Step 4 resolution-mismatch must produce a clear error with actionable message.                          |
| 16  | `admin9_Hexagon` included in `shapefiles.zip` by default. File size managed via resolution choice only. |
| 17  | `make_admin_lookup()` warns and computes `area` in km² (EPSG:4326, s2-based) if a layer is missing it. Does not error, does not re-project. |
| 18  | All layers in `my_shp` must be in EPSG:4326. This is the single project CRS standard. No UTM step anywhere in the workflow. |
| 19  | `rwa_shp` is the output of `data-raw/generate-rwa-package-data.R`, which must be re-run whenever the Step 1 workflow changes. `make_admin_lookup()` must exist before `rwa_shp` can be correctly rebuilt. `data-raw/generate-rwa-package-data.R` must be updated to call `make_admin_lookup()` (replacing the manual centroid-join) and to use `as.numeric(units::set_units(st_area(geometry), "km^2"))` (replacing the current `as.numeric(st_area(geometry))` which produces m²). |
| 20  | `admin9` level number is a **safe convention** for hex layers, not a structural requirement. The package accepts any digit 0–9 as a level number. Level 9 is chosen because it is unlikely to conflict with real admin levels. `validate_geometries()` treats `admin9_Hexagon` identically to any other named admin level. |

---

## 10. Out of scope for this plan

- Implementing the Step 4 HEX API fetch (`fetch_hex_data()`) — covered by arch-05.
- Validator UX refactor (`cli` output) — covered by arch-06.
- Performance benchmarking of `make_hex_grid()` at resolution 7 for large countries (DRC, China) — follow-up issue.
- `R CMD check` example coverage for `make_hex_grid()` and `make_admin_lookup()` — covered by arch-03 testing rules.
