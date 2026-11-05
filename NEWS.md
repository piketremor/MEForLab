# MEForLab 0.2.2

## New

* `TDM()`: mean diameter differentiation (Fuldner 1995) of a subject tree vs
  its competitors.
* `SCI()`: structural complexity index (Zenner and Hibbs 2000) from a polar
  stem map (distance, azimuth) with DBH as the surface height; `geometry` added
  to Suggests.

* Forester marking workflow: `cp_view_stand()` (species crown-area profile and a
  suggested gap band), `cp_plan()` (strata, gap bands, residual BA and species
  favor weights; transparent priority rule or `method = "optimize"`),
  `cp_dbh_guide()` (trees/ac to cut and leave by species x 2-in. DBH class with a
  crown tie-breaker), `cp_guide_sheet()` (one-page PNG: profiles before/after,
  guide grid, diameter distribution) and `cp_workflow()` (all four, writes the
  CSV and sheet). Supporting `cp_mark_species()` (species x stratum growing-space
  allocation) and `cp_species_group()`.
* `cp_printsplit()`: PrintSplit (S), the relative degree of vertical separation
  between canopy strata across one or more gap bands of a CrownPrint profile
  (1 = fully separated, 0 = no separation, < 0 = strata merged); independent
  of stocking. `cp_mark()` reports it before and after marking
  (`$printsplit`). `cp_gap_depth()` is kept as a deprecated alias.
* Crown-profile marking: `cp_mark()` marks trees for removal so the residual
  stand's crown occupancy in canopy space approaches a target profile (by
  default the stand's own profile with a gap band emptied between two strata)
  at a residual basal area. Crown-area weighting (`MCW()`), canopy limits fixed
  at pre-harvest values, greedy Hellinger-distance search, protected trees and
  reason codes. Helpers: `cp_target_carve()`, `cp_target()`;
  `print` and `plot` methods.

# MEForLab 0.2.1

## New

* CrownPrint (Premer 2026, Can. J. For. Res. 56: 1-14,
  doi:10.1139/cjfr-2025-0239): `crownprint()` runs the full crown-profile
  workflow on a plot inventory, with nested step functions:
  - `cp_height()`: total height from the FVS-NE Wykoff equation, the paper's
    Table 4 coefficients, or a local per-species fit of eq. 1 (plot random
    effects via nlme, falling back to nls).
  - `cp_hcb()`: height to crown base from a local fit of eq. 2, the paper's
    Table 5 coefficients, or observed crown-ratio medians.
  - `cp_profile()`: normalized canopy space per plot (lowest live crown = 0,
    tallest tree = 1) and expansion-weighted crown occupancy by species or
    user-defined group in 0.01 layers.
  - `cp_beta()`: beta fits per plot x group with bootstrap SEs.
  - `cp_summary()`: group alpha / beta on the log scale (lme with a random
    stand effect when there are >= 3 stands), paired between-group
    comparisons and profile overlap.
  - `cp_interpret()`, plus `print`, `summary` and `plot` methods.
* `nlme` added to Suggests; `grDevices` to Imports.

# MEForLab 0.2.0

## New

* Growth projection: `project_stands()` with two engines. `conus_project()`
  implements the FVS Remodeling Project CONUS equations in R;
  `fvs_project()` runs a compiled FVS variant through keyword files and reads
  `FVS_Summary` back.
* Silviculture: `treatment()` / `apply_treatment()` cover thinning from
  below, thinning from above, proportional thinning and clearcut, using the
  same definitions in both engines.
* Inventory to tree list: `read_inventory()` takes any data frame or
  csv/xlsx file. It handles prism or fixed-area plots and lets you name the
  columns (matched case-insensitively). Species can be FVS, FIA, TallTimber
  codes, common names, or your own codes through `species_map`. It keeps
  measured heights and fills the missing ones. Also added:
  `expand_plots()`, `expand_sapling_tally()`, `complete_trees()`,
  `stand_summary()`, the `read_demeritt_inventory()` preset, and a species
  crosswalk (`meforlab_species()`, `species_lookup()`).
* `inst/tutorial/capstone_demo.Rmd`.
* testthat tests.

* Merchandising: GreenTimbMerch (Premer; adapted from Weiskittel and Smith)
  is now part of MEForLab.
  - New core: `kozak_taper()`, `kozak_volume()`, `height_at_diameter()`
    and `dob_to_dib()`. They are vectorised, take imperial or metric units,
    and have a `region` argument: `"NE"` (Acadian/Northeast fits), `"GL"`
    (currently the NE parameters) and `"PNW"` (coastal Douglas-fir).
  - `merchandize_trees()` merchandises a whole tree list (current or
    projected) into sawlog board feet, pulp and cull per acre.
  - Product minimums live in `merch_specs()`; override them with
    `options(MEForLab.merch_specs =)`.
  - The original GreenTimbMerch interfaces are kept with the same arguments
    and values: `KozakTaper()`, `KozakTreeVol()`, `DOBtoDIB()`,
    `MerchDiam()`, `Merchandize.ASG()`, `MerchHT()`, `Stick.Cruise()`,
    `Merchandize.Form.Risk()`, `Sawlog.Likelihood()`, `ValueEstimate()`,
    `Genus()`. The `purrr` and `dplyr` dependencies are gone.
  - `Kozak_merchR()` is the PNW (Douglas-fir) merchandiser. It can now use
    the NE/GL taper via `region =` and a `spp` column.

## Fixes

* One `smalians()` (with `units = "imperial"` or `"metric"`) replaces the
  duplicate `smalians()` / GreenTimbMerch `Smalians()` pair.

* GreenTimbMerch taper fixes:
  - **Operator precedence:** `SPP == "BA"`, `"TA"` and `"NC"` were matched
    regardless of bark, so northern white-cedar outside-bark used the
    inside-bark fit.
  - **White spruce inside bark** used a copy of the white pine outside-bark
    parameters. The intended white spruce fit was unreachable and is now
    used.
  - **Species without parameters** now fall back to the generic hardwood or
    softwood curve instead of the softwood inside-bark curve.
* `Stick.Cruise()`: the first section's saw volume no longer has a small
  rounding artefact.
* `Kozak_merchR()`: Scribner volumes below zero (small ends under about
  3.7 in.) are now set to 0.

* `MCW()` computed `a1 * DBH(in) * a2`. It now uses the published metric form
  `a1 * DBH(cm)^a2`, converted to feet. **Results change.**
* `vicary.height()` did not invert `vicary.site()`, and the two functions used
  different RS asymptotes (110.9886 vs 110.9866). Both now share 110.9866;
  check that value against Bulletin 802.
* `basal.area.larger()` returned basal area in *smaller* trees (including the
  tree itself), in sorted rather than input order. **Results change.**
* `BCKozakVol()` called the missing `BCKozakTap()` and `smalians()`.
  `smalians()` is now included, and `BCKozakVol()` takes a `taper` function
  until the BC parameters are added.
* `Kozak_merchR()` errored on an undefined `cr`.
* `vol.calc()`, `wykoff.ht()` and `steinman.site()` now accept vectors. Before,
  they used scalar `if` blocks and failed inside a data frame. Unknown species
  return NA with a warning.
* `coord.convert()` now uses `sf` (`rgdal` has been retired from CRAN).
* `upslope()` / `create_layers()`: namespaced raster calls, and the resolution
  check is fixed.
* Documentation fixed throughout. `NAMESPACE` is now generated by roxygen.
