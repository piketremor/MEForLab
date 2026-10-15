# MEForLab

Forestry functions and growth-projection tools from the Maine Forest Management Lab,
University of Maine ([maineforestlab.com](https://maineforestlab.com)).

```r
remotes::install_github("piketremor/MEForLab")
library(MEForLab)
```

## What's in it

| area | functions |
|---|---|
| stand metrics | `qmd()`, `stand.density.index()`, `relative.density.index()`, `relative.spacing()`, `basal.area.larger()`, `stand_summary()` |
| tree equations | `wykoff.ht()`, `MCW()`, `vol.calc()`, `Kozak_merchR()`, `smalians()` |
| site index | `vicary.site()`, `vicary.height()`, `steinman.site()` |
| competition | `hegyi.index()`, `braathe.index()`, `glover.hool.index()`, `ek.monserud.index()`, `crown.overlap()` |
| wind | `crit.break()`, `crit.throw()` |
| GIS | `coord.convert()`, `upslope()`, `create_layers()` |
| inventory -> tree list | `expand_point_cruise()`, `expand_sapling_tally()`, `complete_trees()`, `read_demeritt_inventory()`, `species_lookup()` |
| growth projection | `project_stands()`, `treatment()`; engines `conus_project()` and `fvs_project()` |

## Two growth engines

* **`engine = "conus"`**: the FVS Remodeling Project's variant-free CONUS
  equations (Johnson, Marshall & Weiskittel,
  [holoros/fvs_remodeling](https://github.com/holoros/fvs_remodeling),
  snapshot 9ead428), implemented in R. It runs anywhere with nothing else to
  install. The equations are preliminary; see `?conus_project`.
* **`engine = "fvs"`**: a compiled FVS variant (Northeast by default) driven
  through keyword files. It needs FVS from
  <https://www.fs.usda.gov/fvs/software/>. Point to it with
  `options(MEForLab.fvs_dir = "C:/FVSbin")`.

```r
trees <- read_demeritt_inventory("DBD_66_Tree_DataBAF20_StandsOK.xlsx",
                                 "Sapling_MASTER_DBDWith2026.xlsx")
thin  <- list(treatment(0, "thin_below", residual_ba = 90, min_dbh = 4.5))
a <- project_stands(trees, "conus", years = 50, regime = thin)
b <- project_stands(trees, "fvs",   years = 50, regime = thin)
```

## Tutorial

`inst/tutorial/capstone_demo.Rmd` walks from the raw Demeritt Forest cruise to
50-year projections and an engine comparison. To open it after installing:

```r
file.edit(system.file("tutorial", "capstone_demo.Rmd", package = "MEForLab"))
```

## License

GPL-2. The bundled CONUS parameter tables come from holoros/fvs_remodeling (GPL-2).
