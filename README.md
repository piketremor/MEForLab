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
| inventory -> tree list | `read_inventory()` (any data frame or csv/xlsx; prism or fixed plots), `expand_plots()`, `expand_sapling_tally()`, `complete_trees()`, `species_lookup()`; `read_demeritt_inventory()` preset |
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
# any inventory: name your columns, say how plots were laid out
trees <- read_inventory("my_cruise.csv", design = "fixed", plot_acres = 1/10,
                        cols = list(stand = "Stand", plot = "Plot", species = "Spp",
                                    dbh = "DBH", ht = "Height", cr = "CR"),
                        species_codes = "fvs")   # or "spcd", "tt", "common", or species_map =
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
