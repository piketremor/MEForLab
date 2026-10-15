#' Species code crosswalk
#'
#' TallTimber numeric codes (used by the Demeritt Forest / UMaine inventory),
#' FVS Northeast-variant alpha codes, and FIA species codes (SPCD).
#'
#' @return data.frame with columns `tt_code`, `fvs`, `spcd`, `common`,
#'   `scientific`, `softwood`
#' @examples
#' head(meforlab_species())
#' @export
meforlab_species <- function() {
  if (is.null(.mfl$species)) {
    f <- system.file("extdata", "species_codes.csv", package = "MEForLab")
    .mfl$species <- utils::read.csv(f, stringsAsFactors = FALSE)
  }
  .mfl$species
}

#' Translate species codes
#'
#' @param x vector of codes
#' @param from,to one of "tt" (TallTimber numeric), "fvs" (NE alpha), "spcd"
#'   (FIA numeric), "common"; `to` can also be "softwood" or "sdimax_ne"
#'   (FVS-NE maximum SDI)
#' @return vector of translated codes; NA where there is no match
#' @examples
#' species_lookup(c(3, 4, 8), from = "tt", to = "fvs")
#' species_lookup(c("RS", "BF"), from = "fvs", to = "spcd")
#' @export
species_lookup <- function(x, from = c("tt", "fvs", "spcd", "common"),
                           to = c("fvs", "spcd", "tt", "common", "softwood", "sdimax_ne")) {
  from <- match.arg(from); to <- match.arg(to)
  sp <- meforlab_species()
  col <- function(k) switch(k, tt = "tt_code", k)
  key <- sp[[col(from)]]
  if (from == "fvs") x <- toupper(trimws(x))
  sp[[col(to)]][match(x, key)]
}

# package-level cache
.mfl <- new.env(parent = emptyenv())
