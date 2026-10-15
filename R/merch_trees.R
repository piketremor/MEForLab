#' Merchandise a whole tree list into products
#'
#' The tree-list front end to the GreenTimbMerch functions. It takes the
#' same tree list as the growth engines (DBH in inches, height in feet, TPA)
#' and returns per-tree and per-acre product volumes. That makes it
#' possible to merchandise a projected stand at any year.
#'
#' For each tree it calls [Merchandize.ASG()], which uses the region's
#' Kozak taper and [merch_specs()]:
#'
#' * **Sawlog:** from a 1-ft stump to the species' sawlog top diameter, cut
#'   into 8-ft logs and scaled with the International 1/4-inch rule. Only
#'   trees flagged as acceptable growing stock (ASG) get a sawlog.
#' * **Pulp:** the rest of the stem to the pulp top diameter.
#' * **Cull:** stump plus top.
#'
#' @param trees tree list with `fvs` (species), `dbh` (in.), `ht` (ft) and
#'   optionally `tpa`, `stand`, and a growing-stock column
#' @param region "NE" or "GL" (taper parameters; see [kozak_taper()])
#' @param gs name of a column holding "ASG"/"USG" (or TRUE/FALSE for ASG), or
#'   NULL to treat every tree that reaches sawlog size as ASG (an upper bound
#'   on sawlog volume)
#' @param by grouping column for the per-acre summary
#' @return list: `trees` (per-tree results in imperial units: `saw_bf`,
#'   `saw_cuft`, `pulp_cuft`, `cull_cuft`, `total_cuft`, `merch_cuft`,
#'   `n_logs`) and, when `tpa` is present, `per_acre` (the same quantities
#'   per acre, by `by`, plus pulp in cords at 128 ft3 per cord)
#' @examples
#' tl <- data.frame(stand = "A", fvs = c("RS", "SM", "WP"), dbh = c(12, 16, 20),
#'                  ht = c(60, 70, 85), tpa = c(40, 20, 10))
#' m <- merchandize_trees(tl)
#' m$trees
#' m$per_acre
#' @export
merchandize_trees <- function(trees, region = c("NE", "GL"), gs = NULL, by = "stand") {
  region <- match.arg(region)
  t <- as.data.frame(trees)
  GS <- if (is.null(gs)) rep("ASG", nrow(t)) else {
    g <- t[[gs]]; if (is.logical(g)) ifelse(g, "ASG", "USG") else toupper(as.character(g)) }
  res <- lapply(seq_len(nrow(t)), function(i) {
    r <- tryCatch(Merchandize.ASG(1, 1, i, t$fvs[i], t$dbh[i] * 2.54, t$ht[i] * 0.3048,
                                  GS = GS[i], region = region),
                  error = function(e) NULL)
    if (is.null(r)) return(rep(NA_real_, 7))
    c(r$Saw.BF.ASG, r$Saw.Vol.ASG, r$Pulp.Vol.ASG, r$Cull.Vol.ASG, r$Total.Vol,
      r$Merch.Vol, r$LogCount)
  })
  m <- do.call(rbind, res)
  m3 <- 35.3147
  out <- t
  out$saw_bf <- m[, 1]
  out$saw_cuft <- m[, 2] * m3
  out$pulp_cuft <- m[, 3] * m3
  out$cull_cuft <- m[, 4] * m3
  out$total_cuft <- m[, 5] * m3
  out$merch_cuft <- m[, 6] * m3
  out$n_logs <- m[, 7]
  res <- list(trees = out)
  if (!is.null(t$tpa)) {
    grp <- if (!is.null(t[[by]])) t[[by]] else rep("all", nrow(t))
    v <- c("saw_bf", "saw_cuft", "pulp_cuft", "cull_cuft", "total_cuft", "merch_cuft")
    pa <- rowsum(out[, v] * t$tpa, grp, na.rm = TRUE)
    pa <- data.frame(group = rownames(pa), pa, row.names = NULL, check.names = FALSE)
    names(pa)[1] <- by
    pa$pulp_cords <- pa$pulp_cuft / 128
    res$per_acre <- pa
  }
  res
}

#' @importFrom stats optimize
NULL
