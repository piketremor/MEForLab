#' Estimate stem volume by integrating a taper function (Smalian's formula)
#'
#' BCKozakVol - ft3 per stem
#'
#' The stem is cut into `sgmts` equal-length segments and segment volumes are
#' summed with Smalian's formula.
#'
#' NOTE: the original version called `BCKozakTap()`, which was never added to
#' the package, so it always errored. The species-specific BC Kozak (1988)
#' parameters are still missing. Until they are added, pass a taper function
#' through `taper`; it must take `(dbh, ht, h)` in inches/feet and return
#' diameter (in.) at height `h` (ft).
#'
#' @param SPP Species code (retained for when BC parameters are added)
#' @param dbh Tree diameter at breast height (in.)
#' @param hgt Total tree height (ft)
#' @param taper Function `(dbh, ht, h)` returning diameter (in.) at height h
#' @param sgmts Number of segments
#' @return Tree volume (ft3)
#' @examples
#' # simple cone as a stand-in taper
#' cone <- function(dbh, ht, h) pmax(dbh * (ht - h) / (ht - 4.5), 0)
#' BCKozakVol("DF", 12, 70, taper = cone)
#' @author Premer, M.I. - Maine Forest Lab - adapted from Brian Smith - UMaine
#' @references Kozak, A. 1988. A variable-exponent taper equation. Can. J.
#'   For. Res. 18: 1363-1368.
#' @export
BCKozakVol <- function(SPP, dbh, hgt, taper = NULL, sgmts = 100) {
  if (is.null(taper))
    stop("BCKozakVol: no BC Kozak taper parameters are in the package yet; ",
         "supply a taper function via `taper = function(dbh, ht, h)`.")
  L <- (hgt - 0.01) / sgmts
  h1 <- L * (0:(sgmts - 1))
  h2 <- h1 + L
  sum(smalians(taper(dbh, hgt, h1), taper(dbh, hgt, h2), L))
}

#' Smalian's log volume
#'
#' @param d1,d2 End diameters (in.)
#' @param L Length (ft)
#' @return Volume (ft3)
#' @examples
#' smalians(10, 8, 16)
#' @export
smalians <- function(d1, d2, L) {
  0.005454154 * (d1^2 + d2^2) / 2 * L
}
