#' Estimate site index of spruce/fir using Vicary, Brann, and Griffin
#'
#' Site Index
#' @param SPP Species, either "BF" or "RS" (vectorised)
#' @param ht Mean top height (ft)
#' @param age Estimated age of origin (years)
#' @return Estimated site index at base age 50 (ft)
#' @examples
#' vicary.site("BF", 65, 42)
#' @author Premer, M.I. - Maine Forest Lab
#' @references
#' Vicary, B.P.; Brann, T.B.; Griffin, R.H. 1984. B802: Base-Age Invariant
#' Polymorphic Site Index Curves for Even-Aged Spruce-Fir Stands in Maine.
#' Maine Agricultural and Forest Experiment Station Bulletin 802.
#' https://digitalcommons.library.umaine.edu/aes_bulletin/130
#' @export
vicary.site <- function(SPP, ht, age) {
  a <- .vicary_asymptote(SPP)
  a * (1 - exp((50 / age) * log((ht - a) / -a)))
}
