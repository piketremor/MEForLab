#' Estimate top height of spruce/fir using Vicary, Brann, and Griffin
#'
#' Top height
#' @param SPP Species, either "BF" or "RS" (vectorised)
#' @param age Estimated age of origin (years)
#' @param si Site index (ft, base age 50)
#' @return Estimated mean top height (ft)
#' @details This is the exact algebraic inverse of [vicary.site()], so
#'   `vicary.site(SPP, vicary.height(SPP, age, si), age)` returns `si`.
#' @examples
#' vicary.height("BF", 42, 65)
#' vicary.site("BF", vicary.height("BF", 42, 65), 42)  # 65
#' @author Premer, M.I. - Maine Forest Lab
#' @references
#' Vicary, B.P.; Brann, T.B.; Griffin, R.H. 1984. B802: Base-Age Invariant
#' Polymorphic Site Index Curves for Even-Aged Spruce-Fir Stands in Maine.
#' Maine Agricultural and Forest Experiment Station Bulletin 802.
#' https://digitalcommons.library.umaine.edu/aes_bulletin/130
#' @export
vicary.height <- function(SPP, age, si) {
  a <- .vicary_asymptote(SPP)
  a * (1 - exp((age / 50) * log(1 - si / a)))
}

# Shared asymptote so vicary.site() and vicary.height() invert exactly.
# NOTE: the two functions previously used 110.9866 and 110.9886 for RS;
# 110.9866 (the vicary.site value) is kept here -- verify against Bulletin 802.
.vicary_asymptote <- function(SPP) {
  a <- c(RS = 110.9866, BF = 112.3188)[as.character(SPP)]
  if (anyNA(a)) stop("Vicary curves are only defined for SPP = 'RS' or 'BF'")
  unname(a)
}
