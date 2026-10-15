#' Estimate site index of spruce/fir using Steinman equations
#'
#' Site Index
#' @param SPP Species code, "RS" or "BF" (vectorised)
#' @param HT Mean top height (ft)
#' @param AGE Estimated age of origin (years)
#' @return Estimated site index at base age 50 (ft)
#' @examples
#' steinman.site("RS", 65, 42)
#' @author Premer, M.I. - Maine Forest Lab
#' @references
#' Steinman, J.R. 1992. A comprehensive evaluation of spruce-fir growth and
#' yield in Maine as related to physical and chemical soil properties. Ph.D.
#' thesis, University of Maine, Orono, Maine.
#' @export
steinman.site <- function(SPP, HT, AGE) {
  cf <- rbind(RS = c(7.8531, -0.62589, -0.00010125, -0.22073, 0.33689),
              BF = c(1.19373, -0.77673, -0.00000137, -0.29571, 0.22433))
  k <- match(as.character(SPP), rownames(cf))
  if (anyNA(k)) stop("steinman.site is only defined for SPP = 'RS' or 'BF'")
  b <- cf[k, , drop = FALSE]
  (b[, 1] * (HT - 4.5)^b[, 2]) * (1 - exp(b[, 3] * AGE))^(b[, 4] * (HT - 4.5)^b[, 5])
}
