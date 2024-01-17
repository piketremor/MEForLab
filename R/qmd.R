#' Calculate Quadratic Mean Diameter
#'
#'This function calculates QMD (inches) given basal area and trees per acre
#' @param ba basal area per acre
#' @param tpa trees per acre
#' @return quadratic mean diameter in inches
#' @examples
#' qmd(120,310)
#' @author Premer, M.I. - Maine Forest Lab


qmd <- function(ba,tpa){
  quad = sqrt(ba/tpa/0.005454)
  return(quad)
}
