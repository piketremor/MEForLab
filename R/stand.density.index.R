#' Calculate Stand Density Index
#'
#' SDI
#' @param tpa - trees per acre
#' @param qmd - quadratic mean diameter (inches)
#' @return the stand density index
#' @examples
#' stand.density.index(310,8.5)
#' @author Premer, M.I. - Maine Forest Lab - originally coded by N. Osborne


stand.density.index <- function(
    tpa,
    qmd
){

  b = 1.605
  out = tpa * (qmd/10)^b
  return(out)

}
