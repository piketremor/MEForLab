#' Calculate Relative Density Index
#'
#' RD
#' @param tpa - trees per acre
#' @param bapa - basal area per acre
#' @return Relative Density Index
#' @examples
#' relative.density.index(220,11.2)
#' @author Premer, M.I. - Maine Forest Lab


relative.density.index <- function(
    bapa,
    qmd
){

  out = bapa/sqrt(qmd)
  return(out)

}
