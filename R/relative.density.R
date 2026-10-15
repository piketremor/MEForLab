#' Calculate Relative Density Index
#'
#' Curtis' relative density, RD = BA / sqrt(QMD)
#' @param bapa - basal area per acre (ft2/ac)
#' @param qmd - quadratic mean diameter (in.)
#' @return Relative Density Index
#' @examples
#' relative.density.index(120, 8.5)
#' @author Premer, M.I. - Maine Forest Lab


#' @export
relative.density.index <- function(
    bapa,
    qmd
){

  out = bapa/sqrt(qmd)
  return(out)

}
