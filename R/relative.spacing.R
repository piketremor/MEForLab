#' Calculate Relative Spacing
#'
#' Relative spacing
#' @param tpa - trees per acre
#' @param domht - stand dominant height (ft.)
#' @return Relative Spacing
#' @examples
#' relative.spacing(310, 55)
#' @author Premer, M.I. - Maine Forest Lab


#' @export
relative.spacing <- function(
    tpa,
    domht
){

  out = sqrt(43560/tpa) / domht
  return(out)

}
