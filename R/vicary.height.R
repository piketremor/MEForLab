#' Estimate top height of spruce/fir using Vicary, Brann, and Griffin
#'
#' Top height
#' @param SPP - Species, either "BF", or "RS"
#' @param si - estimated site index
#' @param age - estimated age of origin
#' @return Estimated mean top height
#' @examples
#' vicary.height("BF",65,42)
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#' Vicary, Bret P.; Brann, Thomas B.; and Griffin, Raph H.. 1984. B802: Base-Age Invariant Polymorphic Site Index Curves for Even-Aged Spruce-Fir Stands in Maine. Maine Agricultural and Forest Experiment Station Bulletins 802. https://digitalcommons.library.umaine.edu/aes_bulletin/130
#'


vicary.height <- function(SPP,si,age){
  if(SPP=="RS"){
    a=110.9886}
  if(SPP=="BF"){
    a=112.3188
  }
    height = a*((1-exp((log(1-(si/a)))/50))*age)
  return(height)
}
