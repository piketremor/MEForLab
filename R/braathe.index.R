#' Calculate Braathe Competition Index
#'
#' Braathe
#' @param comp.ht - height (ft) of the competitor tree
#' @param focal.ht - height (ft) of the subject tree
#' @param distance - distance between trees (ft)
#' @return Braathe index
#' @examples
#' braathe.index(56,57,12)
#' @author Premer, M.I. - Maine Forest Lab
#' @references Braathe, P., 1980. Height increment of young single trees in relation to height and distance of neighboring trees. Mitt. Forstl. VersAnst. 130, 43–48.


braathe.index <- function(comp.ht,focal.ht,distance){
  braathe.index = comp.ht/(focal.ht*distance)
  return(braathe.index)
}
