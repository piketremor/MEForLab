#' Calculate Glover Hool Competition Index
#'
#' Glover Hool
#' @param DBH - DBH (inches) of the focal tree
#' @param qmd - Quadratic mean diameter (inches) of the plot
#' @return Glover Hool index
#' @examples
#' glover.hool.index(10,13)
#' @author Premer, M.I. - Maine Forest Lab
#' @references Glover GR, Hool JN. A basal area ratio predictor of loblolly pine plantation mortality. Forest Science 1979; 25(2): 275-282. 10.1093/forestscience/25.2.275

glover.hool.index <- function(DBH, # DBH of the subject tree
                              qmd){ # quadratic mean diameter at the sample level
  a = DBH/qmd
  return(a)
}
