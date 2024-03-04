#' Calculate Ek and Monserud Competition Index
#'
#' Ek and Monesrud
#' @param overlap - overlap (ft2) of the focal and competitor tree
#' @param heightj - height (ft) of competitor tree
#' @param heighti - height (ft) of focal tree
#' @param crownwidthi - crown width (ft) of the focal tree
#' @param crownwidthj - crown width (ft) of the competitor tree
#' @param MCW - maximum crown width (ft)
#' @return Ek and Monserud Index
#' @examples
#' ek.monserud.index(60,45,20,20,45,130)
#' @author Premer, M.I. - Maine Forest Lab
#' @references Ek, A.R., Monserud, R.A. 1974. FOREST: A model for the growth and reproduction of forest stands. Dept. of Forestry. University of Wisconsin - Madison. Research Report. 93 p.

ek.monserud.index <- function(
    overlap, # area of overlap between two crowns
    heightj, # height of competitor tree
    crownwidthj, # crown width of the competitor tree
    crownwidthi, # crown width of the subject tree
    heighti, # height of the subject tree
    focal_area){ # OG crown area of the subject tree
  em = ((overlap/focal_area)*heightj*crownwidthj/(heighti*crownwidthi))
  return(em)
}
