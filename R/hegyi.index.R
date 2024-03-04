#' Calculate Hegyi Competition Index
#'
#' Hegyi
#' @param DBH - DBH (inches) of the focal tree
#' @param dbi - DBH (inches) of the competitor tree
#' @param distance - Distance (ft.) between trees
#' @return Heygi index
#' @examples
#' hegyi.index(10,13,3)
#' @author Premer, M.I. - Maine Forest Lab
#' @references Hegyi, Frank. 1974. A simulation model for managing jack pine stands. pp. 74-90. In: Growth models for tree and stand simulation, Fries, Joran. ed. Stockholm, Sweden: Royal College of Forestry.

hegyi.index <- function(DBH, #dbh of the subject tree
                        dbhi, # dbh of the competitor tree
                        distance) # distance bewteen stems in feet
{
  i = (dbhi/DBH)/distance
  return(i)
}
