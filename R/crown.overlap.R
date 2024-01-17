#' Calculate the total area of crown overlap between two trees
#'
#' Site Index
#' @param crownradi - crown radius (ft) of the competitor trees
#' @param crownradj - crown radius (ft) of the focal/subject tree
#' @param distance - distance (ft) between the two stems
#' @return Crown overlap (ft2)
#' @examples
#' crown.overlap(15,24,17)
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#'  The use of crown overlap is useful for calculating competition indices

crown.overlap <- function(crownradi, # crown radius of the competitor
                          crownradj, # crown radius of the subject tree
                          distance) # distance in feet between the two stems
{
  ov = (crownradi^2)*acos((distance^2+crownradi^2-crownradj^2)/(2*distance*crownradi))+
    (crownradj^2)*acos((distance^2+crownradj^2-crownradi^2)/
                         (2*distance*crownradj))-
    0.5*sqrt((-distance+crownradi+crownradj)*
               (distance+crownradi-crownradj)*
               (distance-crownradi+crownradj)*
               (distance+crownradi+crownradj))
  return(ov)
} # calculate area of overlap of two crowns in square feet using trig functions
