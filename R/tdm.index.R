#' Calculate Mean Diameter Differentiation (TDM)
#'
#' TDM - mean dbh differentiation of a subject tree vs its competitors
#' TDM = 1 - 1/n * sum(min(di,dj)/max(di,dj))
#' @param dj - DBH of the subject tree
#' @param di - vector of DBH of the competitor trees (same units as dj)
#' @return TDM (0 = equal sizes, toward 1 = large size contrast)
#' @examples
#' TDM(52,c(16,22,30,18))
#' @author Premer, M.I. - Maine Forest Lab
#' @references Fuldner, K. 1995. Strukturbeschreibung von Buchen-Edellaubholz-Mischwaldern. PhD dissertation, University of Gottingen.
#'
#' Premer, M.I., Chhin, S., and Zhang, J. 2021. Local testing and calibration of species-specific competition indices in Sierran mixed-conifer forests: application transfer to evolving objectives. Can. J. For. Res. 51: 524-532.

#' @export
TDM<-function(dj, #dbh of the subject tree
              di) #dbh of the competitors
{
  di<-di[!is.na(di)]
  n<-length(di)
  if(n==0) return(NA)
  tt<-0
  for(i in 1:n){
    tt<-tt+min(di[i],dj)/max(di[i],dj)
  }
  tdm<-1-(tt/n)
  return(tdm)
}
