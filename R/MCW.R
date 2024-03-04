#' Calculate Maximum Crown Width
#'
#' MCW
#' @param SPP - Species FVS Code
#' @param DBH - Diameter at breast height
#' @return Maximum Crown Width (ft.)
#' @examples
#' MCW("RS",8.5)
#' @author Premer, M.I. - Maine Forest Lab
#'@references
#'Russell, M. B., & Weiskittel, A. R. (2011). Maximum and Largest Crown Width Equations for 15 Tree Species in Maine.
#'Northern Journal of Applied Forestry, 28(2), 84–91. https://doi.org/10.1093/njaf/28.2.84

MCW <- function(SPP,DBH)
{
  SPcodes=c('BF','BS','EH','WP','NC','RS','WS','AB','GB','RB','RO','PB','QA',
            'RM','SM','YB','OH','OS','99')
  coefs = matrix(c(
    # a1           a2
    1.37       , 0.572      ,    # BF
    0.535      , 0.742      ,    # BS
    2.44       , 0.408      ,    # EH
    1.24       , 0.585      ,    # WP
    1.63       , 0.436      ,    # NC
    1.80       , 0.461      ,    # RS
    1.50       , 0.496      ,    # WS
    2.93       , 0.434      ,    # AB
    2.24       , 0.382      ,    # GB
    2.24       , 0.382      ,    # RB
    4.08       , 0.310      ,    # RO
    1.48       , 0.623      ,    # PB
    1.31       , 0.586      ,    # QA
    2.17       , 0.491      ,    # RM
    3.31       , 0.356      ,    # SM
    4.04       , 0.308      ,    # YB
    4.04       , 0.308      ,    # OH
    1.597128571, 0.513957143,    # OS
    2.24262    , 0.462653333),   # 99
    ncol=2,byrow=TRUE)
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)
  X <- coefs[sprow,1]*(DBH*0.3937)*coefs[sprow,2]
  ft = X*3.28
  return(ft)
}
