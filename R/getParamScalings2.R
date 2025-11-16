#' getParamScalings2
#' 
#' @examples \dontrun{
#'   P = c(rfw = .1, fwmax = 120)
#'   getParamScalings2(names(P), P)
#' }
#' @importFrom data.table as.data.table
#' @export
getParamScalings2 <- function(freeSensParms
                              , parms
                              , relSD = 0.05){
  parScalings <- list(parameter = freeSensParms)
  # kk <- parms[  parScalings$parameter  ]
  # length(kk)
  parScalings$pca.center <- unname(unlist(parms[  parScalings$parameter  ]))
  parScalings$pca.sd <- unlist(  abs( parScalings$pca.center)  ) * relSD
  # str(parScalings)
  as.data.table(parScalings)
}
