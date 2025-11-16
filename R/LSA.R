#' LSAjac
#' @examples
#' \dontrun{
#'   SIMS <- runFunHES()
#'   ddsens <- LSAjac(SIMS, time = 0)
#'   sapply(ddsens, uniqueN)
#'   out_pca <- paramSens2pca(ddsens)
#'   out_pca$correlmatrix
#'   out_pca$rotation
#'   out_pca$plot
#'   p <- parameterClustering(out_pca, nPC = 2, kTree = 2)
#'   p
#' }
#' 
#' @export
LSAjac <- function(SIMS
                   , x = SIMS$Pars
                   , time = 0){
  derivsAll <- cyclist05wrapper(x = x, SIMS = SIMS)
  names(derivsAll)
  str(derivsAll)
  require(numDeriv)
  jacobian_matrix <- jacobian(func = cyclist05wrapper
                              , x = x
                              , SIMS = SIMS
                              , time = time
                              , kpiOnly = TRUE)
  parNames <- names((SIMS$Pars))
  ddsens <- as.data.table(jacobian_matrix)
  setnames(ddsens, names(ddsens), parNames)
  ddsens[, kpi := names(derivsAll)]
  .ddsens <<- copy(ddsens)
  ddsens[]
}



#' cyclist05wrapper
#' 
#' @export
cyclist05wrapper <- function(x = SIMS$parmsUsed
                             , SIMS
                             , time = 0
                             , y = SIMS$y_init00
                             , driv_funs = SIMS$driv_funs
                             , CONSTANTS = SIMS$CONSTANTS
                             , kpiOnly = TRUE){
  
  if (time > 0){
    ind <- findInterval(time, unlist(SIMS$out[, time]))
    nms <- names(y)
    y = unlist(as.data.table(SIMS$out)[ind, ..nms])
  }
  derivs <- cyclist05(time = time
                      , y = y
                      , pa = x
                      , driv_funs = driv_funs
                      , CONSTANTS = CONSTANTS
  )
  if (!kpiOnly){
    derivs0 <- setNames(as.numeric(derivs[[1]]), names(SIMS$y_init00))
    res <- c(derivs0, derivs[[2]])
  } else {
    res <- derivs[[2]]
  }
  res
  # as.list(res)
}
