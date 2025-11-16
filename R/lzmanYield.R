#' lzmanYield
#' @examples \dontrun{
#' }
#' @export
lzmanYield <- function(dtw
                       , maxFutureSteps = 0
                       , bycols = "plot_syn"){
  dtw <- as.data.table(dtw)
  dtw <- copy(dtw)
  dtw[, harvest := yield / afw]
  
  if (maxFutureSteps == 0){
    # aal to the end
    dtw[, stem.diam.pred := findTails(stem.diam, which = "right", repl = "last"), by = c(bycols)]
    
    dtw[, yield.pred   := findTails(yield, which = "right", repl = "last"), by = c(bycols)]
    dtw[, afw.pred     := findTails(afw, which = "right", repl = "last"), by = c(bycols)]
    dtw[, harvest.pred := findTails(harvest, which = "right", repl = "last"), by = c(bycols)]
    
  } else {
    
    dtw[, stem.diam.pred := stem.diam]
    dtw[, yield.pred   := yield]
    dtw[, afw.pred     := afw]
    for (nstep in seq(maxFutureSteps)){
      dtw[, stem.diam.pred := shift(stem.diam.pred,  n = 1L, type = "lag"), by = c(bycols)]
      dtw[, yield.pred     := shift(yield.pred, n = 1L, type = "lag"), by = c(bycols)]
      dtw[, afw.pred       := shift(afw.pred,   n = 1L, type = "lag"), by = c(bycols)]
    }
    dtw[, harvest.pred := yield.pred / afw.pred]
  }
  
  dtw[, oogst.cu      := aphCumsum(harvest)     , by = c(bycols)]
  dtw[, oogst.cu.pred := aphCumsum(harvest.pred), by = c(bycols)]
  dtw[, productie.cu      := aphCumsum(yield)     , by = c(bycols)]
  dtw[, productie.cu.pred := aphCumsum(yield.pred), by = c(bycols)]
  
  return(dtw)
}
