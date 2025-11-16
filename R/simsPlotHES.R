#' simsPlotHES
#' @export
simsPlotHES <- function(SIMS
                        , SIMS.prior = list()
                        , pList = list()
                        , midfix = ""
                        , doi = "time"
                        , doTransient = FALSE
                        , addTransient = FALSE
                        , ...
){
  if (addTransient){
    SIMS$outORIG <- SIMS$out
    SIMS$out2 <- SIMS$out
    SIMS$out2[, time := time + tail(SIMS$times, 1)]
    SIMS$out <- rbind(SIMS$out_tr, SIMS$out2)
    .SIMS_TR <<- SIMS
  }
  
  # transient #############################
  if (doTransient & !is.null(SIMS$out_tr)){
    SIMS$out_tr[, 1]
    SIMS$out[, 1]
    p_tr <- plotBySegment(SIMS$out_tr
                          , dois = doi
                          , ...)
    p_tr
    pList[[paste0("p_transient_", midfix, doi)]] <- p_tr
  }
  
  
  # main run #############################
  p <- plotBySegment(SIMS$out
                     , dois = doi
                     , ...)
  pList[[paste0("p_run_", midfix, doi)]] <- p
  
  
  # prior sim #############################
  if (length(SIMS.prior)){
    if (all(dim(out) == dim(SIMS.prior$out.prior))){
      outDiff <- as.data.table(out - SIMS.prior$out.prior)
      outDiff[, time := out[, "time"]]
      outDiff[, T_air := out[, "T_air"]]
    }
    p_diff <- plotBySegment(outDiff
                            , dois = doi
                            , ...)
    pList[[paste0("p_changed_", midfix, doi)]] <- p_diff
  }
  
  pList
}
