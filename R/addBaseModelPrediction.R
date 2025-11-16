#' addBaseModelPrediction
#' @importFrom data.table copy as.data.table
#' @export
addBaseModelPrediction <- function(dtw
                                   , modelId = "lz"
                                   , FUN = lzman
                                   , yois = c("yield", "plantLoad")[1:2]
                                   , modelArgs = list()
                                   , bycols = c("plot_syn", "cycle_syn")
                                   , truthExtension = c(".actual", "")[1]
){
  dtw <- data.table::as.data.table(dtw)
  dtw <- data.table::copy(dtw)
  bycols <- intersect(bycols, names(dtw))
  for (yoi in yois){
    if (yoi %in% names(dtw)){
      dtw[, (paste0(yoi, "_", modelId, truthExtension)) := get(paste0(yoi, truthExtension))]
      
      dtw[, (paste0(yoi, "_", modelId, ".pred")) := 
            do.call(FUN, c(list(get(yoi)), modelArgs))
          , by = c(bycols)]
    } else {
      log_error("{yoi} not found in names of dt")
    }
  }
  dtw
}
