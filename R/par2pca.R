#' par2pca
#' scales and transforms parameters to  'PCA space' from 'original space'
#' @export
par2pca <- function(parms
                    , loads
                    , parScalings
                    , doExp = FALSE
                    ){
  nms <- names(parms)
  ok1 <- all.equal(parScalings$parameter, row.names(loads))
  log_trace("names from parScalings same {ok1}")
  ok2 <- all.equal(nms, row.names(loads))
  log_trace("names from parms same {ok2}")
  
  parScalingsTmp <- as.data.frame(parScalings)
  row.names(parScalingsTmp) <- parScalingsTmp$parameter
  parScalingsTmp <- parScalingsTmp[row.names(loads),]
  
  if (doExp){
    parmsScaled <- (logParms(parms) - parScalingsTmp$pca.center) / parScalingsTmp$pca.sd
  } else {
    parmsScaled <- (unlist(parms)   - parScalingsTmp$pca.center) / parScalingsTmp$pca.sd
  }
  # str(loads)
  # t(parmsScaled) %*% loads
  # parmsScaled %*% t(loads)
  parmsReduced <- as.numeric(tcrossprod(parmsScaled, loads))  # <-- must be transposed version!
  names(parmsReduced) <- paste0("PC", seq_along(parmsReduced))
  parmsReduced
}
