#' pca2par
#' transforms back parameters from 'PCA space' to 'original space'
#' @export
pca2par <- function(parmsReduced
                    , loads
                    , parScalings
                    , dims = min(length(parmsReduced), nrow(parScalings))
                    # , lower = NULL
                    # , upper = NULL
){
  parmsReduced <- unlist(parmsReduced)
  log_trace("dims: {dims}")
  if (dims < length(parmsReduced)){
    if (length(parmsReduced) == nrow(parScalings)){
      parmsReduced[seq(dims+1, length(parmsReduced))] <- 0
    }
    if (length(parmsReduced) == dims){
      parmsReduced <- c(parmsReduced, rep(0, nrow(loads)-dims))
    }
  }
  log_trace("dims: {dims}")
  # str(parmsReduced)
  
  ok <- all.equal(parScalings$parameter, row.names(loads))
  log_trace("names same {ok}")
  
  parScalingsTmp <- as.data.frame(parScalings)
  row.names(parScalingsTmp) <- parScalingsTmp$parameter
  parScalingsTmp <- parScalingsTmp[row.names(loads),]
  # parScalingsTmp <- parScalings[parameter %in% row.names(loads),]
  # parmsReduced <- parmsReduced[seq(dims)]
  # loads <- loads[, seq(dims), drop = FALSE]
  parmsTmp <- as.list(as.data.frame(
    parScalingsTmp$pca.center + 
      parScalingsTmp$pca.sd * tcrossprod(parmsReduced, t(loads)) # 20241108 ERROR with t() added
    ))
  # if (!is.null(lower)){
  #   parmsTmp <- pmax(parmsTmp, lower)  
  # }
  # if (!is.null(upper)){
  #   parmsTmp <- pmin(parmsTmp, upper)  
  # }
  names(parmsTmp) <- parScalingsTmp$parameter
  parmsTmp
}

