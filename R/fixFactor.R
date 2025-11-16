#' fixFactor
#' @examples \dontrun{
#'   fixFactor(x = c("B", "C", "A"))
#'   fixFactor(c("B", "C", "A"), xlevels = LETTERS[3:1])
#'   fixFactor(c("B", "C", "A"), xlevels = "C")
#'   # on a list
#'   fixFactor(list(c("B", "C", "A"), c("A", "C", "A")), xlevels = LETTERS[3:1])
#' }
#' @export
fixFactor <- function(x
                      , xlevels = unique(unlist(x))
                      , keepAll = TRUE
                      , ordered = TRUE) {
  
  if (inherits(x, "list")){
    ###################################################### RECURSIVE IF A LIST
    res <- lapply(x
                  , fixFactor
                  , xlevels = xlevels
                  , keepAll = keepAll
                  , ordered = ordered)
  } else {
    if (keepAll){
      unMentioned <- setdiff(unique(unlist(x)), xlevels)
      xlevels <- c(xlevels, unMentioned)
    }
    res <- factor(x
                  , levels = xlevels
                  , ordered = ordered)
  }
  res
}



#' fixFactorDF
#' 
#' @export
fixFactorDF <- function(dfg
                        , xx
                        , desc = FALSE
                        ) {
  wasDT <- data.table::is.data.table(dfg)
  dfg <- as.data.frame(dfg)
  for(x in xx) {
    if (desc){
      dfg[, x] <- factor(dfg[, x], levels = rev(unique(dfg[, x])), ordered = TRUE)
    } else {
      dfg[, x] <- factor(dfg[, x], levels = unique(dfg[, x]), ordered = TRUE)
    }
  }
  if (wasDT) data.table::setDT(dfg)
  dfg
}
