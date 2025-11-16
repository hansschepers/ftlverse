#' treatOutlier QA for a vector (e.g. inside a data.table)
#'
#' @param x `numeric()` A vector to check for outliers
#' @param coef `numeric()` A scalar to be used as coefficient in the selected
#'   method
#' @param method `character(1)` The method uesed to detect the outliers
#' @param action `character(1)` What to do with outliers
#' @param direction Should the outliers be treated at the low side `-1`, both
#'   directions `0` or for large values `1`
#' @param info `character(1)` string to be used in verbose output
#' @param infoWidth `numeric(1)` argument to be used in verbose output
#' @param verbosity `numeric(1)` should outlier summary be displayed in console
#'
#' @examples \dontrun{
#'   library(data.table)
#'   # ok to use with Real Outliers...
#'   dtTmp <- data.table(value = c(1,200,3,4, NA, 5, -600, 8))
#'   dtTmp[, value := treatOutlier(value)]
#'   dtTmp
#'
#'   # NOT ok to use without Real Outliers...
#'   dtTmp <- data.table(value = c(1,3,4, NA, 5, 6))
#'   dtTmp[, value := treatOutlier(value)]
#'   dtTmp
#' }
#'
#' @importFrom zoo na.approx zoo
# @importFrom grDevices boxplot.stats
# @importFrom stats quantile sd
#' @export
treatOutlier <- function(
    x
    , method = c("none", "boxstats", "sds", "quantile", "asym"
                 , "refstats"
    )
    , coef = 0.02
    , action = c("NA", "maxmin", "NAfilled")[3]
    , deriv = 0
    , direction = 0
    , LB = -Inf, UB = Inf
    , RS = data.table()
    , scope = "global"
    , procName = "drain"
    , info = ":|:"
    , infoWidth = 20
    , verbosity = 0
){
  lb <- -Inf
  ub <- Inf
  if (info != ":|:"){
    info <- format(info, width = infoWidth, justify = "right")
  }
  
  xorig <- x
  for (ii in seq_len(deriv)){
    x <- diff1(x)
  }
  
  if (grepl("refstats", method, ignore.case = TRUE)){
    RS <- as.data.table(RS)
    RS <- RS[RSscope %in% scope]#[, RSscope := NULL]
    RS <- RS[processName == procName][1]  # or last??
    if (!nrow(RS)){
      log_warn("RS has no rows left, nothing done")
      lb <- -Inf
      ub <- Inf
    } else {
      lb <- RS$RSmin
      ub <- RS$RSmax
    }
  }
  
  if (grepl("boxstats", method, ignore.case = TRUE)){
    bb <- boxplot.stats(x, do.out=TRUE, coef=coef)
    lb <- bb[["stats"]][1]
    ub <- bb[["stats"]][5]
  }
  if (grepl("quantile", method, ignore.case = TRUE)){
    lb <- quantile(x, probs=  coef, na.rm=TRUE)
    ub <- quantile(x, probs=1-coef, na.rm=TRUE)
  }
  if (grepl("sds", method, ignore.case = TRUE)){
    lb <- mean(x, na.rm=TRUE) - sd(x, na.rm=TRUE) * coef
    ub <- mean(x, na.rm=TRUE) + sd(x, na.rm=TRUE) * coef
  }
  
  if (grepl("asym", method, ignore.case = TRUE)){
    sk <- hskewness(x)
    wid <- hmadmean(x)
    logger::log_trace("sk {round(sk,2)}, wid = {round(wid,2)}")
    
    lb <- max(min(x, na.rm = TRUE), mean(x, na.rm = TRUE) - sqrt(abs(-1 - min(0, sk))) * wid * coef)
    ub <- min(max(x, na.rm = TRUE), mean(x, na.rm = TRUE) + sqrt(     1 + max(0, sk))  * wid * coef)
  }
  if (!grepl("none", method, ignore.case = TRUE)) {
    lb <- max(lb, LB)
    ub <- max(lb, UB)
  } else {
    lb <- LB
    ub <- UB
  }
  if (!exists("lb")) {message("unrecognized method") ; return(xorig)}
  
  toolow <-  x < lb
  toohigh <- x > ub
  x <- xorig
  
  if (direction ==  1) toolow  <- NULL
  if (direction == -1) toohigh <- NULL
  
  if (action == "maxmin"){
    x[toolow] <- pmax(x, lb)
    x[toohigh] <- pmin(x, ub)
  }
  if (action == "NA"){
    x[toolow] <- NA
    x[toohigh] <- NA
  }
  
  if (action %in% c("NAfilled", "NAfill")){
    x[toolow] <- NA
    x[toohigh] <- NA
    x <- interNAZoo(x)
  }
  
  if (verbosity > 0){
    info <- paste(c(info, "lb: ", round(lb, 2), ":  ", sum(toolow, na.rm = TRUE),
                    ", ub: ", round(ub, 2), ":  ", sum(toohigh, na.rm = TRUE)), collapse = " ")
  }
  return(x)
}
