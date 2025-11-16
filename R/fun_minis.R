#' hna.omit
#' @export
hna.omit <- function(x, ...){
  x[!is.na(x)]
  # na.omit(x, ...) 
}
# hna.omit <- function(x, ...){ x }


#' hfirst
#' 
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hfirst <- function(x, na.rm = TRUE, makeNumeric = FALSE, ...) {
  x <- unlist(x)
  if (makeNumeric) {
    x <- as.numeric(x)
  }
  if (!length(x) > 0) {
    return(NA_real_)
  }
  x[1]
}

#' hlast
#' 
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hlast <- function(x, na.rm = TRUE, makeNumeric = FALSE, ...) {
  x <- unlist(x)
  if (makeNumeric) {
    x <- as.numeric(x)
  }
  if (!length(x) > 0) {
    return(NA_real_)
  }
  x[length(x)]
}


#' hvalid
#' @examples \dontrun{
#'   x <- c(NA, NA, 1,3, NA, NaN, Inf, 2, NA)
#'   hvalid(x)
#'   hmean(hvalid(x))
#' }
#' @export
hvalid <- function(x, lb = -Inf, ub = Inf){
  x[(x < lb) | (x > ub) | his.na(x)] <- NA
  x
}


#' hwday
#' @importFrom lubridate wday
#' @export
hwday <- function(...){
  res <- lubridate::wday(...)
  unlist(res)
}


#' hwdayDiffrange
#' @importFrom lubridate wday
#' @export
hwdayDiffrange <- function(...){
  res <- lubridate::wday(...)
  hdiffrange(res)
}


#' hwdayMean
#' @importFrom lubridate wday
#' @export
hwdayMean <- function(...){
  res <- lubridate::wday(...)
  hmean(res)
}




#' hmean
#' 
#' modified default version of base::
#' 
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hmean <- function(x, na.rm = TRUE, makeNumeric = FALSE, trim = 0, ...) {
  x <- unlist(x)
  if (makeNumeric) {
    x <- as.numeric(x)
  }
  x <- hna.omit(x)
  x[is.infinite(x)] <- NA
  if (!length(x) > 0) {
    return(NA_real_)
  }
  mean(x, na.rm = na.rm, trim = trim)
}


#' hmin
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hmin <- function(x, na.rm = TRUE, keepInf = TRUE, ...) {
  x <- hna.omit(x)
  if (!keepInf){
    x[is.infinite(x)] <- NA
  }
  if (!length(x) > 0) {
    return(NA_real_)
  }
  res <- min(x, na.rm = na.rm)
  if (is.infinite(res)){
    res <- NA_real_
  }
  res
}


#' hmax
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hmax <- function(x, na.rm = TRUE, keepInf = TRUE, ...) {
  # if (is.numeric(x)) 
  x <- hna.omit(x)
  if (!keepInf){
    x[is.infinite(x)] <- NA
  }
  if (!length(x) > 0) {
    return(NA_real_)
  }
  res <- max(x, na.rm=na.rm)
  if (is.infinite(res)){
    res <- NA_real_
  }
  res
}


#' replaceNa
#' 
#' @export
replaceNa <- function(x, repl = 1){
  x[his.na(x)] <- repl
  x
}


#' sumna
#' counts NA
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
sumna <- function(x, na.rm = TRUE, ...) {
  sum(his.na(x))
}


#' suminf
#' counts Inf and -Inf
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
suminf <- function(x, na.rm = TRUE, ...) {
  sum(is.infinite(x))
}

#' hI
#' counts Inf and -Inf
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hI <- suminf


#' okData
#' 
#' @export
okData <- function(x, ...) {
  if (length(x) == 0) {
    return(0)
  }
  round(sum(!his.na(x))/length(x) * 100)
}


#' sum0
#' counts zeros (0)
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
sum0 <- function(x, na.rm = TRUE, ...) {
  sum(x == 0, na.rm = TRUE)
}



#' his.nan
#' 
#' @export
his.nan <- function(x){
  if (length(x) == 0) return(logical(0))
  sapply(seq_along(x), function(i) any(is.nan(unlist(x[i]))))
}


# beware with '%in%':
# > is.na(NaN)  # NaN is a subset of NA
# [1] TRUE
# > NA %in% NaN 
# [1] FALSE

#' his.na
#' 
#' NaN occurs with mean(NA, na.rm = TRUE) or mean(numeric(0)) or 0/0 or sin(Inf)
#' @examples \dontrun{
#'   his.na(x = 1:4)
#'   his.na(x = NA)
#'   his.na(x = c(NA, 1:4))
#'   his.na(x = numeric(0))
#'   his.na(x = c(Inf, 1:4))
#'   his.na(x = c(Inf, 1:4), InfAsNA = FALSE)
#'   his.na(x = list(a=NA, b = 1:4))
#'   his.na(x = list(a=2, b = c(Inf, 1:4)))
#' }
#' 
#' @export
his.na <- function(x, InfAsNA = TRUE){
  
  if (sum(is.na(x)) == length(x)) return(rep(TRUE, length(x)))
  if (length(x) == 0) return(logical(0))
  res <- sapply(seq_along(x), \(i) {
    rr <- any(is.na(unlist(x[i])))
    if (InfAsNA){
      rr <- rr | is.infinite(unlist(x[i])  )
    }
    rr
  }
  )
  res <- unlist(res)
  res
}


#' hmedian
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hmedian <- function(x, ...) {
  x[is.infinite(x)] <- NA
  median(x, na.rm = TRUE)
}


#' hsd
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hsd <- function(x, timeUnitLength = 3600*24, ...){
  if (!inherits(x, c("numeric", "integer", "double", "POSIXct", "POSIXt", "Date"))) {
    return(NA)
  }
  x[is.infinite(x)] <- NA
  res <- sd(x, na.rm = TRUE)
  if (length(x) == 1 & his.na(res[1])){
    res <- 0
  }
  if (sum(c("POSIXct", "POSIXt") %in% class(x))) {
    res <- res / timeUnitLength
  }
  res
}


#' hscale
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hscale <- function(x, timeUnitLength = 3600*24, trim = 0, ...){
  (x - hmean(x, trim = trim)) / hsd(x, timeUnitLength = timeUnitLength)
}


#' hmad
#' 
#' mean average distance to the median
#' 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hmad <- function(x, na.rm = TRUE, ...) {
  x[is.infinite(x)] <- NA
  mad(x, na.rm=na.rm)
}


#' hmadmean
#' 
#' mean average distance to the mean
#' 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hmadmean <- function(x, na.rm = TRUE, trim = 0, ...) {
  x[is.infinite(x)] <- NA
  res <- mad(x, na.rm=na.rm, center = hmean(x, trim = trim))
  if (sum(c("POSIXct", "POSIXt") %in% class(x))) {
    res <- as.numeric(res, units = "days")
  }
  res
}


#' hlb
#' 
#' @export
hlb <- function(x, coef = 5, ...) {
  max(hmin(x), hmean(x) - sqrt(abs(-1 - min(0, hskewness(x)))) * hmadmean(x) * coef)
}

#' hlb
#' 
#' @export
hcutLow <- function(x, ...) {
  lb <- hlb(x, ...)
  hsum(x < lb)
}


#' hub
#' 
#' @export
hub <- function(x, coef = 5, ...) {
  min(hmax(x), hmean(x) + sqrt(     1 + max(0, hskewness(x)))  * hmadmean(x) * coef)
}


#' hub
#' 
#' @export
hcutHigh <- function(x, ...) {
  ub <- hub(x, ...)
  hsum(x > ub)
}


#' hTrim
#'  #copy from DescTools :: Trim
#' @export
hTrim <- function (x, trim = 0.1, na.rm = FALSE, ...) {
  if (na.rm) 
    x <- x[!his.na(x)]
  if (!is.numeric(trim) || length(trim) != 1L) 
    stop("'trim' must be numeric of length one")
  n <- length(x)
  if (trim > 0 && n) {
    if (is.complex(x)) 
      stop("trim is not defined for complex data")
    if (anyNA(x)) 
      return(NA_real_)
    if (trim >= 0.5 && trim < 1) 
      return(NA_real_)
    if (trim < 1) 
      lo <- floor(n * trim) + 1
    else {
      lo <- trim + 1
      if (trim >= (n/2)) 
        return(NA_real_)
    }
    hi <- n + 1 - lo
    res <- sort.int(x, index.return = TRUE)
    trimi <- res[["ix"]][c(1:(lo - 1), (hi + 1):length(x))]
    x <- res[["x"]][lo:hi][order(res[["ix"]][lo:hi])]
    attr(x, "trim") <- trimi
  }
  return(x)
}


#' htsd
#' 
#' @export
htsd <- function(x, trim = 0, na.rm = TRUE, ...)  {
  sd(hTrim(x, trim=trim, na.rm=na.rm), na.rm=na.rm)
}


#' hsum
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hsum <- function(x, na.rm = TRUE, ...) {
  x <- unlist(x)
  x <- as.numeric(x)
  x <- hna.omit(x)
  x[is.infinite(x)] <- NA
  if (!length(x) > 0) return(NA_real_)
  sum(x, na.rm=na.rm)
}


#' hlength2
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hlength2 <- function(x
                     , y
                     , na.rm = TRUE, ...){
  hlength(x)
}


#' hlength
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hlength <- function(x, na.rm = TRUE, ...){
  if (na.rm){
    x <- x[!his.na(x)]
  }
  length(x)
}


#' huniqueN
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
huniqueN <- function(x) hlength(unique(x))


#' hN
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hN <- hlength


#' hrange
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hrange <- function(x, na.rm = TRUE, ...) {
  x[is.infinite(x)] <- NA
  range(x, na.rm=na.rm)
} 


#' hIQR
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hIQR <- function(x, na.rm = TRUE, ...) {
  x[is.infinite(x)] <- NA
  IQR(x, na.rm=na.rm, ...)
}


#' hskewness
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hskewness <- function(x, ...) {
  if (sum(c("POSIXct", "POSIXt") %in% class(x))) x <- as.numeric(x)
  if (!is.numeric(x)) {
    return(NA)
  }
  x[is.infinite(x)] <- NA
  skewness(x)
}


#' hkurtosis
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hkurtosis <- function(x, ...) {
  if (sum(c("POSIXct", "POSIXt") %in% class(x))) x <- as.numeric(x)
  if (!is.numeric(x)) {
    return(NA)
  }
  x[is.infinite(x)] <- NA
  kurtosis(x)
}

