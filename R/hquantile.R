#' hquantile
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile <- function(x, probs = seq(0, 1, 0.25), na.rm = TRUE
                      , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  unname(quantile(x, probs = probs, na.rm=na.rm, names = names, type = type))
}


#' hquantile99
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile99 <- function(x, probs = 0.99, na.rm = TRUE
                        , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  hquantile(x, probs = probs, na.rm=na.rm, names = names, type = type)
}

#' hquantile95
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile95 <- function(x, probs = 0.95, na.rm = TRUE
                        , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  hquantile(x, probs = probs, na.rm=na.rm, names = names, type = type)
}


#' hquantile75
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile75 <- function(x, probs = 0.75, na.rm = TRUE
                        , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  hquantile(x, probs = probs, na.rm=na.rm, names = names, type = type)
}


#' hquantile25
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile25 <- function(x, probs = 0.25, na.rm = TRUE
                        , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  hquantile(x, probs = probs, na.rm=na.rm, names = names, type = type)
}


#' hquantile05
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile05 <- function(x, probs = 0.05, na.rm = TRUE
                        , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  hquantile(x, probs = probs, na.rm=na.rm, names = names, type = type)
}


#' hquantile01
#' modified default version of base::
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hquantile01 <- function(x, probs = 0.01, na.rm = TRUE
                        , names = TRUE, type = 7, ...) {
  x <- hna.omit(x)
  if (!length(x) > 0) return(NA_real_)
  hquantile(x, probs = probs, na.rm=na.rm, names = names, type = type)
}
