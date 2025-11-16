#' hna.locf
#' @param x [vector] potentially containing leading and trailing NA's
#' @return [vector] with leading NA 's recoded to 0 and trailing NA's replaced by last non-NA value.
#' @importFrom zoo na.locf
#' @export
hna.locf <- function(x, fillLeftNA = FALSE) {
  x <- zoo::na.locf(x, na.rm = FALSE)
  if(fillLeftNA) {
    x[is.na(x)] <- 0
  }
  return(x)
}


#' keepCumOnMax
#'
#' Cumulatives should never decrease (if the underlying origin is known to be positive..)
#' picks first Maximum, not absolute maximum!
#'
#' @examples \dontrun{
#'   x <- c(NA, 3,4,NA, 6,8,7,6,9,5,NA)
#'   hna.locf(x)
#'   hna.locf(x, fillLeftNA = TRUE)
#'   keepCumOnMax(x)
#'   keepCumOnMax(x, locf = FALSE)
#' }
#' @export
keepCumOnMax <- function(x, locf = TRUE){
  dip <- which(diff1(x) < 0)[1]
  if (!is.na(dip)) {
    if (log_threshold() > 4000){
      message(4)
      print(dip)
      print(x)
      print(x[dip])
      print(x[dip-1])
    }
    if (dip == 1){
      rrr <- x[dip]
    } else {
      rrr <- x[dip-1]
    }
    x[seq(dip, length(x))] <- rrr
  }
  if(locf){
    x <- zoo::na.locf(x, na.rm = FALSE)
    x[is.na(x)] <- 0
  }
  x
}

#' keepCumulativeIncreasing
#'
#' Cumulatives should never decrease (if the underlying origin is known to be positive..)
#' picks first Maximum, not absolute maximum!
#'
#' @examples \dontrun{
#'   x <- c(NA, 3,4,NA, 6,8,7,6,9,5,NA)
#'   hna.locf(x)
#'   hna.locf(x, fillLeftNA = TRUE)
#'   keepCumOnMax(x)
#'   keepCumOnMax(x, locf = FALSE)
#'   keepCumulativeIncreasing(x)
#'   keepCumulativeIncreasing(x, 0)
#'   # not working: keepCumulativeIncreasing(x, NA)
#' }
#' @export
keepCumulativeIncreasing <- function(x, LeftNA = 0){
  x2 <- x
  # .x <<- x
  x2 <- findTails(x2, which = "left", repl = LeftNA)
  x2[diff0(x2) < 0] <- NA
  x2 <- zoo::na.locf(x2, na.rm = FALSE)
  x2 <- keepCumOnMax(x2)
  # .x2 <<- x2
  x2
}

