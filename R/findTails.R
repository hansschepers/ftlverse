#' Detect NA values at the beginning or end of a vector
#'
#' Returns a vector with boolean values for NA's but only if they are at the
#' beginning or the end.
#'
#' @examples \dontrun{
#'   x <- c(NA, NA, 5, 3, 1, NA, NA, NA, 2, 12, NA, 0, NA)
#'   NATails(x)
#'   isTails(x)
#'   findTails(x)
#'   findTails(x, suspect = c(0, NA, NaN))
#'   isTails(x, suspect = c(0, NA, NaN))
#' }
#'
#' @export
NATails <- function(x){
  is.na(zoo::na.approx(zoo::zoo(x), na.rm = FALSE))
}


#' middleNAs
#' @export
middleNAs <- function(x, ...){
  is.na(findTails(x, repl = 1, ...))
}


#' isTails
#'
#' @export
isTails <- function(x, ...){
  res <- rep(FALSE, length(x))
  res[findTails(x, ...)] <- TRUE
  res
}

#' firstNonNA
#'
#' @export
firstNonNA <- function(x
                       , suspect = c(NA, NaN)){
  which(!x %in% suspect, arr.ind = TRUE)[1]
}


#' firstNonNAValue
#'
#' @export
firstNonNAValue <- function(x
                       , suspect = c(NA, NaN)){
  x[which(!x %in% suspect, arr.ind = TRUE)[1]]
}

#' lastNonNA
#'
#' @export
lastNonNA <- function(x
                      , suspect = c(NA, NaN)){
  length(x) - firstNonNA(rev(x), suspect = suspect) + 1
}

#' lastNonNAValue
#'
#' @export
lastNonNAValue <- function(x
                      , suspect = c(NA, NaN)){
  x[length(x) - firstNonNA(rev(x), suspect = suspect) + 1]
}


#' findTails
#' @examples \dontrun{
#'   x <- c(0, 3, 1, NA, Inf, NA, NA, 2, 12, NA)
#'   findTails(x, suspect = 0, which = "both")
#'   findTails(x, suspect = c(0, 1, NA), which = "both")
#'   findTails(x, which = "none")
#'   findTails(x, which = "none", repl = 88)
#'   findTails(x, suspect = c(0, 1, NA), which = "both")
#'   findTails(x, suspect = c(0, 3, 1, NA), which = "both")
#'   findTails(x, suspect = c(0, 3, NA), which = "both")
#'   findTails(x, suspect = c(0, 3, NA), which = "left")
#'   findTails(x, suspect = c(0, 3, NA), which = "right")
#'   findTails(x, suspect = 1, which = "both")
#'
#'   findTails(x, repl = 999)
#'   findTails(x, repl = "text")                                 # beware, converts all to character...
#'   findTails(x, suspect = 0, which = "both", repl = "first")
#'   findTails(x, suspect = 0, which = "both", repl = "last")
#'   findTails(x, which = "both", repl = "last")
#'   findTails(x, which = "both", repl = "first")
#'   findTails(x, suspect = c(0, NA), which = "both", repl = "first")
#'
#'   findTails(x, suspect = 1, which = "both", repl = 999)      # suspect is not found
#'   findTails(x, suspect = 1, which = "both", repl = "jj")     # suspect is not found
#'
#'   findTails(x, suspect = c(0, 3, NA), which = "both", repl = 0)
#'   findTails(x, suspect = c(0, 3, NA), which = "both", countOnly = TRUE)
#'   findTails(x, suspect = c(0, 3, NA), which = "left", countOnly = TRUE)
#'   findTails(x, suspect = c(0, 3, NA), which = "right", countOnly = TRUE)
#'   findTails(1, countOnly = TRUE)
#'   findTails(NULL, countOnly = TRUE)
#'   findTails(numeric(0), countOnly = TRUE)
#'   findTails(x=NA, countOnly = TRUE)
#'   findTails(x=c(NA, NA), countOnly = TRUE)
#'   findTails(x=c(NA, NA, 3, 7, 1, 5, NA, NA, NA), countOnly = TRUE)
#'   xMiddleSignal <- c(3, 7, NA, 1, 5)
#'   hfrollmean(xMiddleSignal, n = 3)
#'   x <- c(NA, 0, 0, xMiddleSignal, NA, NA, NA)
#'   x
#'   findTails(x, suspect = c(NA, 0), FUN = hfrollmean, funArgs = list(n = 3))
#' }
#' @export
findTails <- function(x
                      , suspect = c(NA, NaN)
                      , which = c("left", "right", "both", "none")[3]
                      , countOnly = FALSE
                      , InfAsNA = TRUE
                      , repl = NULL
                      , info = character(0)
                      , FUN = NULL
                      , funArgs = list()
){
  if (length(info)){
    cat(info)
    str(x)
  }
  if (is.null(x)){
    if (countOnly) {
      return(0)
    } else {
      return(NULL)
    }
  }
  if (length(x) < 1){
    if (countOnly) {
      return(0)
    } else {
      return(NULL)
    }
  }
  if ("none" %in% which){
    if (countOnly) {return(0)}
    if (is.null(repl)) {
      return(integer(0))
    } else {
      return(x)
    }
  }
  res <- structure(c(NA)[0], class = class(x))
  if (InfAsNA){
    x[is.infinite(x)] <- NA
  }
  if (which %in% c("left", "both")){
    firstnonna <- firstNonNA(x, suspect = suspect)
    if(is.na(firstnonna)) firstnonna <- length(x) + 1
    if (firstnonna > 1){
      res <- union(res, 1:(firstnonna - 1))
    }
  }
  if (which %in% c("right", "both")){
    x <- rev(x)
    firstnonna <- firstNonNA(x, suspect = suspect)
    if(is.na(firstnonna)) firstnonna <- length(x) + 1
    if (firstnonna > 1){
      res <- union(res, length(x) + 1 - rev(1:(firstnonna - 1)))
    }
    x <- rev(x)
  }

  # replace tails with something ----
  if (!is.null(repl)){
    if (!length(res)) return(x)
    if (!is.na(repl)){
      if (repl[1] == "first"){
        firstnonna <- x[firstNonNA(x, suspect = suspect)]
        repl <- firstnonna
      }
      if (repl[1] == "last"){
        firstnonna <- rev(x)[firstNonNA(rev(x), suspect = suspect)]
        repl <- firstnonna
      }
    }
    x[res] <- repl[1]
    return(x)
  }

  # work on tails ----
  if (!is.null(FUN)){
    res <- seq_along(x) %in% res
    funArgs <- c(list(x[!res]), funArgs)
    x[!res] <- do.call(FUN, funArgs)
    return(x)
  }

  # return the length of the suspect elements ----
  if (countOnly){
    res <- length(res)
  }

  # return the positions of the suspect elements ----
  return(res)
}


#' leftNAcount
#' @export
leftNAcount <- function(x, ...) findTails(x, countOnly = TRUE, which = "left", ...)

#' rightNAcount
#' @export
rightNAcount <- function(x, ...) findTails(x, countOnly = TRUE, which = "right", ...)

#' leftZerocount
#' @export
leftZerocount <- function(x, ...) findTails(x, suspect = 0, countOnly = TRUE, which = "left", ...)

#' rightZerocount
#' @export
rightZerocount <- function(x, ...) findTails(x, suspect = 0, countOnly = TRUE, which = "right", ...)

#' notSuspect
#' @export
notSuspect <- function(x, suspect = c(0, NA, NaN), which = "both", ...) {
  length(x) - findTails(x, suspect = suspect, countOnly = TRUE, which = which
                        , ...
  )
}

