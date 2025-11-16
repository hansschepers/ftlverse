#' angleDiff
#' angular differencing, choosing smallest step comparing clock or counter clockwise
#' @examples \dontrun{
#'   angleDiff(c(1:22))
#'   angleDiff(c(22:1))
#' }
#' @export
angleDiff <- function(x){
  x <- x%%(2*pi) - pi
  
  res <- c(0, diff(x))
  
  altern <- res + 2*pi
  indexToReplace <- abs(res) > abs(altern)
  indexToReplace[is.na(indexToReplace)] <- FALSE
  # print(indexToReplace)
  res[indexToReplace] <- altern[indexToReplace]

  altern <- res - 2*pi
  indexToReplace <- abs(res) > abs(altern)
  indexToReplace[is.na(indexToReplace)] <- FALSE
  # print(indexToReplace)
  res[indexToReplace] <- altern[indexToReplace]
  
  res
}
