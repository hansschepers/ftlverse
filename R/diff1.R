#' diff0
#' @description as diff(), but prepends a 0
#' @export
diff0 <- function(x, na.rm = TRUE) {
  c(0, diff(x))
}


#' diff1
#' @description as diff(), but prepends the difference between the first two values
#' 
#' @export
diff1 <- function(x, na.rm = TRUE) {
  c(x[2] - x[1], diff(x))
}


#' diffv1
#' @description as diff(), but prepends the first value (to be inverse of cumsum or aphCumsum)
#' @examples \dontrun{
#'   x <- c(3,6,1,8,8,3,0,2)
#'   x2 <- diffv1(cumsum(x))
#'   all.equal(x, x2)
#'   
#'   diffv1(aphCumsum(x))
#'   aphCumsum(diffv1(x))
#'   cumsum(diffv1(x))
#' }
#' @export
diffv1 <- function(x, na.rm = TRUE) {
  c(x[1], diff(x))
}

