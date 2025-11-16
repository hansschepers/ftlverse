#' fracShift
#' @examples \dontrun{
#'   fracShift(5, n=1.3)
#'   fracShift(5, n=1.2)
#'   fracShift(5, n=5.2)
#'   fracShift(5, n=5.5)
#'   x <- c(1,2,3,4,3,2,5,7,9,7,5,4,3)
#'   fracShift(x, n = 3, fill = 0)
#'   fracShift(x, n = 1.2)
#'   fracShift(x, n = 0.2)
#'   fracShift(x, n = -0.2)
#'   fracShift(x, n = -1.96)
#'   fracShift(x, n =  1.96)
#'   
#'   for (n in seq(-2, 2, 0.04)) {
#'     plot(x, type = "b", main = n, xlim = c(0, 30), ylim = c(0, 10))
#'     (y <- fracShift(x, n))
#'     points(y, col = 2, type = "b")
#'     Sys.sleep(.1)
#'   }
#' }
#' @importFrom data.table shift
#' @export
fracShift <- function(x, n, ...){
  # n <- replaceNa(n, 0)
  n[is.na(n) | is.infinite(n)] <- 0
  
  x <- c(x, rep(0, max(ceiling(abs(n)))))
  x1 <- data.table::shift(x, n = trunc(n), ...)
  x2 <- data.table::shift(x, n = trunc(n + sign(n)), ...)
  x2[is.na(x2)] <- 0
  
  fr <- n - trunc(n)
  fr <- abs(fr)
  
  res <- (1 - fr) * x1
  if (fr > 0){
    res <- res + fr * x2
  }
  res
}
