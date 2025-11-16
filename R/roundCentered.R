#' roundCentered
#' @examples \dontrun{
#'   op <- options(digits = 6)
#'   x <- c(seq(0, 2.5, .1), NA, Inf)
#'   round(x)
#'   roundCentered(x, shiftStep = 1)
#'   roundCentered(x, shiftStep = 2)
#'   roundCentered(x, shiftStep = 3)
#'   roundCentered(x, shiftStep = 4)
#'   
#'   roundCentered(x, shiftStep = 1, centered = FALSE)
#'   roundCentered(x, shiftStep = 2, centered = FALSE)
#'   roundCentered(x, shiftStep = 3, centered = FALSE)
#'   roundCentered(x, shiftStep = 4, centered = FALSE)
#'   roundCentered(-x, shiftStep = 1)
#'   roundCentered(-x, shiftStep = 2)
#' }
#' @export
roundCentered <- function(x
                          , shiftStep = 2
                          , centered = TRUE
                          , digits = 0){
  round(x * shiftStep + centered/(2*shiftStep), digits = digits) / 
    shiftStep - centered/(2*shiftStep)
}
