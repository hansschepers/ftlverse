#' lzman
#' @examples \dontrun{
#'   x <- c(NA, NA, 1, NA, 0, 7, 5, NA, NA, NA)
#'   frollmeanMirror(x, 1)
#'   frollmeanMirror(x, 2)
#'   lzman(frollmeanMirror(x, 2))
#'   frollmean(x, 3)
#'   frollmean(x, 3, align = "center")
#'   lzman(x)
#'   lzman(x, 2)
#'   lzman(x, n = 3)
#'   lzman(x, n = 4)
#' }
#' @export
lzman <- function(x, n = 1){
  if (n > 1) {
    x <- frollmeanMirror(x, n = n)
  }
  findTails(x, which = "right", repl = "last")
}
