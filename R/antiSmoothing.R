#' antiSmoothing
#' @examples \dontrun{
#'   antiSmoothing(y = runif(40), doplot = TRUE)
#' }
#' @export
antiSmoothing <- function(y = runif(40)
                          , x = seq_along(y)
                          , f = .2
                          , doplot = FALSE){
  ysm <- lowess(x, y, f = f)$y
  res <- y - ysm
  yasm <- ysm + res * 2
  if (doplot){
    plot(x, y, ylim = c(-1, 2))
    lines(x, ysm, col = 3)
    points(x, yasm, col = 4)
  }
  # hMAPE(y, yasm)
  # hMAPE(y, ysm)
  # sd(y)
  # sd(ysm)
  # sd(yasm)
  return(yasm)
}
