#' quantile_cu
#' @examples \dontrun{
#'   x <- c(1:9, seq(10, 15, by = 0.5))
#'   x <- c(1, 3, 5, 2, 7, 6, 7, 8)
#'   hsum(x[1:2]) / hsum(x) / 0.25
#'   hmean(x[1:4]) / hmean(x[5:8])
#'   hmean(x[1:4]) / hmean(x)
#'   hmean(x[1:2]) / hmean(x)
#'   hmean(x[1:6]) / hmean(x)
#'   quantile_cu(x)
#'   quantile_cu(x, isCumulative = T)
#'   quantile_cu(rev(x))
#' }
#' @export
quantile_cu <- function(x
                        , fracs = c(0.25, 0.5, 0.75)[2]
                        , isCumulative = FALSE
                        # , isCumulative = all(diff(interpolateLinearly(x)) >= 0)
){
  if (isCumulative){
    x.cu <- x
  } else {
    x.cu <- aphCumsum(x)
  }
  x.cu01 <- scale01(x.cu)
  i_qu <- seq_along(x)
  i_qu <- floor(quantile(i_qu, fracs))
  # str(i_qu)
  x.cu01[i_qu] / fracs
}



#' early
#' @examples \dontrun{
#'   x <- c(1, 3, 5, 2, 7, 6, 7, 8)
#'   hmean(x[1:4]) / hmean(x[5:8])
#'   early(x)
#'   1/early(x)
#'   early(rev(x))
#' }
#' @export
early <- function(x, fracs = 0.5){
  i_qu0 <- seq_along(x)
  i_qu <- seq(floor(quantile(i_qu0, fracs)))
  hmean(x[i_qu]) / hmean(x[setdiff(i_qu0, i_qu)])
}
