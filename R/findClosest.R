#' findClosest
#' @examples \dontrun{
#'   findClosest(x = 3, vec = c(1,2,6, 11, 3.1, NA, 1))
#' }
#' @export
findClosest <- function(x
                        , vec
                        , multiplier = 1
                        , returnX = FALSE){
  if (returnX) {
    if (multiplier > 0){
      return(x * multiplier)
    } else {
      return(x)
    }
  }
  d <- abs(x*multiplier - vec)
  vec[which(d == hmin(d))]
}
