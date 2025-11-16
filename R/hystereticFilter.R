#' hystereticFilter
#' @examples \dontrun{
#'   x <- c(1,2,NA,4,5,6,7,8,9,8,7,NA,5,4,3,NA,1)
#'   hystereticFilter(x, tUpper = 7, tLower = 3)
#'   [1]  1  2 NA  4  5  6 NA NA NA NA NA NA NA NA  3 NA  1
#' }
#' @param x numeric vector
#' @param tUpper threshold to start NAs when 'going up'
#' @param tLower threshold to stop NAs when 'going down'
#' @export
hystereticFilter <- function(x, tUpper, tLower){
  if (!is.na(x[1])) {
    flag <- ifelse(x[1] >= tUpper, TRUE, FALSE)
  } else flag = FALSE
  for (i in 2:length(x)) {
    if (!is.na(x[i])) {
      flag <- ifelse(x[i] >= tUpper, TRUE, flag)
      flag <- ifelse(x[i] <= tLower, FALSE, flag)
      if (x[i] > tLower & x[i] < tUpper & flag) {
        x[i] = ifelse(x[i] < x[i - 1], NA, x[i])
      }
    }
  }
  x[which(x >= tUpper)] <- NA
  return(x)
}
