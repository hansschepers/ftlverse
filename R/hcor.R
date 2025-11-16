#' hcor
#' @examples \dontrun{
#'   dd <- hcor(1:10, 10:1)
#'   dd <- hcor(data.table(x1 = 1:10, x2 = (10:1)^2))
#'   dd
#'   solve(dd)
#'   det(dd)
#'   det(solve(dd))
#' }
#' 
#' @export
hcor <- function(x, y = NULL
                 , DT = NULL
                 , use = "pairwise.complete.obs"
                 , FUN = "cor"
                 , ...){
  if (!is.null(DT)){
    x = DT[[x]]
    y = DT[[y]]
  }
  if (is.data.table(x)){
    ok <- aphVariableLevels(x)
    x <- x[, ..ok]
    x <- cleanSD0(x)
  }
  do.call(get(FUN), list(x = x, y = y, use = use, ...))
}