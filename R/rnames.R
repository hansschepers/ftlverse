#' rnames
#' get name of a value x, based on a named vector v
#' @examples \dontrun{
#'   rnames(3, setNames(1:26, paste("name", LETTERS)))
#' }
#' @export
rnames <- function(x, v){
  names(v)[which(v == x)]
}
