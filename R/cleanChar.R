#' cleanChar
#' # used as 'cleanTitle'(defined there) in pggs()
#' @examples \dontrun{
#'   cleanChar("m")
#'   cleanChar(c("m", "k"))
#'   cleanChar("")
#'   cleanChar(character())
#' }
#' @export
cleanChar <- function(x
                      , collapse = c("\n", ", ")[1]
                      , repl = list(ggplot2::waiver(), NULL, "", " ")[[1]]
                      ){
  if (nchar(collapse) > 0){
    x <- paste(x, collapse = "\n")
  }
  if (length(x) == 0){
    x <- repl
  } else {
    if (nchar(x) == 0) x <- repl
  }
  x
}
