#' htimestamp
#' @examples \dontrun{
#'   htimestamp()
#'   htimestamp("%H%M%OS3")
#'   htimestamp("%Y%W")
#' }
#' @export
htimestamp <- function(fo = "%Y%m%d_%H%M%OS"
                       , pref = character(0)
                       , form = paste0(pref, fo)
                       , digits.secs = 2
                       ){
  getOption("digits.secs")
  # .oldDigitsSecs <<- options(digits.secs = digits.secs)
  res <- format(Sys.time(), form)
  res <- sub("\\.", "_", res)
  options(digits.secs = NULL)
  res
}


#' hdatestamp
#' 
#' @examples \dontrun{
#'   hdatestamp()
#' }
#' @export
hdatestamp <- function(fo = "%Y%m%d", ...){
  htimestamp(fo = fo, ...)
}
