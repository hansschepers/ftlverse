#' wrapPaste
#' 
#' @export
wrapPaste <- function(... , width = floor(0.9 * getOption("width")) ){
  txt <- paste(...)
  paste(stringi::stri_wrap(txt, width = width), collapse = "\n")
}

