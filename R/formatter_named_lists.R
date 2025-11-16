#' Concatenate named lists into a character vector via paste
#'
#' @examples \dontrun{
#'   library(logger)
#'   log_formatter(formatter_glue_or_sprintf)
#'   log_formatter(formatter_named_lists, namespace = "named_lists")
#'   log_info("test {list(rr=4, k=21)}")
#'   log_info("test {ww}", list(rr=4, k=21))
#'   log_info("test {qq}", list(rr=4, k=21), namespace = "named_lists")
#'   log_threshold(INFO)
#' }
#' @param title title of the list
#' @param x named list to be concatenated
#' @param ... not used
#' @return string
#' @export
formatter_named_lists <- function(title, x, ...) {
  paste(title, paste(names(x), x, sep = ":", collapse = ", "))
}
