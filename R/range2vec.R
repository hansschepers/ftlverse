#' range2vec
#' @examples \dontrun{
#'   range2vec(c(3, 12))
#'   range2vec(c(3, 12), 3)
#' }
#' @export
range2vec <- function(ran, by = 1) {
  seq(from = ran[1], to = ran[2], by = by)
}
