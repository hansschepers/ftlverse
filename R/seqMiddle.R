#' seqMiddle
#'
#' @export
seqMiddle <- function(x) {
  x2 <- cumsum(!NATails(x))
  x2[NATails(x)] <- NA
  x2
}
