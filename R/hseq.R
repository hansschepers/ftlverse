#' hseq
#' @examples \dontrun{
#' hseq(0)
#' hseq(1)
#' hseq(2)
#' }
#' 
#' @export
hseq <- function(n) {
  seq_along(rep(1, n))
}

#' hseq_along
#' @examples \dontrun{
#'   hseq_along(LETTERS[0])
#'   hseq_along(LETTERS[1:7])
#' }
#' 
#' @export
hseq_along <- function(x) {
  seq_along(rep(1, length(x)))
}
