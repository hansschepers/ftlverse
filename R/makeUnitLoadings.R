#' makeUnitLoadings
#' 
#' @param nms names of parameters (e.g. freeSensParms)
#' @param cols NULL or names of columns (defaults to "PC"[1..])
#' @examples \dontrun{
#'   makeUnitLoadings(nms = LETTERS[1:4])
#' }
#' @export
makeUnitLoadings <- function(nms, cols = paste0("PC", seq_along(nms)) ){
  loads <- diag(length(nms))
  dimnames(loads) <- list(nms, NULL)
  dimnames(loads)[[2]] <- cols
  loads
}
