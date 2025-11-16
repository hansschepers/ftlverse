#' summary.SIMS
#' @export
summary.SIMS <- function(SIMS){
  dd <- strList(SIMS)
  simArgs <- names(formals(runFunHES))
  res <- list()
  res$args <- dd[rn %in% simArgs][, rn := factor(rn, levels = simArgs, ordered = TRUE)][order(rn)]
  res$rest <- dd[!rn %in% simArgs]
  res$size <- object.size2(SIMS)
  res$tail <- tail(SIMS$out, 3)
  print(res)
  invisible(res)
}
