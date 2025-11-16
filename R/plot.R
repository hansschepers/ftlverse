#' plot.SIMS
#' plots result of a runFun() call
#' @export
plot.SIMS <- function(out, ..., doplot = TRUE, doMelt = TRUE){
  library(ftlverse)
  library(data.table)
  ppggs(out, ..., doplot = doplot, doMelt = doMelt)
}

#' plot.deSolve
#' @export
plot.deSolve <- plot.SIMS

# class(out) <- c("SIMS", class(out))
# class(out) <- class(out)[-1]
# class(out)

# plot(out)
# deSolve:::plot.deSolve(out)
