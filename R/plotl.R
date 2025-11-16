#' plotl
#' @export
plotl <- function(..., type="l") plot(..., type=type)



#' plotl2
#' @export
plotl2 <- function(y1, y2, ylim=range(c(y1,y2), na.rm=TRUE), type="l", col="red", doscale=c(), ...) {
  if ("y1" %in% doscale) {y1 <- scale(y1) ; ylim=range(c(y1,y2), na.rm=TRUE)}
  if ("y2" %in% doscale) {y2 <- scale(y2) ; ylim=range(c(y1,y2), na.rm=TRUE)}
  plot(y1, ylim=ylim, type=type, ...)
  lines(y2, col=col)
}



#' matplotl
#' @export
matplotl <- function(..., type="l") matplot(..., type=type)


# to put in .Rprofile 
# plot <- function(...){
#   plotSize <- par()$pin
#   #print(par()[c("pin", "mar", "plt", "cex")])
#   par(mar=c(4,2,2,1))
#   par(plt=c(0.22, 0.98, max(0.15, 0.45-plotSize[2]*0.1) , 0.9))
#   par(cex = min(2, 0.125 * sum(plotSize) ) )
#   graphics::plot(...)
# }
# plot(1:123, xlab="234", ylab = "234", main = "234")


