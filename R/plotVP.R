#' plotVP
#' @examples \dontrun{
#'   p <- plotVP()
#' }
#' @export
plotVP <- function(DTwide = getTestWeekData(iseed = 123456)
                   , positions = 14
                   , pggInput = list()
){
  stemProfile <- animWeeklyData(DTwide, positions = positions)
  .ply <<- stemAnim(stemProfile)
  
  stemProfile[, FN := round(FN)]
  pggs(stemProfile[FN > 0]
       , xoi = "FN", xsc = c(0, 25), xlab = c("Fruits per m2")
       , yoi = "h", ysc = c(0, 350), ylab = "height (cm above ground)", hline = 330
       , facet_w = "itime", free_y = FALSE
       , geom = "pointline"
       , fsize = 14, label="FN", mega = TRUE, psize = 7
       , chunkTitle = "VerticalProfile"
       , input = pggInput
       , doplot = FALSE)
}
