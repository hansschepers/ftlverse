#' aphPalette
#' 
#' @export
aphPalette <- function(colors = c("red", "orange", "blue")
                       , n = 12
                       , foi = "doy"
                       , df = setNames(list(a=1:n), foi)
                       # , space = c("rgb", "Lab")[2]
                       , direction = 1
                       , ...
){
  if (direction == -1){
    colors <- rev(colors)
  }
  n <- length(unique(as.list(df)[[foi]]))
  paletteFn <- colorRampPalette(colors
                                # , space = space
                                , ...)
  col = paletteFn(n)
  col
  return(col)
}
