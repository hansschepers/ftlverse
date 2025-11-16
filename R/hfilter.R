#' hfilter
#' @examples \dontrun{
#'   x <- c(1, 4, NA, 9, 1, NA)
#'   hfilter(x)
#' }
#' @export
hfilter <- function(x
                    , npad = 3
                    , sd = .85
                    , filter = gaussianKernel(npad, sd)
                    , padType = c("mirror", "mean"
                                  , "copy", "circular"
                                  , "tailValue", "constant", "NA")[1]
                    , ...
){
  x <- aphApprox2(x, rule = 1)
  padProcess(x
             , fun = "filter"
             , npad = npad
             , padType = padType
             , funargs = list(filter = filter)
             , ...
  )
}
