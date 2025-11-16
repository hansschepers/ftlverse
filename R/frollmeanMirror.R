#' frollmeanMirror
#' @details in default, fills hows when smaller than window n
#' @examples \dontrun{
#'   frollmean(c(5, 3), n = 2, align = "right")
#'   frollmeanMirror(c(5, 3), n = 2, align = "right")
#'   frollmeanMirror(c(5, 3), n = 2, align = "right", padType = "tailValue")
#'   frollmeanMirror(c(5), n = 2, align = "right", padType = "tailValue")
#'   frollmeanMirror(c(5), n = 3, align = "right", padType = "tailValue")
#'   frollmeanMirror(c(5, 3), n = 9, align = "right", padType = "tailValue")
#'   
#'   x <- c(3, 5, 0, 10, 6, 2,3,4,5,6,NA,5,4,3,2,1, NA)
#'   data.table::frollmean(x, n = 3)
#'   data.table::frollmean(x, n = 3, align = "center")
#'   data.table::frollmean(x, n = 3, fill = 0, align = "center")
#'   data.table::frollmean(x, n = 3, fill = 0, align = "center", na.rm = TRUE)
#'   hpad(x, s = 3)
#'   frollmeanMirror(x, n = 3)
#'   frollmeanMirror(fillInternalNAs(x), n = 3, na.rm = FALSE)
#'   frollmeanMirror(x, s = 3)
#'   frollmeanMirror(x, n = 3, align = "right")  # is default! not center
#'   frollmeanMirror(x, n = 3, align = "center")
#'   frollmeanMirror(x, s = 3, align = "left")
#'   frollmeanMirror(x = 6:8, s = 6, padType = "tailvalue")
#' }
#' 
#' @inheritParams aphPad
#' @inheritParams data.table::frollmean
#' @param ... Additional parameters to be passed on to [data.table::frollmean()]
#'
#' @export
frollmeanMirror <- function(x
                            , n = 3
                            , na.rm = TRUE
                            , align = c("right", "left", "center")[3]
                            , padType = c("mirror", "mean"
                                       , "copy", "circular"
                                       , "tailValue", "constant", "NA")[1]
                            , constant = c(0, 0)
                            , s = n
                            , fillNA = TRUE
                            , verbosity = log_threshold()
                            , ...){
  n <- s
  n <- abs(n)
  if (n == 1){
    return(x)
  }
  if (fillNA) {
    x <- fillInternalNAs(x)
  }
  x2 <- aphPad(x, s = n, padType = padType, constant = constant)
  if (verbosity > 1000){
    print(x2)
  }
  n <- abs(n)
  x2fr <- data.table::frollmean(x2, n = n, na.rm = na.rm
                                , align = align
                                , ...
                                )
  res <- x2fr[seq(from = n+1, length.out = length(x))]
  res
}

# hpad <- aphPad # in separate file!
