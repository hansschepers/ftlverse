#' hfrollmean
#' @description smoother that keeps first and last point untouched. for small data (<50 points)
#' @examples \dontrun{
#'   vv <- runif(100) ; mean(vv)
#'   vv <- c(3, 5, 7, 2, 1, -3, -1, 1, 3) ; sum(vv)
#'   vv <- c(3, 5, 17, 2, 1, NA, -3, -1, 1, 3, NA, NA, NA) ; hmean(vv) ; length(vv)
#'   vvsm <- hfrollmean(vv, n=3, reps = 5, align = "right", doplot = TRUE)
#'   vvsm <- hfrollmean(vv, n=3, reps = 5, align = "left", doplot = TRUE)
#'   vvsm <- hfrollmean(vv, n=3, reps = 5, align = "center", doplot = TRUE)
#'   vvsm <- findTails(vv, FUN = hfrollmean, funArgs = list(n = 3, reps = 5, align = "left", doplot = TRUE))
#'   vvsm <- findTails(vv, FUN = hfrollmean, funArgs = list(n = 3, reps = 5, align = "right", doplot = TRUE))
#'   vvsm <- findTails(vv, FUN = hfrollmean, funArgs = list(n = 3, reps = 5, align = "center", doplot = TRUE))
#'   
#'   vv <- c(NA, NA, 0, 0, 3, 5, 17, 21, 1, NA, -3, -1, 1, 3, NA, NA, NA) ; hmean(vv) ; length(vv)
#'   vvsm <- findTails(vv, suspect = c(NA, 0), FUN = hfrollmean, funArgs = list(n = 3, reps = 5, align = "left", doplot = TRUE))
#'   
#'   vvsm <- hfrollmean(vv, doplot = TRUE)
#'   plot(vv) ; lines(vvsm)
#'   
#'   hfrollmean(x = 6:8, n = 6, padType = "tailvalue")
#'   
#'   x <- c(NA, NA, NA, 3, 5, 6, 9, NA, 1, NA)
#'   hfrollmean(x, n=3, reps=3, doplot = TRUE, fillNA = FALSE)
#'   firstNonNA(x)
#'   lastNonNA(x)
#' }
#'   
#' @inheritParams aphPad
#' @inheritParams data.table::frollmean
#' @param ... Additional parameters to be passed on to [data.table::frollmean()]
#' 
#' @export
hfrollmean <- function(x
                       , n = 3
                       , reps = 1
                       , align = "center"
                       , na.rm = TRUE
                       , padType = c("tailValue", "mirror", "mean"
                                     , "copy", "circular"
                                     , "constant", "NA")[1]
                       , constant = c(0, 0)
                       , fillNA = TRUE
                       , doplot = FALSE){
  n <- round(n)
  if (abs(n) == 1){
    return(x)
  }
  if (sumna(x) == length(x)){
    log_warn("hfrollmean| only NA in x")
    return(x)
  }
  
  if (doplot){
    plot(seq_along(x), x)
  }
  # .x <<- x ; stop()
  # print(sys.call())
  firstLast <- c(firstNonNA(x), lastNonNA(x))
  # str(x)
  # str(firstLast)
  if (align == "right") firstLast <- firstLast[1]
  # if (align == "left") firstLast <- firstLast[2]
  memStartEnd <- x[firstLast]
  ii <- 1
  for (ii in seq(reps)){
    x <- frollmeanMirror(x
                         , n = n
                         , padType = padType
                         , constant = c(0, 0)
                         , align = align
                         , na.rm = na.rm
                         , fillNA = fillNA)
    if (padType != "circular") {
      x[firstLast] <- memStartEnd
    }
    if (align == "right" & !is.na(firstLast[1])) {
      toNA <- setdiff(hseq(firstLast[1]), firstLast[1])
      # message(61)
      # str(toNA)
      if (length(toNA)) x[toNA] <- NA
    }
    # if (align == "left") {
      # toNA <- setdiff(hseq(length(x)), hseq(firstLast[2]))
      # message(67)
      # str(toNA)
      # if (length(toNA)) x[toNA] <- NA  # [1]!! not [2]
    # }
    if (doplot){
      lines(x, col = ii)
    }
  }
  x
}
