#' hsmooth
#' 
#' smoothing with rmean, but not accross jumps larger than a threshold
#' 
#' @param s width of moving mean
#' @param thr threshold
#' @examples \dontrun{
#'     x <- c(1,3,5,NA, 7,9, 48, 46, 44, 42, 40, 30, 32, 34, 36, 22, 23)
#'     plot(x)
#'     
#'     x1 <- hfrollmean(x,5)
#'     lines(x1, type = "b", col = "orange")
#'     x1r <- zoo::rollmeanr(x,5)
#'     lines(x1r)
#'     x1fr <- data.table::frollmean(x,5, align = "right")
#'     lines(x1fr, col = "red")
#'     x1frm <- frollmeanMirror(x, 3, align = "right")
#'     lines(x1frm, col = "green", type = "b")
#'     
#'     x1f <- hfilter(x, npad = 3, sd = .01)
#'     lines(x1f, col = "orange", lwd = 4)
#'     hpad(x, padType = "circular")
#'     
#'     x1f <- hfilter(x, npad = 3, sd = .8, padType = c("mirror", "mean", "copy", "circular", "tailValue", "constant", "NA")[4])
#'     lines(x1f, col = "green", lwd = 2)
#'     
#'     x1f <- hfilter(x, npad = 3, sd = .8, padType = c("mirror", "mean", "copy", "circular", "tailValue", "constant", "NA")[1])
#'     lines(x1f, col = "purple", lwd = 2)
#'     
#'     x2 <- hsmooth(x, thr = 20)
#'     lines(x2, col="red")
#'     
#'     x3 <- hclamper(x, thr = 8, start0=FALSE)
#'     lines(x3, col="blue")
#'     points(x3, col="blue")
#' }
#' @export
hsmooth <- function(x
                    , ...
                    , thr = inf
                    , FUN = get("hfrollmean")
){
  x <- interNAZoo(x)
  jump <- abs(c(0,diff(x))) >= thr
  f <- cumsum(jump)
  # message("Nr of stretches: ", data.table::uniqueN(f))
  f <- factor(f)
  f
  d <- lapply(split(x, f), FUN, ...)
  # unname(unlist(d))
  structure(unsplit(d, f), jump=jump)
}




#' hclamper2
#' 
#' remove jumps larger than a threshold
#' @examples \dontrun{
#'   x <- c(6,7,8,9,1,2,3, 23, 24, 25)
#'   hclamper2(x)
#'   hclamper2(x, thr = 5)
#'   hclamper2(x, thr = c(-5, 5))
#'   hclamper2(x, thr = c(-5, 25))
#'   hclamper2(x, thr = c(-25, 25))
#'   hclamper2(x, replace0 = 1)
#'   hclamper2(x, replace0 = 0)
#'   hclamper2(x, replace0 = 0, n = 3)
#'   hclamper2(x, replace0 = 0, n = 5)
#'   hclamper2(x, replace0 = 0, n = 5, align = "center")
#' }
#' @param thr threshold
#' @export
hclamper2 <- function(x
                      , thr = 5
                      , replace0 = 1
                      , start0 = FALSE
                      , n = 2
                      , align = "right"
                      , sh = 1
                      , ...
){
  # x <- interNAZoo(x)
  x <- aphApprox2(x)
  if (start0){
    ffirst <- x[2]-x[1]
    if (is.na(ffirst)) ffirst <- 0
  } else {
    ffirst <- 0
  }
  dx <- c(ffirst, diff(x))
  dx.froll <- frollmeanMirror(diff1(x), n = n, align = align, ...)
  dx.froll <- shift(dx.froll, sh)
  if (length(thr) == 2){
    
    ind <- which(dx < thr[1])
    dx[ind] <- (1-replace0)*dx.froll[ind]
    
    ind <- which(dx > thr[2])
    dx[ind] <- (1-replace0)*dx.froll[ind]
    
  } else {
    ind <- which(abs(dx) > thr)
    # str(ind)
    # str(dx.froll)
    dx[ind] <- (1-replace0)*dx.froll[ind]
  }
  dx
  (1-start0)*x[1] + aphCumsum(dx)
}
