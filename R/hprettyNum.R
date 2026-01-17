#' hprettyNum
#' 
#' @examples \dontrun{
#'   x <- c(0.984, 1.3456789, 12.43, 123, 1234.000001, 123456789, 1e11, -0.332, -1.2, -1234412.11, 2021, 1969, 1970, 2030, 2031)
#'   hprettyNum(x, 1)
#'   hprettyNum(x, 1, abb = FALSE)
#'   hprettyNum(x)
#'   hprettyNum(x, 3, abb = TRUE)
#'   hprettyNum(x, 3, abb = FALSE, asNum = TRUE)
#'   hprettyNum(x, 3, abb = FALSE)
#'   as.numeric(hprettyNum(x, 3, abb = FALSE))
#' }
#' @param digits integer
#' @export
hprettyNum <- function(x
                       , digits = 3
                       , abb = FALSE
                       , tol = 1e-4
                       , format = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")[1]
                       , NAblank = TRUE
                       , asNum = FALSE
                       , omit = character()
                       , ...) {
  if (!length(x)) return(x)
  digits <- pmax(1, digits)
  if (inherits(x, c("matrix"))) {
    x <- as.data.table(x, keep.rownames = TRUE)
  }
  
  ####################################### recursive call!! (do for each column)
  if (inherits(x, c("data.frame", "data.table"))) {
    # wide DT only!
    fois <- union(aphTimes(x), aphFactors(x))
    fois <- setdiff(fois, omit)
    # str(fois)
    yois <- setdiff(names(x), fois)
    yois <- setdiff(yois, omit)
    # str(yois)
    xfois <- as.list(x)[fois]
    xyois <- as.list(x)[yois]
    
    wc <- mapply(hprettyNum, xyois
                 , digits = digits, abb = abb, tol = tol
                 , format = format, asNum = asNum
                 # , omit = character()
                 , MoreArgs = list(...)
                 , SIMPLIFY = FALSE
    )
    wc <- c(as.list(xfois), wc)
  }
  
  if (inherits(x, "data.table")) {
    wc <- data.table::as.data.table(wc)
    return(wc)
  }
  if (inherits(x, "data.frame")) {
    wc <- as.data.frame(wc)
    return(wc)
  }
  if (inherits(x, "list")) x <- unlist(x)
  if (sum(c("difftime")                  %in% class(x))) x <- as.numeric(x)
  if (sum(c("POSIXct", "POSIXt", "Date") %in% class(x))) {
    if (inherits(x, "Date")) format <- "%Y-%m-%d"
    return(format(x, format = format, ...))
  }
  if (!is.numeric(x)) return(x)
  keepDate <- x %in% 1970:2050
  s <- sign(x)
  x <- abs(x)
  hdiv <- h <- floor(log10(x))
  if (abb){
    gg <- h >= 9
    mm <- h %in% 6:8
    kk <- h %in% 3:5
    gg[is.na(gg)] <- FALSE
    mm[is.na(mm)] <- FALSE
    kk[is.na(kk)] <- FALSE
    h[gg] <- h[gg] - 9
    h[mm] <- h[mm] - 6
    h[kk] <- h[kk] - 3
  }
  
  xx <- format(x / 10^hdiv, scientific = 20, digits=digits, trim=TRUE)
  w <- suppressWarnings( as.numeric(xx)*10^h )
  w[abs(w) < tol] <- 0
  wc <- format(w, drop0trailing= TRUE
               , scientific = 20
               , digits = digits, nsmall = 0, trim = TRUE)
  if (abb){
    wc[gg] <- paste0(wc[gg], "G")
    wc[mm] <- paste0(wc[mm], "M")
    wc[kk] <- paste0(wc[kk], "k")
  }
  ssLogical <- s == -1
  ssLogical[is.na(ssLogical)] <- FALSE
  wc[ssLogical] <- paste0("-", wc[ssLogical])
  ssLogical <- s == 0
  ssLogical[is.na(ssLogical)] <- FALSE
  wc[ssLogical] <- "0"
  wc[keepDate] <- x[keepDate]
  if(NAblank){
    wc[wc == "NA"] <- ""
  }
  if (asNum){
    wc <- as.numeric(wc)
  }
  wc
}
