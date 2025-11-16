#' aphPad
#' 
#' pad vector with various padType's
#' @param padType [mirror] boundary conditions
#' 
#' @examples \dontrun{
#' y <- c(1,5,4,6,9,1,2,1,2,3,6)
#' span <- 3
#' plot(c(rep(NA, span), y, rep(NA, span)), type = "b", lwd = 3, main = "padding types")
#' y1 <- aphPad(y, s = span, padType = "mirror") ; points(y1, cex = .6, col=1, type = "b")
#' y2 <- aphPad(y, s = span, padType = "mean")   ; points(y2, cex = .6, col=2, type = "b")
#' y3 <- aphPad(y, s = span, padType = "copy")   ; points(y3, cex = .6, col=3, type = "b")
#' y4 <- aphPad(y, s = span, padType = "circular")  ; points(y4, cex = .6, col=4, type = "b")
#' y5 <- aphPad(y, s = span, padType = "tailValue") ; points(y5, cex = .6, col=5, type = "b")
#' 
#' aphPad(y, s = span, padType = "tailValue")
#' aphPad(y, s = span, padType = "NA")
#' 
#' plot(c(rep(NA, span), y, rep(NA, span)), type = "b", lwd = 3, main = "smoothing per padding type")
#' padType <- "mirror"
#' y1s <- c(rep(NA, span), frollmeanMirror(y, n = span, padType = padType))                    ; points(y1s, cex = .6, col=1, type = "b")
#' y1sc <- c(rep(NA, span), frollmeanMirror(y, n = span, padType = padType, align = "center")) ; points(y1sc, cex = .6, type = "b", col = "purple")
#' 
#' padType <- "mean"
#' y2s <- c(rep(NA, span), frollmeanMirror(y, n = span, padType = padType))                    ; points(y2s, cex = .6, col=2, type = "b")
#' y2sc <- c(rep(NA, span), frollmeanMirror(y, n = span, padType = padType, align = "center")) ; points(y2sc, cex = .6, type = "b", col = "purple")
#' 
#' hpad(x = 6:8, s = 6)
#' hpad(x = 6:8, s = 6, padType = "tailvalue")
#' }
#' @param s width of padding
#' @param constant numeric constant to pad with if padType = 'constant'
#' @param keepNA overrides the padding for left, right both or none of the tails with NA
#' 
#' @export
aphPad <- function(x
                 , s = 5
                 , padType = c("mirror", "mean", "linear"
                            , "copy", "circular"
                            , "tailValue", "constant", "NA")[1]
                 , constant = c(0, 0)
                 , keepNA = c("none", "left", "right", "both")[1]
                 ){
  nn <- length(x)
  s <- abs(s)
  used_s_paddingLength <- min(s, nn)
  
  if (tolower(padType) == "mirror"){
    s <- min(s, nn)
    x2 <- c(rev(x[seq(s)]), x, rev(x[seq(nn-s+1,nn)]))
  }
  
  if (tolower(padType) == "mean"){
    # s <- min(s, nn)
    x2 <- c(rep(hmean(x[seq(min(s, nn))]), s)
            , x
            , rep(hmean(x[seq(nn-min(s, nn)+1,nn)]), s)
    )
    # same as 
    # padval <- c(mean(x[1:padlen]), mean(x[(xlen - padlen + 1):xlen]))
    # x <- c(rep(padval[1], ylen), x, rep(padval[2], ylen))
  }
  
  if (tolower(padType) == "copy"){
    # copy unreversed (not mirrored!)
    s <- abs(s)
    s <- min(s, nn)
    x2 <- c(x[seq(s)], x, x[seq(nn-s+1,nn)])
  }
  
  if (tolower(padType) == "circular"){
    # periodic boundary conditions
    s <- abs(s)
    s <- min(s, nn)
    x2 <- c(x[seq(nn-s+1,nn)], x, x[seq(s)])
  }
  
  if (tolower(padType) == "tailvalue"){
    # s <- min(s, nn)
    x2 <- c(rep(x[1], s)
            , x
            , rep(x[nn], s)
    )
  }
  if (tolower(padType) == "constant"){
    # s <- min(s, nn)
    x2 <- c(rep(constant[1], s)
            , x
            , rep(constant[2], s)
    )
  }
  if (tolower(padType) == "na" | is.na(padType)){
    # s <- min(s, nn)
    x2 <- c(rep(NA, s)
            , x
            , rep(NA, s)
    )
  }
  if (keepNA %in% c("both", "left"))  x2[1:s] <- NA
  if (keepNA %in% c("both", "right")) x2[seq(length(x2)-s+1,length(x2))] <- NA
  return(x2)
}


#' hpad
#' @export hpad
hpad <- aphPad

