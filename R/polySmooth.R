#' polySmooth
#' @examples \dontrun{
#'   y <- runif(100)
#'   y <- c(1,3,5,6,5,4,7,8,9,4,2,1)
#'   plot(y)
#'   yp <- polySmooth(y, degree = 1); lines(yp)
#'   yp <- polySmooth(y, degree = 2); lines(yp)
#'   yp <- polySmooth(y, degree = 5); lines(yp)
#'   yp <- polySmooth(y, degree = 7); lines(yp)
#'   yp <- hfrollmean(y, n = 7); lines(yp)
#'   yp <- hfrollmean(y, n = 5); lines(yp)
#'   yp <- hfrollmean(y, n = 3); lines(yp)
#'   yp <- frollmeanMirror(y, n = 3); lines(yp)
#'   
#'   yp <- padPolySmooth(y, degree = 15); lines(yp, lwd = 2)
#'   yp <- padPolySmooth(y, degree = 5); lines(yp, lwd = 2)
#'   yp <- padPolySmooth(y, degree = 5, npad = 11); lines(yp, lwd = 2)
#' }
#' @export
polySmooth <- function(y
                       , x = seq_along(y)
                       , degree = 7
                       , newdata = data.frame(doy = x) # 1:365
){
  if (sum(!is.na(y)) < 2) {
    log_warn("polySmooth| not enough non-NA data found")
    return(y)
  }
  model <- polyFit(y = y, x = x, degree = degree)
  yp = predict(model, newdata = newdata)
  yp
}

#' polySmooth
#' @examples \dontrun{
#'   y <- runif(365)
#'
#'   y <- c(1,3,5,6,5,4,7,8,9,4,2,1)
#'   x = seq_along(y)
#'   mo3 <- polyFit(y, degree = 3, ortho = FALSE)
#'   mo3
#'   # see fitted values with help of full lm object...
#'   predict(mo3)
#'
#'   # works:
#'   # instead of ... cc[1] + cc[2] * x + cc[3] * x*x + cc[4]*x^3
#'   drop(sapply(0:3, \(e) x^e) %*% coef(mo3))
#'
#'   mo3 <- polyFit(y, degree = 3, ortho = TRUE)
#'   mo3
#'   # NOT working:
#'   drop(sapply(0:3, \(e) x^e) %*% coef(mo3))
#'   # to use coefs only, prepare model matrix first
#'   coefficients <- coef(mo3)
#'   degree <- length(coefficients) - 1
#'   modelMatrix_365 <- as.matrix(polyFit(y, degree = degree)$model)
#'   modelMatrix_365[, 1] <- 1
#'   # modelMatrix_365[1:3, ]
#'   # orthonormal model matrix
#'   t(as.matrix(modelMatrix_365)[, 2:4]) %*% as.matrix(modelMatrix_365)[, 2:4]
#'   matplot(modelMatrix_365, type = "l")
#'   # recall this is what we try to reconstruct without the mo3 object itself
#'   predict(mo3)
#'   drop(modelMatrix_365[, seq(degree+1)] %*% coefficients)
#' }
#' @export
polyFit <- function(y
                    , x = seq_along(y)
                    , degree = 7
                    , ortho = TRUE){
  # NOT using orthogonal polynomials!!
  formula1 = paste0("y ~ poly(x,"
                    , degree
                    , ", raw = "
                    , ifelse(ortho, "FALSE", "TRUE")
                    , ")")
  model <- lm(eval(parse(text=formula1)))
  return(model)
}




#' padPolySmooth
#' @export
padPolySmooth <- function(y
                          # , xx = seq_along(y)
                          , degree = 7
                          , fun = "polySmooth"
                          , npad = 1
                          , padType = c("mirror", "mean"
                                        , "copy", "circular"
                                        , "tailValue", "constant", "NA")[6]
                          , constant = c(hmean(y[seq(floor(length(y)/mirrorFraction))])
                                         , hmean(rev(y)[seq(floor(length(y)/mirrorFraction))]))
                          , mirrorFraction = 4
                          , keepNA = c("none", "left", "right", "both")[1]
                          # , ...
                          ){
  yp <- padProcess(x = y
                   , fun = fun
                   , npad = npad
                   , padType = padType
                   , constant = constant
                   , keepNA = keepNA
                   , funargs = list(#x=xx, 
                                    degree = degree))
  yp
}
