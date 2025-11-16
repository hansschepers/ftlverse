#' monotonic_transform
#' Function for monotonic transformation using natural splines
#' splines::ns
#' @export
monotonic_transform <- function(x
                                , df = 4
) {
  # Create basis
  knots <- quantile(x, probs=seq(0, 1, length.out=df+2)[-c(1, df+2)])
  basis <- ns(x, knots=knots)
  
  # Ensure monotonicity by cumulative sum of squared coefficients
  coef <- abs(svd(basis)$v[,1])
  coef <- cumsum(coef^2)
  
  # Transform
  as.vector(basis %*% coef)
}
