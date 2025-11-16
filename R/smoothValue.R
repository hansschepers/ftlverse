#' smoothValue
#' smooth time series with a smoothing kernel
#'
#' @param value `numeric()`. A vector of values to smooth.
#' @param kernel `kernel`. A vector of values representing a kernel, for example
#' the result of [gaussianKernel()].
#'
#' @export
smoothValue <- function(value, kernel = NULL) {
  if (is.null(kernel)) kernel <- gaussianKernel()
  smoothedValue <- convolve2(value, kernel, padlen = length(kernel))
}
