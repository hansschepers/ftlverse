#' gaussianKernelHS
#' create kernel for convolution
#'
#' @param ln `numeric(1)`. A number of quantiles to use, from `1:ln`.
#' @param sd `numeric(1)`. A vector of standard deviations.
#'
#' @importFrom stats dnorm median
#' @export
gaussianKernelHS <- function(ln = 15, sd = 3) {
  kseq <- seq(ln)
  kernel <- dnorm(kseq, mean = median(kseq), sd = sd)
  kernel <- kernel / sum(kernel)
  return(kernel)
}
