#' LPfilter
#'
#' A low pass filter implementation to smooth out all high frequency
#' disturbances
#' @param data The time series to be filtered
#' @param samplesPerHour number of samples per hours for example
#' \code{samplesPerHour} = 12 for 5 min sampling interval
#' @param cutOffTime cut-off time in hours
#' @param sigma The standard deviation of the gaussian
#' @param deTrendingMethod a detrending methode that has to be selected from
#' "original", "lm" and "lpf".
#' "original" will use a straight line with force to zero bias to reinforce
#' rotational character of the signal in order to detrend the signal.
#' "lm" will use a linear estimator as the best fit to the data with bias other
#' than zero.
#' "lpf" will use a low-pass filter as a local linear estimator to detrend the
#' data.
#' @details A low pass filter allows low frequency signals to pass and high
#'   frequency signals (noise) are filtered out. The cutoff frequency is defined
#'   by the length of the signaland the total period. For stem and wire load
#'   cells frequencies below 4 hours are assumed to be inferred by other
#'   mechanisms than growth. The input data will normally be a cumulative series
#'   of data. For sake of circularity the slope is removed from the signal
#'   before the frequency response is calculated. After filtering the slope is
#'   added again and returned.
#' @return the filtered time series
#' @examples
#' \dontrun{
#' x0 <- seq(0, 100)
#' x <- sin(x0 * 2 * pi / 100) + sin(x0 / 2) - x0 / 2 + 30
#' sph <- 6
#' y1 <- LPfilter(x, sph, 3, deTrendingMethod = "original")
#' y2 <- LPfilter(x, sph, 3, deTrendingMethod = "lpf")
#' y3 <- LPfilter(x, sph, 3, deTrendingMethod = "lm")
#' plot(x0/sph, x, type = "l")
#' lines(x0/sph, y1, col = "red")
#' lines(x0/sph, y2, col = "green")
#' lines(x0/sph, y3, col = "blue")
#' }
#'
#' @importFrom logger log_error
#' @importFrom stats fft lm predict
#' @export
LPfilter <- function(data, samplesPerHour = 12, cutOffTime = 4, sigma = NULL,
                     deTrendingMethod = "original") {
  N <- length(data)
  lpl <- N %/% (samplesPerHour * cutOffTime)
  if (lpl < 1){
    logger::log_error("length of data is smaller than (twice) filter length; returning NA's")
    return(rep(NA,N))
  }
  
  if (is.null(sigma)) sigma <- 0.5 * lpl
  
  if (deTrendingMethod == "original") {
    ramp <- data[1] + seq(0, N - 1) / N * (data[N] - data[1])
  } else if (deTrendingMethod == "lm") {
    ramp <- predict(lm(x ~ time, data.frame(time = seq_along(data), x = data)))
  } else if (deTrendingMethod == "lpf") {
    if (lpl < 10){
      logger::log_warn("lpf uses low frequency ramp of 10% of requested smoothing but signal length is not sufficient.")
    }
    cot <- min(lpl, 10) * N / (samplesPerHour * lpl)
    ramp <- Recall(data, samplesPerHour = samplesPerHour,cutOffTime = cot, deTrendingMethod = 'original')
  } else {
    logger::log_error("non-existing detrending method chosen")
    stop()
  }
  
  data0 <- data - ramp # start and end at zero
  transDat <- fft(data0)
  
  gkern <- gaussianKernel(2 * lpl + 1, sigma)[(lpl + 1):(2 * lpl)]
  gkern <- gkern / gkern[1]
  # gkern <- 1/(1+(seq(1,2*lpl)/lpl)^max(2,2*cutOffTime))
  FDfilter <- rep(0, N)
  # FDfilter[1:(2*lpl)] <- gkern
  # FDfilter[(N - 2*lpl + 1):(N)] <- rev(gkern)
  FDfilter[1:lpl] <- gkern
  FDfilter[(N - lpl + 1):(N)] <- rev(gkern)
  
  transDatFilt <- FDfilter * transDat
  filtDat <- Re(fft(transDatFilt, inverse = TRUE) / N)[1:N] + ramp
  return(filtDat)
}

