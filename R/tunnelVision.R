#' tunnelVision
#'
#' #TODO could be a method of treatOutliers?
#'  
#' @param data The time series to be filtered
#' @param degree polynomial order
#' @details A low pass filter allows low frequency signals to pass and high
#' @return the filtered time series
#' @examples \dontrun{
#'   y <- 10*rnorm(1001); y[16] <- 122 ; y[55] <- 118
#'   y <- 10 - 4e-3*(seq(101)-50)^2 + 3*runif(101); y[16] <- 15 ; y[55] <- 6
#'   # y <- tunnelVision(y, qu = c(0, 1.1))
#'   # y <- tunnelVision(y, qu = c(-0.001, 1))
#'   formals(tunnelVision)$requestedOutput <- "all"
#'   tv <- tunnelVision(y, degree = 2, requestedOutput = "all")
#'   plot(tv$orig)
#'   lines(tv$result, col = "blue")
#'   tv <- tunnelVision(y, degree = 2, qu = .05, maxIter = 1)
#'   plot(tv$orig)
#'   lines(tv$result, col = "blue")
#'   tv <- tunnelVision(y, degree = 1, maxIter = 21, kpiStop = 0)
#'   plot(tv$orig)
#'   lines(tv$result, col = "blue")
#'   
#'   tv <- tunnelVision(y, kpiStop = 0, degree = 3, maxIter = 111)
#'   lines(tv$result, col = "blue")
#'   tv <- tunnelVision(y, qu = c(0.0, 1.1), kpiStop = 0, degree = 5, maxIter = 111)
#'   lines(tv$result, col = "red")
#'   tv <- tunnelVision(y, qu = c(-.1, 1), kpiStop = 0, degree = 5, maxIter = 111)
#'   lines(tv$result, col = "green")
#'   str(.lis[[1]])
#'   str(.lis)
#'   #plot(unlist(purrr::map(.lis, 3)))
#'   #unlist(purrr::map(.lis, 4))
#'   #plot(unlist(purrr::map(.lis, 4)))
#'   #plot(unlist(purrr::map(.lis, 4))[-(1:2)])
#'   .lis
#' }
#' @export
tunnelVision <- function(
    y
    , x = seq_along(y)
    , qu = c(0, 1)  # per cycle, take out min and max, not more
    , degree = 1
    , formu = paste0("y ~ poly(x, degree = ", degree,")")
    , maxIter = 10
    , kpiStop = 6  # 5 for norm(1000), 2.5 for unif(1000)
    , kpiMustDecrease = FALSE
    , doplot = FALSE
    , requestedOutputs = c("last")
    , doNA = c("mean", "drop", "fill")[1]
    , info = character(0)
){
  info <- as.character(info)
  result <- list(original = y)
  y[is.infinite(y)] <- NA
  res <- y
  nnn <- length(y)
  if (doNA[1] == "fill"){
    res <- interNAZoo(y)
  }
  if (doNA[1] == "mean"){  # for degree = 1 best...
    res <- y
    res[is.na(res)] <- hmean(res)
  }
  
  if (doNA[1] == "drop"){
    ycopy <- y
    indexNoNAs <- !is.na(y)
    outInOrig <- seq_along(ycopy)[indexNoNAs]
    res <- y[indexNoNAs]
    x <- x[indexNoNAs]
  }
  
  res
  
  if (length(qu) == 1){
    qu <- c(qu, 1 - qu)
  }
  if ("all" %in% requestedOutputs) {
    requestedOutputs <- c("iiter", "outLB", "outUB", "mae", "kpi"
                          , "kpiimprov", "maeImprov", "res")
  }
  all <- 1
  if (length(info) > 0) {
    cat(paste0("starting with ", info, ": "))
  }
  lis <- list()
  if (doplot){
    plot(res, xlim = c(0, nnn))
  }
  iiter <- 1
  while (iiter <= maxIter){
    iiter <- iiter + 1
    # str(x)
    # str(res)
    fit <- lm( formu, data.frame(x = x, y = res), na.action = na.omit)#na.exclude)
    rr <- fit$residuals
    # str(rr)
    if (!length(rr)) { next }
    # kpi <- max(abs(rr))
    marr <- median(abs(rr))
    if (marr < 1e-6) { next }
    kpi <- max(abs(rr)) / marr
    if (length(info) > 0) {
      cat(paste(round(kpi, 1), " "))
    }
    mae <- mean(abs(rr))
    if (iiter == 2) maeOld <- mae
    if (iiter == 2) kpiOld <- kpi
    maeImprov <- mae / maeOld
    kpiimprov <- kpiOld / kpi
    if (kpi < kpiStop | ((kpi > kpiOld) & kpiMustDecrease)){
      if ("last" %in% requestedOutputs){
        # cat(paste0("stopped at ", iiter, "\n"))
        if (doNA == "drop"){
          log_trace(sum(indexNoNAs)-length(res))
          # str(res)
          # print(hsummary(res))
          ycopy[indexNoNAs] <- res
          res <- ycopy
          # stop()
        }
      }
      break
    }
    maeOld <- mae
    kpiOld <- kpi
    if (qu[1] < 0){
      outLB <- integer(0)
    } else {
      outLB <- which(rr <= quantile(rr, qu[1]))
    }
    if (qu[2] > 1){
      outUB <- integer(0)
    } else {
      outUB <- which(rr >= quantile(rr, qu[2]))
    }
    out <- union(outLB, outUB)
    # print(out)
    pred <- predict(fit)
    if (doplot){
      points(pred, col = "orange", pch=22)
      lines(pred, col = "orange")
    }
    
    # unlist(hsummaryC(pred - res))
    if (doNA == "drop"){
      res[indexNoNAs] <- pred[outInOrig[out]]
    } else {
      res[out] <- pred[out]
    }
    
    if (doplot){
      lines(res, col = iiter, lwd = iiter)
    }
    
    lis[[iiter]] <- mget(setdiff(requestedOutputs, "last"))
  } # iteration loop
  
  if (length(info) > 0) cat(paste0("end@ ", iiter, "\n"))
  
  if (doNA == "drop"){
    ycopy[indexNoNAs] <- res
    res <- ycopy
  }
  result$result <- res
  if (length(list) > 1) lis <- lis[-1]
  .lis <<- lis
  if ("last" %in% requestedOutputs) {
    result <- res
  }
  return(result)
}

if(F){
  y <- c(3, 4, 5, 14, 3, 4, 2, 3)
  hmean(y)
  tunnelVision(y, info = "test")
  tunnelVision(y, info = "test", kpiStop = 4)
  tunnelVision(y, info = "test", kpiStop = 4, qu = c(-1,1))
  # y <- c(1, 3, 4, 5, 14, 3, 4, 2, 3)
  tunnelVision(y = y, qu = c(-1, 1)
               # , doNA = "drop"
               , doNA = "mean"
               , requestedOutputs = "all"
               , doplot = T
               , maxIter = 2
               , info = "ww"
               , kpiStop = 1
  )
}