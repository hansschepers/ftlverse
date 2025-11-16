#' hr2
#' @examples \dontrun{
#' x <- tent(seq(0, 1, 1/24))
#' y <- 112 + 0.5 * shift(x, 3, fill = 0) 
#' plot(seq_along(x), x); lines(seq_along(x), y)
#' hr2(x, y, lag.max = 4)
#' hr2(x, y, lag.max = 4, roReturn = "r2")
#' hr2(x, y, lag.max = 4, roReturn = "r2b")
#' a = .01
#' y <- .4 + shift(x,2,fill = 0) * 1 + (runif(length(x))-0.5)*2 * a
#' plot(seq_along(x), x); lines(seq_along(x), y)
#' hr2(x, y, lag.max = 4)
#' mod <- lm(y ~ x)
#' plot(seq_along(x), x); lines(seq_along(x), y)
#' plot(x, y) ; lines(x, predict(mod))
#' }
#' 
#' @export
hr2 <- function(x
                , y
                , lag.max = 0
                , roReturn = character() #"r2b"
                , use = "pairwise.complete.obs") {
  mod <- lm(y ~ x)
  r <- cor(x, y, use = use)
  r2 <- r*r
  r2b = 1 - sum(residuals(mod)^2) / sum( (y - hmean(y) )^2)
  # r2sqrt <- sign(r) * sqrt(r2)
  res <- c(r2b = r2b
           , r2 = r2
    # , hr2 = r2*sign(r)
    , r = r
    # , r2sqrt = r2sqrt
    )
  if (lag.max > 0){
    ccf_result <- ccf(x
                      , y
                      , na.action = na.pass
                      , lag.max = lag.max
                      , plot = FALSE)
    if (logger::log_threshold() > 500) print(ccf_result)
    best <- which.max(abs(ccf_result$acf))
    # print(best)
    corMax <- unlist(ccf_result$acf)[best]
    lagMax <- unlist(ccf_result$lag)[best]
    res <- c(res, corMax = corMax, lagMax = lagMax)
  } else {
    res <- c(res, corMax = r, lagMax = 0)
  }
  if (length(roReturn)) res <- res[roReturn]
  res
}



#' hr2dt
#' @examples \dontrun{
#'   y1 <- tent(seq(0, 1, 1/24))
#'   y2 <- 2 + 0.5 * data.table::shift(y1, 3, fill = 0) 
#'   y3 <- 2 + 0.25 * data.table::shift(y1, -2, fill = 0) 
#'   dt <- cbind(y1, y2, y3)
#'   hr2dt(dt, lag.max = 6, kpi = "corMax", keep.tri = c("lower", "upper")[2])
#'   hr2dt(dt, lag.max = 6, kpi = "corMax", omit = "diag")
#'   hr2dt(dt, lag.max = 6, kpi = "corMax")
#'   hr2dt(dt, lag.max = 6, kpi = "lagMax")
#'   hr2dt(dt, lag.max = 6, kpi = "r")
#'   hr2dt(dt, kpi = "r")
#'   hr2dt(dt)
#'   cor(dt)
#' }
#' @export
hr2dt <- function(dt
                  , kpi = "r"
                  , lag.max = 0
                  , yois = aphVariableLevels(dt)
                  , keep.tri = c("lower", "upper")[0]
                  , omit = c("diag")[0]
                  ){
  n <- length(yois)
  res <- as.data.frame(matrix(NA, nrow = n, ncol = n, dimnames = list(yois, yois)))
  res
  x <- yois[1]
  y <- yois[2]
  dt <- data.table::copy(data.table::as.data.table(dt))
  for (x in yois){
    for (y in yois){
      ana <- hr2(dt[[x]]
                 , dt[[y]]
                 , lag.max = lag.max)
      res[x, y] <- ana[kpi]
    }
  }
  if ("lower" %in% keep.tri) res[upper.tri(res)] <- NA
  if ("upper" %in% keep.tri) res[lower.tri(res)] <- NA
  if ("diag" %in% omit) diag(res) <- NA
  res
}


# from YP
# r2 = function() {
#   
#   cleanData <- private$getCleanPredTruth()
#   rss <- private$..sse()
#   tss <- sum(private$..se(x = cleanData$truth, y = mean(cleanData$truth)))
#   1 - rss / tss
# }