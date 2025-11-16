#' modelKpis
#' 
#' @export
modelKpis <- function(mod, unitConv = c(1, 1)){
  coe <- coef(mod)
  coe <- coe * unitConv[seq_along(coe)]
  .mod <<- mod
  smod <- summary(mod)
  r2a <- setNames(smod$adj.r.squared, "r2a")
  r2 <- setNames(smod$r.squared, "r2")
  # icptCov <- setNames(smod$cov.unscaled[1, 1], "icptCov")
  # slopeCov <- setNames(smod$cov.unscaled[2, 2], "slopeCov")
  
  res <- c(coe
           , r2a
           , r2
           # , icptCov, slopeCov
           )
  res <- as.list(res)
  res <- as.data.table(res)
  res
}

returnNAblock <- function(model, endRow, n){
  res <- c(endRow = endRow, n = n, modelKpis(model))
  if (endRow < n){
    res <- sapply(res, \(x) NA , USE.NAMES = TRUE, simplify = FALSE)
  }
  res
}



#' hmodel
#' @export
hmodel <- function(DT
               , endRow = nrow(DT)
               , n = 4
               , xoi = "RADJCM"
               , yoi = "GHTEMP"
               , curv = 1
               , curvx = curv
               , curvy = curv
               , intercept = " + 0"
               , formula = makeFormula(yoi,  xoi, intercept = intercept)) {
  
  # filter 
  startWindow <- max(1, endRow - n + 1)
  dataSel <- as.data.table(DT)[seq(startWindow, endRow)]
  # dataSel <<- copy(.dataSel)
  # bend
  if (curvx != 1) dataSel[, (xoi) := get(xoi) ^ curvx]
  if (curvy != 1) dataSel[, (yoi) := get(yoi) ^ curvy]
  .dataSel <<- copy(dataSel)
  dataSel
  # run model
  model <- lm(formula, data = dataSel)
  # extract model kpis
  res <- returnNAblock(model, endRow, n)
  res
}

hmodelByRow <- function(DT
                        , startRow = 1
                        , ...){
  dataSel <- DT[startRow:nrow(DT)]
  rbindlist(lapply(seq(nrow(dataSel))
                   , \(x) hmodel(dataSel, endRow = x, ...))
            , idcol = "rownr", fill = TRUE)
}


# aphVaryArg <- function(DT
#                         , startRow = 1
#                        , varyArg
#                         , ...){
#   dataSel <- DT[startRow:nrow(DT)]
#   rbindlist(lapply(seq(nrow(dataSel)), \(x) hmodel(dataSel, endRow = x, ...))
#             , idcol = "rownr", fill = TRUE)
# }
