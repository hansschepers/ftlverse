#' hspca0
#'
#' @author Hans.Schepers@@gmail.com
#' @importFrom data.table as.data.table
#' @export
hspca0 <- function(dfg
                   , yois = getyois(dfg)
                   , correlmatrix = NULL
                   , suffix = ""
                   , use = c("pairwise.complete.obs", "complete")[1]
){
  if (is.null(correlmatrix)) {
    wasDT <- inherits(dfg, "data.table")
    dfg <- as.data.frame(dfg)
    correlmatrix <- cor( dfg[, yois], use=use)
  } else {
    dfg <- diag(nrow = nrow(correlmatrix))
    attr(dfg, "dimnames") <- attr(correlmatrix, "dimnames")
    dfg <- as.data.frame(dfg)
    yois = getyois(dfg)
    wasDT <- inherits(dfg, "data.table")
  }
  eig <- eigen(correlmatrix)
  expl <- eig$values / sum(eig$values)
  loadings <- eig$vectors
  
  hsscale <- function(x) scale(x
                               , center = apply(x, 2, mean, na.rm=T)
                               , scale = apply(x, 2, sd, na.rm=T)
  )
  
  scores <- as.data.frame(hsscale(as.matrix(dfg[,yois]) %*% loadings))
  pcnames <- paste0("PC",1:ncol(scores),suffix)
  names(scores) <- pcnames
  
  # dfg <- cbind(dfg, scores)
  dfg[,names(scores)] <- scores
  
  loadings <- as.data.frame(loadings)
  names(loadings) <- pcnames
  row.names(loadings) <- yois
  
  if (wasDT) dfg <- data.table::as.data.table(dfg)
  attr(dfg, "outpca") <- list(correlmatrix = correlmatrix,
                              expl = expl,
                              axisLabels = pcaAxisLabels(expl),
                              rotation = loadings,
                              scores = scores,
                              use = use,
                              yois = yois
  )
  return(dfg)
}


#' generate labels for Axis in biplot for PCA
#' @examples \dontrun{
#'   expl <- c(.5, .2, .1)
#'   pcaAxisLabels(expl)
#'   pcaAxisLabels(expl, factoraxes = 2:3)
#'   pcaAxisLabels(expl, baseLabel = "Principal component ")
#'   pcaAxisLabels(expl, 2:3, labs = c("ripeness", "color depth", "nutritional value"))
#' }
#' @export
pcaAxisLabels <- function(expl
                          , factoraxes = seq_along(expl)#[1:2]
                          , baseLabel = "PC"
                          , labs = paste0(rep(baseLabel, length(expl)), factoraxes)
                          ){
  paste0(labs[factoraxes], " (", round(100 * expl[factoraxes], 0), "%)")
}
