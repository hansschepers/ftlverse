#' dt2array
#' function to cubify a data.table
#' taken from:
#' https://akhilsbehl.github.io/blog/2014/08/20/r-converting-a-data-dot-table-to-a-multi-way-array-cube/
dt2array <- function (x, facts, dims) {
  stopifnot(is.data.table(x))
  setkeyv(x, rev(dims))
  stopifnot(!any(duplicated(x)))
  dimensions <- lapply(x[ , rev(dims), with = FALSE],
                      function (x) sort(unique(x)))
  xFull <- data.table(expand.grid(dimensions, stringsAsFactors=FALSE))
  setkeyv(xFull, rev(dims))
  x <- data.table:::merge.data.table(xFull, x, by=dims, all=TRUE)
  factsVec <- unlist(x[ , facts, with=FALSE], recursive=FALSE, use.names=FALSE)
  nFacts <- length(facts)
  nDims <- length(dims)
  if (nFacts > 1) {
    dim(factsVec) <- c(sapply(dimensions, length), nFacts)
    dimnames(factsVec) <- c(dimensions, "facts"=list(facts))
    return(aperm(factsVec, perm=c(nDims:1, nDims + 1)))
  } else {
    dim(factsVec) <- sapply(dimensions, length)
    dimnames(factsVec) <- dimensions
    return(aperm(factsVec))
  }
}
