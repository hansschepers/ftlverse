#' getElements2dt
#' @export
getElements2dt <- function(coefs  # a list
                          , elems = c("parms", "metrics1")
                          , idcol = "doy"){
  resLi <- list()
  for (elem in elems){
    parList <- lapply(coefs, getElement, elem)
    resLi[[elem]] <- rbindlist(parList, idcol = "doy")
  }
  res <- resLi[[1]]
  res
  for (elem in elems[-1]){
    dd <- resLi[[elem]]
    setDT(dd)
    nms <- setdiff(names(dd), idcol)
    nms <- intersect(nms, names(res))
    if (length(nms)) setnames(dd, nms, paste(elem, nms, sep = "."))
    res <- dd[res, on = idcol]
  }
  res
}
