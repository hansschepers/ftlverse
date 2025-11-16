#' pggsSplit
#' 
#' @export
pggsSplit <- function(dfg
                      , facet_w = aphVariables(dfg)
                      , input0 = list(geom = "point"
                                      , ci.alpha = .1
                                      , annoFit = TRUE
                                      , subtitle = facet_w)
                      , input = list()
                      , ...){
  pggsInput <- mergeParameters(input0, input)
  setDT(dfg)
  ww <- split(dfg, by = facet_w)
  pList <- lapply(seq_along(ww)
                   , function(ii) {
                     pggs(
                  , dfg = ww[[ii]]
                  , facet_w = "nothing"
                  , input = pggsInput
                  , subtitle = names(ww)[ii]
                  , ... ) })
  Reduce("+", pList)
}

