#' applyMcParList
#' @examples \dontrun{
#'   mcParList <- list(par1 = c(0.1, .002, 2)) # no names needed, but this order is taken:
#'   mcParList <- list(par1 = c(Bias = 0.1
#'                             , Variation = 0.002
#'                             , Frequency = 2))
#'   parms = list(par1 = 0.2)
#'   applyMcParList(parms, mcParList)
#'   mcParList <- list()
#'   applyMcParList(parms, mcParList)
#' }
#' @export
applyMcParList <- function(parms
                           , mcParList = list()){
  mcExtentions <- c("Bias", "Variation", "VariationFrequency")
  for (mcPL in names(mcParList)){
    parms <- mergeParameters(
      parms
      , as.list(setNames(mcParList[[mcPL]], paste0(mcPL, mcExtentions))))
  }
  parms
}

# mcParList <- as.list(setNames(c(0, .001, 4)
#                               , paste0("RTR", mcExtentions)))
# 
# mcParList <- list(RTRBias = 0   # celcius per Joules/day/cm2 RTR, bias
#                   , RTRVariation = 0.001        # celcius per Joules/day/cm2 RTR, variation
#                   , RTRVariationFrequency = 4   # noise frequency
# )
# 
# mcParList <- list(RTRBias = 0   # celcius per Joules/day/cm2 RTR, bias
#   , RTRVariation = 0.001        # celcius per Joules/day/cm2 RTR, variation
#   , RTRVariationFrequency = 4   # noise frequency
# )
# mcParList <- list()
