#' fitHM
#' @examples \dontrun{
#'   modelsGiven$hmFromCumulativeTemp <- fitHM(dtw)
#' }
#' @export
fitHM <- function(dtw){
  list(pars = fitMaturity(dtw
                                   , moi = "harvestMaturity"
                                   , tempName = "temp24hr"
                                   , tempExtra = 0)
       , class = "hFunModel"
       , predictionFunction = predictMaturity
       , origdata = data.table::copy(data.table::as.data.table(dtw))
  )
}

#################################################### WIP ############
#################################################### WIP ############
#################################################### WIP ############
#################################################### WIP ############
#################################################### WIP ############
#################################################### WIP ############
#################################################### WIP ############

#' predict.hFunModel
#' 
#' @export
predict.hFunModel <- function(fitObject
                              , newdata
                              , tempColumn = "temp24hr"
                              , daysPerRow = 7
                              , n = 1){
  if (missing(newdata)){
    newdata = object$origdata
  }
  
  temp24hr <- as.data.frame(newdata)[, tempColumn]
  with(pars,
       predictMaturity(temperature = daysPerRow * temp24hr
                       , maturityDegreeDays = maturityDegreeDays
                       , baseTemp = baseTemp
                       , n = n)
  )
}
