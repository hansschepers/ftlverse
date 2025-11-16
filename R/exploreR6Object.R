#' exploreR6Object
#' @examples \dontrun{
#'   all.vars(body(function(x) kk$jj))
#'   tempModel <- ModelLinear$new(target = "GHTEMP", features = c("OUTEMP","RAD"),
#'                                   dateCol = "dateTime", id = "testTemperaturePred")
#'   exploreR6Object(tempModel)[1:3]
#'   data <- DataCycleActive$new(accountId = "1046"
#'                                , processNames = c("GHTEMP", "OUTEMP", "RAD"))
#'   data$getData()
#'   tempModel$train(data = data)
#'   exploreR6Object(tempModel)[1:2]
#' }
#' @export
exploreR6Object <- function(objR6){
  exclude <- c("self", "private")
  funs <- sapply(names(objR6), \(x) is.function(objR6[[x]]))
  atts <- names(funs[!funs])
  meths <- names(funs[funs])
  # x <- "save"
  defaultArgs <- sapply(meths, \(x) formals(objR6[[x]])
                     , simplify = FALSE, USE.NAMES = TRUE)
  methArgs <- sapply(defaultArgs, names, simplify = FALSE, USE.NAMES = TRUE)
  allVars = sapply(meths, \(x) setdiff(all.vars(body(tempModel[[x]]))
                                       , exclude)
                   , simplify = FALSE, USE.NAMES = TRUE)
  list(atts = atts
       , meths = meths
       , methArgs = methArgs
       , defaultArgs = defaultArgs
       , allVars = allVars
  )
}

