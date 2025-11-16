#' reportDefaults
#' @examples \dontrun{
#'   {
#'   reportList <- reportDefaults()
#'   summarizeReport(reportList)
#'   summarizeReport(reportList, doField = TRUE)
#'   reportList$readStore$backend
#'   reportList$store$backend
#'   }
#' }
# @import DBI
#' @export
reportDefaults <- function(account_id = "1005"
                           , reportId = "testReport"
                           , filePath = "devTest"
                           , prefix = "hhsche2/_moduleLake"
                           , configList = configDefaults()
                           , getMeta = FALSE
                           , store = storeDefaults(configList = configList, prefix = prefix)  # default: , getMeta=FALSE
                           , storeLocal = TSLocalStore()
                           , storeS3    = TSS3Store()
                           , readStore = storeDefaults(configList = configList, getMeta=getMeta)
                           # , prodStore = storeDefaults(configChoice = "defaultPROD", getMeta=getMeta)
                           # , ...
){
  
  reportList <- list(
    admin = list(
      ids = list(
        reportId = reportId
        # , cycle_name = reportId
        , title = paste(reportId)
        , filePath = filePath
        , account_id = as.character(account_id)
      )
      , configList = configList
      , author = toupper(Sys.getenv("USERNAME", "unknown"))
      , userName = toupper(Sys.getenv("SHINYPROXY_USERNAME", "unknown"))
      , userRoles = trimws(unlist(strsplit(toupper(Sys.getenv("SHINYPROXY_USERGROUPS", "unknown")), ",")))
    )
    , readStore = readStore
    , store = store
    , storeLocal = storeLocal
    , storeS3    = storeS3
    , timestamps = list(
      start = format(Sys.time(), "%Y%m%d_%H%M%d")
    )
    , problem = list(
      description = ""
      , objective = "minimize"
      , costFunction = "costCohort"
    )
    , shinyInput = list()
    , data = list(
      sensors = list(
        startWith = "clean"
      )
      , drivers = list(
        weather = ""
        , agronomicLogs = ""
      )
      , weekData = ""
      , events = ""
      , dataProcessing = list(
        smooth = ""
        , outliers = ""
        , aggregate = ""
        , focus = ""
      )
    )
    , sim = list()
    , knowledge = list(
      model = "cohortSim"
      , parameters = ""
      , hyperParameters = ""
    )
    , products = list(
      plantActivity = ""
      , plantBalance = ""
      , Prediction = ""
      , Warning = ""
    )
  )
  # filename where this report will be (automatically) saved
  reportList$admin$rdsFileName <- file.path(reportList$admin$ids$filePath
                                            , paste0(reportList$admin$ids$reportId, ".rds"))
  
  reportList
}
