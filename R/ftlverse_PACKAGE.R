#' ftlverse
#'
#' @keywords internal
"_PACKAGE"

# order matters below; this will be an intended warning:
# Warning message:
#   replacing previous import ‘data.table::wday’ by ‘lubridate::wday’ when loading ‘ftlverse’ 

#' @importFrom data.table as.data.table copy data.table dcast frollmean is.data.table key melt rbindlist
#' @importFrom data.table set setcolorder setkeyv setnafill setnames := %chin% .SD CJ .BY nafill frollapply shift
#' @importFrom data.table setDT setDF setkey setattr last first setorderv uniqueN tstrsplit
#' @importFrom logger log_trace log_debug log_info log_warn log_error log_fatal log_threshold
#' @importFrom lubridate year quarter month isoweek floor_date yday hour mday minute yday is.Date now today seconds minutes hours days years
# @import aphLite
# @importFrom aphLite compareLists hprettyNum universalConstants aphMelt hdcast aphVariableLevels aphTimes aphFactors

#' @importFrom logger log_info log_debug log_warn log_error log_fatal log_threshold log_trace
#' @importFrom deSolve ode

#' @importFrom stringi stri_wrap
NULL

#' @importFrom glue glue
NULL

# quiets concerns of R CMD check re the .'s that appear in data.tables
#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
