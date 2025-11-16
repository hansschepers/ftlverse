#' cycleFocus
#' 
#' @export
cycleFocus <- function(DT){
  DTcyc <- copy(DT)
  if ("resetDay" %in% names(DTcyc)) DTcyc[, resetDay := NULL]
  if ("account_id" %in% names(DTcyc)) DTcyc[, account_id := NULL]
  if ("accountId" %in% names(DTcyc)) DTcyc[, accountId := NULL]
  DTcyc <- DTcyc[!is.na(value)]
  DTcyc <- DTcyc[!grepl("resetValue", processName)]
  DTcyc[]
}
