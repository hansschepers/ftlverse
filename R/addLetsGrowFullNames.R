#' addLetsGrowFullNames
#' 
#' @export
addLetsGrowFullNames <- function(DT, store){
  
  lgTrans <- as.data.table(DBI::dbReadTable(store$conn, "lg_item"))
  
  setnames(lgTrans, c("item_code", "measure"), c("processName", "fullName"))
  
  lgTrans <- lgTrans[, .(processName, fullName)]
  
  lgTrans <- unique(lgTrans[processName %in% unique(DT$processName)])
  
  DT <- data.table::merge(unique(DT), lgTrans, on = "processName", all.x = T)
  
  DT[is.na(fullName), fullName := processName]
  
  return(DT)
}

