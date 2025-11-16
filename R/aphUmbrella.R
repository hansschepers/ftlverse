#' aphUmbrella
#'  
#' adds value.cu.ce or its 'cousins' to a data.frame or data.table
#' @examples \dontrun{
#'    library(data.table)
#' }
# @seealso [stabilityPlot]
#' @export
aphUmbrella <- function(dfg
                     , doi = rev(aphTimes(dfg))[1]
                     , yoi = aphValues(dfg)
                     , foi = aphFactors(dfg)
                     , xsync = doi #"wsfh"
                     , byGroups = NULL, fois = NULL
                     , variable.name = aphVariables(dfg)
                     , keep = c("n", ".ce", ".cu", ".cu.ce", ".cu.mean", ".mean", ".zmean")[2:4]
                     ){
  if (isTRUE(is.na(doi))) doi <- character(0)
  if (isTRUE(is.na(xsync))) xsync <- character(0)
  # backward compatibility:
  if (is.null(byGroups) & !is.null(fois)){
    byGroups <- fois
  }
  
  wasDT <- inherits(dfg, "data.table")
  if (!wasDT){
    DT <- as.data.table(dfg)
  } else {
    DT <- data.table::copy(dfg)
  }
  
  if (variable.name %in% names(DT)){
    message(paste("adding ", variable.name, " to byGroups"))
    byGroups = base::union(variable.name, byGroups)
  }
  if (yoi %in% names(DT)){
    message("renaming")
    setnames(DT, yoi[1], "value")
  } else { 
    stop("yoi not found in data")
  }
  .DT0 <<- DT
  # summarize means
  DT <- DT[, .(value = hmean(value)), by = c(doi, foi, byGroups)]
  # 'sort'
  setkeyv(DT, c(byGroups, foi, doi))
  # add 'week since first harvest'
  if (isTRUE(xsync != doi)){
    DT[, c(xsync) := seqMiddle(value), by = c(foi, byGroups)]
  }
  # DT[, c(xsync) := list(seqMiddle2(value)), by = c(foi, byGroups)]
  setkeyv(DT, c(byGroups, foi, xsync))
  DT[, value := interNAZoo(value), by = c(foi, byGroups)]
  # add cumulative
  DT[, value.cu := aphCumsum(value), by = c(foi, byGroups)]
  # DT[, value.cu := mutateMiddle2(value, FUN = "cumsum"), by = c(foi, byGroups)]
  # add centered versions (.ce and .cu.ce)
  DT[,          n := hlength(value),    by = c(xsync, byGroups)]
  DT[, value.mean    := hmean(value),    by = c(xsync, byGroups)]
  DT[, value.cu.mean := hmean(value.cu), by = c(xsync, byGroups)]
  DT[, value.ce := value - value.mean]
  DT[, value.cu.ce := value.cu - value.cu.mean]
  DT[, value.zmean := hmean(value), by =  byGroups ]
  
  extension.s <- c(".ce", ".cu", ".cu.ce", ".cu.mean", ".mean", ".zmean")
  extension.s <- extension.s[ paste0("value", extension.s) %in% names(DT) ]
  keep <- keep[ paste0("value", keep) %in% names(DT) ]
  # DT[,.(paste0("value", setdiff(extension.s, keep))) := NULL ]
  DT[,paste0("value", setdiff(extension.s, keep))] <- NULL
  setnames(DT, 
           old = paste0("value", keep), 
           new = paste0(yoi, keep))
  names(DT)
  if (!wasDT) setDF(DT)
  DT[]
}

