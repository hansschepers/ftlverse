# account_names <- c("Bryte", "Van_Adrichem")
# account_ids <- getFactorLevels(cacheStore$conn, "account_id", account_name = account_names)$value
# account_ids <- setdiff(account_ids, "1048")
# account_ids

# qaList <- aphQC(DTagri)

#' getS3CleanData
#' 
#' @export
getS3CleanData <- function(accountId
                         , store
                         , api = c("agrisensys", "lg")[1]
                         , cleanVersion = c("Small", "Collect")[1]
                         , doi = "dateTime"
                         , timeResolutionChosen = c("orig", "hour", "3hours", "day", "week")[1]
                         # , doAddMeta = TRUE
                         ){
  
  getCleanData <- function(accountId, store, api, cleanVersion) {
    (path <- file.path(accountId, api, paste0("clean", cleanVersion, ".rds")))
    res <- tryCatch(readObject(store, path), error = print)
    res
  }
  
  DTlist <- lapply(setNames(accountId, accountId)
                   , getCleanData
                   , store = store, api = api, cleanVersion = cleanVersion)
  DT <- rbindlist(DTlist, use.names = TRUE, idcol = "accountId")
  if (log_threshold() > 400){  .DT00 <<- DT  }
  bycols <- c(aphFactors(DT), doi)
  
  # aggregate doi / dateTime ----
  if(timeResolutionChosen != "orig") {
    timeResolution <- setNames(c(5, 5, 3600, 3*3600, 24*3600, 7*24*3600)
                               , c("orig", "5min", "hour", "3hours", "day", "week"))
    
    DT[, (doi) := xts::align.time(base::get(doi), timeResolution[timeResolutionChosen])]
    DT <- DT[, list(value = hmean(value)), by = bycols]
  }
  setkeyv(DT, bycols)
  return(DT)
}



#' addMetaAgrisensys
#' 
#' @export
addMetaAgrisensys <- function(DTagri, store){
  DTagri <- addMeta(DT = copy(DTagri)
                    , metaConn = store$conn
                    , fac.oi = 'name_department_letsgrow'
                    )
  setnames(DTagri, 'name_department_letsgrow', 'differentiator')
  
  if (log_threshold() >= 400) print(DTagri[, .N, by = differentiator])
  
  if (sumna(DTagri$differentiator) > 0){
    log_warn("NAs in differentiator!")
    # not necessary?
    # DTagri[is.na(differentiator), differentiator := 'afd_6_gr6']
  }
  # DTagri[, cycle_name := NULL]
  aphKey(DTagri)
  return(DTagri)
}
