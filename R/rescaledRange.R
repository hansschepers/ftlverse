#' rescaledRange
#' @examples \dontrun{
#'   dt <- copy(dtm[variable %in% c("rain.mm", "rh", "radiation", "temperature")])
#' }
#' @export
rescaledRange <- function(dt
                          , dois = aphTimes(dt)
                          , doisOut = "dateTime" # c("hour", "hrSlot")
                          , fois = aphFactors(dt, addClasses = character(0))
                          , variable.name = aphVariables(dt)
                          , hfuns = c("hdiffrange", "hmean", "hlength", "hmin", "hmax", "hsd")[1:6]
                          , verbosity = 1
){
  res <- list()
  byCols = union(fois, variable.name)
  if (verbosity > 0){
    message("byCols: ")
    print(byCols)
  }
  
  tmp <- dt[, .(value = hmean(value)), by = c(byCols)]
  res[["all"]] <- tmp[, hsummary(value, hfuns = hfuns), by = c(byCols)]
  
  dois <- setdiff(dois, doisOut)
  iiDoi <- 1
  for (iiDoi in seq_along(dois)){
    byColsii <- c(byCols, dois[seq(iiDoi)])
    byColsii
    tmp <- dt[, .(value = hmean(value)), by = c(byColsii)]
    dd <- tmp[, hsummary(value, hfuns = hfuns), by = c(byCols)]
    message(dois[iiDoi])
    if (verbosity > 0) {
      print(dd)
    }
    res[[dois[iiDoi]]] <- dd
  }
  # res[["all"]] <- NULL
  resdt <- rbindlist(res, idcol = "doi")
  dois <- c("all", dois)
  resdt[, doi := factor(doi, levels = dois, ordered = TRUE)]
  .resdt <<- resdt
  if ("hlength" %in% names(resdt)){
    resdt[, hlength := as.numeric(hlength)]
  }
  # setdiff(names(resdt), union("doi", byCols))
  resdtm <- melt(resdt, id.vars = union("doi", byCols)
                 , variable.factor = FALSE
                 , variable.name = "kpi")
  # resdtm[kpi == "hlength"]
  if (verbosity > 0) {
    print(dcast(resdtm[kpi == hfuns[1]]
                , makeFormula(union(c("kpi", "doi"), setdiff(byCols, variable.name)), variable.name)
                , value.var = "value"
    )  )
  }
  resdtm
}
