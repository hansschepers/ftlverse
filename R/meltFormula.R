#' meltFormula
#' @examples \dontrun{
#'   df <- data.frame(a = 1, c = 4, c.pred = 5)
#'   meltFormula(df, formu = "c")
#'   meltFormula(df, formu = "c", keepWide = TRUE)
#' }
#' @description selects the formula-variables from a data.table
#' @param ext char vector with extensions to also pick up
#' @importFrom data.table as.data.table
#' @export
meltFormula <- function(dtw
                        , formu
                        , keepWide = FALSE
                        , ext = c("", ".pred")
){
  # convert 'formu' into all its variables
  if (is.character(formu)) {
    formu <- paste0(formu, "~ .")
  }
  variables <- all.vars(as.formula(formu))
  variables <- setdiff(variables, ".")
  variables <- trimws(variables)
  
  # add requested extensions
  variables <- unlist(lapply(ext, function(x) paste0(variables, x)))
  
  regex <- paste0("(^", paste(variables, collapse = "$)|(^"), "$)")
  # message(regex)
  if (!keepWide){
    dt <- aphMelt(data.table::as.data.table(dtw))[grepl(regex, processName)]
  } else {
    keep <- c(aphFactors(dtw), grep(regex, names(dtw), value = TRUE))
    dt <- data.table::as.data.table(dtw)[, ..keep]
  }
  aphKey(dt)
  dt
}



#' selectFormula
#' @examples \dontrun{
#'   df <- data.frame(a = 1, c = 4, c.pred = 5)
#'   selectFormula(df, formu = "c")
#'   selectFormula(df, formu = "c", keepWide = TRUE)
#' }
#' @description selects the formula-variables from a data.table
#' @param ext char vector with extensions to also pick up
#' @importFrom data.table as.data.table
#' @export
selectFormula <- function(dtw
                        , formu
                        , domelt = FALSE
                        , docast = FALSE
                        , ext = c("", ".pred")
                        , ...
){
  # convert 'formu' into all its variables
  if (is.character(formu)) {
    formu <- paste0(formu, "~ .")
  }
  variables <- all.vars(as.formula(formu))
  variables <- setdiff(variables, ".")
  variables <- trimws(variables)
  
  # add requested extensions
  variables <- unlist(lapply(ext, function(x) paste0(variables, x)))
  
  regex <- paste0("(^", paste(variables, collapse = "$)|(^"), "$)")
  # message(regex)
  if (!"processName" %in% names(dtw)){
    # was wide
    keep <- c(aphFactors(dtw), grep(regex, names(dtw), value = TRUE))
    dt <- data.table::as.data.table(dtw)[, ..keep]
    if (domelt){
      dt <- aphMelt(dt, ...)
    }
  } else {
    # was long
    dt <- dtw[grepl(regex, processName)]
    if (docast){
      dt <- hdcast(dt, ...)
    }
  }
  dt
}
