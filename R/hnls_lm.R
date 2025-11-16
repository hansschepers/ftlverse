#' hnls_lm
#'
#' @export
hnls_lm <- function(data
                    , lhs = "harvestMaturity"
                    , rhs = "temp24hr.clean"
                    , intercept = "icpt"
                    , algorithm = "port"
                    , ...
){
  formu <- makeNlsFormula(lhs, rhs, intercept
                          , ...
  )
  start <- setNames(rep(1, length(rhs)), paste0("p_", unique(rhs)))
  if (nchar(intercept) > 0){
    start = c(c(icpt = 0), start)
  }
  
  nls(formu
      , data = copy(data)
      , start = start
      , na.action = na.exclude
      , algorithm = algorithm)
}


#' hnls
#' @examples \dontrun{
#'   dtw <- data.table(temp24hr = 15:25)
#'   dtw[, harvestMaturity := 200 / (temp24hr - 8)]
#'   dtw
#'   aphNlsFit_HM <- hnls(data = dtw
#'           , formu = "harvestMaturity ~ maturityDegreeDays/7 / (temp24hr - tempBase)"
#'           , pars = list(maturityDegreeDays = 1200, tempBase = 10)
#'           , verbosity = 100)
#' }
#' @export
hnls <- function(formu
                     , data
                     , pars
                     , na.action = na.exclude
                     , algorithm = "port"
                     , verbosity = 0
                     , ...
){
  if ("tempExtra" %in% names(data)){
    start$tempExtra <- NULL
  }
  lhs <- all.vars(as.formula(formu)[[2]])
  rhs <- all.vars(as.formula(formu)[[3]])
  
  freePars <- setdiff(c(lhs, rhs), names(data))
  start <- pars[freePars]
  if (length(freePars)){
    if (verbosity > 0){
      log_info("freePars {freePars}")
      print(start)
    }
    if (NA %in% names(start)){
      unrecognizedTerms <- which(is.na(names(start)), arr.ind = TRUE)
      print(freePars[unrecognizedTerms])
      stop("unrecognizedTerms")
    }
  } else {
    log_error("no free parameters")
    stop("no free parameters")
  }
  
  missingPars <- setdiff(names(data), c(lhs, rhs))
  if (length(missingPars)){
    log_error("missingPars {missingPars}")
    # stop()
  }
  
  nls(formu
      , data = copy(data)
      , start = start
      , na.action = na.action
      , algorithm = algorithm
      , ...)
}


