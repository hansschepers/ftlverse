#' applyBoundaries
#' @examples \dontrun{
#'   applyBoundaries(parms = list(a = 2, b = 3, c = 4, d = -1)
#'       , elems = c("a", "b", "d")
#'       , lower = c(0, 0, 0)
#'       , upper = 1
#'       )
#'
#'   funcChr <- "plantBalanceModel05Ode"
#'   Pars       <- do.call(get(paste0(funcChr, "Parms")), list())
#'   Boundaries <- do.call(get(paste0(funcChr, "Boundaries")), list())
#'   pp <- applyBoundaries(parms = Pars, Boundaries)
#' }
#'
#' @export
applyBoundaries <- function(parms
                            , Boundaries = NULL
                            , elems = names(parms)
                            , lower = -Inf
                            , upper = Inf
){
  if (length(Boundaries)){
    elems <- names(Boundaries$lower)
    lower <- unlist(Boundaries$lower)
    upper <- unlist(Boundaries$upper)
  }
  notFound <- setdiff(elems, names(parms))
  if (length(notFound)){
    log_trace("boundary elements not found: {paste(notFound, collapse = ', ')}")
    elems <- setdiff(elems, notFound)
  }
  log_trace("boundary elements: {paste(elems, collapse = ', ')}")
  parmsTmp <- unlist(parms[elems])

  nel <- length(elems)
  if (length(lower) < nel){
    lower <- rep(lower, nel)
    names(lower) <- elems
  }
  if (length(upper) < nel){
    upper <- rep(upper, nel)
    names(upper) <- elems
  }
  if (length(setdiff(elems, names(lower)))){
    if (length(lower) == nel){
      log_warn("assuming names of lower are elems in same order")
      names(lower) <- elems
    } else {
      stop("names of lower do not match elems")
    }
  }
  if (length(setdiff(elems, names(upper)))){
    if (length(upper) == nel){
      log_warn("assuming names of upper are elems in same order")
      names(upper) <- elems
    } else {
      stop("names of upper do not match elems")
    }
  }

  lower[is.na(lower)] <- -Inf
  upper[is.na(upper)] <-  Inf
  correctedLower <- parmsTmp < lower[elems]
  if (any(correctedLower)){
    log_warn("lower boundary applied: {paste(elems[correctedLower], collapse = ', ')}")
    log_warn("lower boundary applied: {paste(parmsTmp[correctedLower], collapse = ', ')}")
  }
  correctedUpper <- parmsTmp > upper[elems]
  if (any(correctedUpper)){
    log_warn("upper boundary applied: {paste(elems[correctedUpper], collapse = ', ')}")
    log_warn("upper boundary applied: {paste(parmsTmp[correctedUpper], collapse = ', ')}")
  }

  parmsTmp <- pmax(parmsTmp, lower[elems])
  parmsTmp <- pmin(parmsTmp, upper[elems])
  parms2 <- data.table::copy(parms)   # also works when parms is a list
  parms2[elems] <- parmsTmp
  parms2
}

