#' hdcast2
#'
#' @export
hdcast2 <- function(DT
                   , lhs = setdiff(names(DT), union(variables, value.var))
                   , rhs = variables
                   , value.var = aphValues(DT)
                   , variables = aphVariables(DT)
                   , skip = character()
                   , ...){
  setDT(DT)
  DT <- copy(DT)
  if (length(skip)) DT[, c(skip) := NULL]
  hdcast(DT
         , lhs = lhs
         , rhs = rhs
         , variables = variables
         , ...)
}

#' hdcast
#'
#' @export
hdcast <- function(DT
                   , lhs = base::union(dois, setdiff(fois, variables))
                   , rhs = variables
                   , value.var = aphValues(DT)
                   , dois = aphTimes(DT)
                   , fois = aphFactors(DT)
                   , variables = aphVariables(DT)
                   , fun.aggregate = "hmean"
                   , formu = makeFormula(lhs, rhs, swap)
                   , swap = FALSE
                   , removePanel = TRUE
                   , autoKey = TRUE
                   , ...) {
  if (is.null(DT)) {
    log_fatal("hdcast:| DT is null, cannot cast, returning NULL")
    return(NULL)
  }
  setDT(DT)
  DT <- copy(DT)
  if (any(c("all", ".") %in% lhs)){
    lhs <- base::setdiff(names(DT), c(value.var, rhs))
  } else {
    if (removePanel & "panel" %in% names(DT)){
      DT[, panel := NULL]
      # note always recompute defaults..
      variables = aphVariables(DT)
      lhs <- base::union(dois, base::setdiff(fois, variables))
      rhs <- variables
    }
  }
  formu <- makeFormula(lhs, rhs, swap)

  if (length(rhs) == 0 | nrow(DT) == 0){
    log_warn("hdcast|: nothing to cast on, returning NULL")
    print(sys.call())
    return(NULL)
  }

  if (inherits(fun.aggregate, "character")){
    if (!exists(fun.aggregate, mode = "function")){
      fun.aggregate <- hmean
    } else {
      fun.aggregate <- get(fun.aggregate)
    }
  }
  # str(fun.aggregate)
  checkForDoubles <- DT[, .N, by = c(lhs, rhs)]
  if (nrow(checkForDoubles[N > 1])){
    # checkForDoubles2 <- DT[!is.na(get(value.var)), .N, by = c(lhs, rhs)]
    checkForDoubles2 <- DT[!is.na(get(value.var)), .N, by = c(all.vars(parse(text = formu)))]
    if (nrow(checkForDoubles2[N > 1])){
      log_warn("hdcast| Value-ed-doubles in c(LHS, RHS)! - see object '.checkForDoubles2[N > 1]'")
      log_warn("hdcast| lhs {lhs}")
      log_warn("hdcast| rhs {rhs}")
      print(dim(checkForDoubles2[N > 1]))
      print(checkForDoubles2[N > 1][seq(min(6, nrow(checkForDoubles2)))])
      .checkForDoubles2 <<- copy(checkForDoubles2)
      # print(checkForDoubles2[N > 1])
      # if (!fun.aggregate %in% c("hlength", "length")){
      #   log_fatal("doubles in LHS of hdcast, while expecting to take mean!")
      # }
    } else {
      log_warn("hdcast| there were doubles in c(LHS, RHS), but with value = NA, check object '.checkForDoubles'")
      log_warn("hdcast| lhs {lhs}")
      log_warn("hdcast| rhs {rhs}")
      .checkForDoubles <<- copy(checkForDoubles)
      # print(dim(checkForDoubles[N > 1]))
      # print(checkForDoubles[N > 1])
    }
  }
  if (!autoKey){
    lhs <- union(".TMPindex", lhs)
    DT[, .TMPindex := .I]
    formu <- makeFormula(lhs, rhs, swap)
  }

  log_trace("formula {formu}")
  DTwide <- data.table::dcast(DT
                    , formula = formu
                    , fun.aggregate = fun.aggregate
                    , value.var = value.var
                    , ...
  )

  if (!autoKey){
    DTwide[, .TMPindex := NULL]
  }

  return(DTwide[])
}


#' aphCast
#'
#' @export
aphCast <- hdcast
