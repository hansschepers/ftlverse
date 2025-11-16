#' hlst0
#' 
#' a base:: version of tibble::lst
#' evaluates the list elements sequentially, using prior elements as given
#' 
#' @examples \dontrun{
#'   hlst0(a=4, b=5+a)
#'   hlst0(a = 4, b = 5 + c, c = 2)
#' }
#' @return list
#' @export
hlst0 <- function(...){
  listToEval <- match.call()
  # forget name of function in call
  listToEval <- as.list(listToEval)[-1]
  
  out <- vector(mode = "list", length = length(listToEval))
  names(out) <- names(listToEval)
  
  for (ii in seq_along(listToEval)) {
    value <- listToEval[[ii]]
    if (is.language(value)) {
      value <- eval(value, envir = listToEval)
    }
    out[[ii]] <- value
  }
  out
}



#' lstUseEnv
#' as lst(), but can start from an existing environment 
#' 
#' @examples \dontrun{
#'   priorList <- list(f2 = function(x) {-x})
#'   with(priorList, dplyr::lst(a = 4, b = f2(a)))
#'   with(priorList, dplyr::lst(b = f2(a), a = 4))
#'   with(priorList, dplyr::lst(a = 4, b = do.call(f2, list(a = a))))
#'   eval(dplyr::lst(a = 4, b = f2(a)), envir = as.environment(priorList))
#'   ww <- as.environment(priorList)
#'   ls(ww)
#'   get("f2", ww)
#'   
#'   # lstUseEnv works the same as lst()
#'   qq <- lstUseEnv(a=4, b=5+a)
#'   print(qq)
#'   
#'   # prior knowledge (constants, functions)
#'   priorEnv <- new.env()
#'   assign("parmMerlice", 100, envir = priorEnv)
#'   assign("f2", function(x, parm) {-x + parm}, envir = priorEnv)
#'   ls(priorEnv)
#'   get("f2", priorEnv)(11, 33)
#'   qq <- lstUseEnv(priorEnv = priorEnv, a = 4, b = f2(a, parm = parmMerlice))
#'   qq
#'   str(qq$b)
#'   
#'   priorList <- list(f2 = function(x) {-x})
#'   # works
#'   lstUseEnv(a = 4, b = 5 + c, c = 2, c = 6, d = function(x) {x+11}, e = d(11))
#'   lstUseEnv(b = priorList$f2(a), a = 4)
#'   with(priorList, lstUseEnv(b = priorList$f2(a), a = 4))
#'   # does not work
#'   lstUseEnv(penv, b = f2(a), a = 4)
#'   with(priorList, lstUseEnv(b = f2(a), a = 4))
#'   lstUseEnv(a = 4, b = 5 + c, c = 2, c = 6, e = d(11), d = function(x) {x+11})
#'   lstUseEnv(a = 4, b = 5 + c, c = 2, c = 6, d = function(x) {x+11}, e = f2(11))
#'   penv <- as.environment(priorList)
#'   lstUseEnv(priorEnv = penv, b = f2(a), a = 4)
#'   lstUseEnv(priorEnv = penv, f2 = f2(a), a = 4)
#' }
#' 
#' @export
lstUseEnv <- function(priorEnv = new.env(), ...){
  listToEval <- match.call()
  # forget name of function in call
  listToEval <- as.list(listToEval)[-1]
  # listToEval <- as.list(listToEval)[-(1:2)]
  listToEval$priorEnv <- NULL
  # listToEval <- rev(listToEval)
  for (ii in seq_along(listToEval)){
    value <- expression(listToEval[[ii]])
    if (is.language(value)){
      value <- eval(value, envir = priorEnv)
    }
    # listToEval[[names(listToEval[ii])]] <- value
    # if (is.function(eval(value))){
    #   message("is function")
    #   assign(names(listToEval[ii]), eval(value), envir = priorEnv)
    # } else {
    #   message("is not a function")
      assign(names(listToEval[ii]), value, envir = priorEnv)
    # }
  }
  # message("-----------")
  # print(mget(ls(priorEnv), envir = priorEnv))
  # message("-----------")
  for (ii in seq_along(listToEval)){
    value <- listToEval[[ii]]
    if (is.language(value)){
      value <- eval(value, envir = priorEnv)
    } 
    assign(names(listToEval[ii]), value, envir = priorEnv)
  }
  # print(listToEval)
  mget(ls(priorEnv), envir = priorEnv)
}


# ww <- list(a = function() 4, b = function() 5 + c(), c = function() 2)
# with(ww, b())
# lst(a = function() 4, b = function() 5 + c(), c = function() 2)



# lstUseEnvNathanstyle <- function(prior = list(), ...){
#   listToEval <- match.call()
#   # forget name of function in call
#   listToEval <- as.list(listToEval)[-1]
#   # if ("prior" %in% names(listToEval)){
#   #   listToEval$prior <- NULL
#   # }
# 
#   for (ii in seq_along(listToEval)){
#     value <- listToEval[[ii]]
#     if (is.language(value)){
#       value <- eval(value, envir = listToEval)
#     }
#     listToEval[[ii]] <- value
#     assign(names(listToEval[ii]), value, envir = prior)
#   }
#   print(as.list(prior))
#   listToEval
# }
# 


#' lstNE
#' 
#' @export
lstNE <- function(...) {
  fnCall <- match.call()
  listToEval <- as.list(fnCall)[-1]
  nms <- names(listToEval)
  missingNms <- if (is.null(nms)) 1L else nms == ""
  names(listToEval)[missingNms] <- do.call(c, lapply(
    listToEval[missingNms],
    function(x) deparse(x)
  ))
  out <- vector(mode = "list", length = length(listToEval))
  names(out) <- names(listToEval)
  for (element in seq_along(listToEval)) {
    value <- listToEval[[element]]
    if (is.language(value)) {
      evaled <- eval(
        expr = value,
        envir = out[!duplicated(names(out)[seq_len(element)], fromLast = TRUE)]
      )
      if (length(evaled) == 0L & is.language(value)) {
        evaled <- eval(value, envir = listToEval)
      }
      if (!is.null(evaled)) out[[element]] <- evaled
    } else if (!is.null(value)) {
      out[[element]] <- value
    }
  }
  out
}
# a <- 1:3
# b <- letters[4:6]
# lstNE(a, b)
# tibble::lst(a, b)

