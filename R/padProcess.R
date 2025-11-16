#' padProcess
#' 
# @example inst/example/ex_padProcess.R
#' @export
padProcess <- function(x
                       , fun = "fracShift"
                       , npad = 3
                       , padType = c("mirror", "mean"
                                     , "copy", "circular"
                                     , "tailValue", "constant", "NA")[1]
                       , constant = c(0, 0)
                       , keepNA = c("none", "left", "right", "both")[1]
                       , funargs = list()
                       ){
  
  x2 <- aphPad(x
               , s = npad
               , padType = padType
               , keepNA = keepNA
               , constant = constant)
  
  if (inherits(fun, "character")) {
    fun3 <- get(fun)
  }
  
  arg1name = names(formals(get(fun)))[1] # "x"" or "data"
  argList <- c(setNames(list(x2), arg1name), funargs)
  x2fr <- do.call(fun, argList)
  
  x2fr[seq(from = npad + 1, length.out = length(x))]
}
