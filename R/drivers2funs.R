#' drivers2funs
#' @export
drivers2funs <- function(drivers
                         , xoi = names(drivers)[1]
                         , yois = setdiff(names(drivers), xoi)
                         , rule = 2
                         , ...){
  sapply(yois, \(yoi) approxfun(drivers[[xoi]]
                                , drivers[[yoi]]
                                , rule = rule
                                , ...)
         , USE.NAMES = TRUE)
}