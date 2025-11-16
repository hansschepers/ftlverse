#' makeModel
#' @description generic funtion to create simple machine learning models like
#'   svm or lm with columns in a data.table
#' @param DT the data.table which contains the data to create the model from
#' @param ooi output or response variable name. Must be present in the \code{DT}
#' @param ioi variable names of the input variables of the model. all names must
#'   be present as a column in the \code{DT}
#' @param fn the function to be used. Defaults to svm
#' @param ... additional arguments to pass to the function
#' 
#' @export
makeModel <- function(DT, ooi, ioi, fn = e1071::svm, ...){
  
  if (length(ooi) != 1) {
    logger::log_error("a single output variable is required to make a prediction")
    return(NULL)
  }
  if (!all(c(ooi,ioi) %in% names(DT))){
    logger::log_error("all variables must be present as a column in DT")
    return(NULL)
  }
  formu <- as.formula(paste(ooi,"~",paste(ioi,collapse="+")))
  model <- fn(formu,data = DT, ...)
  return(model)
}
