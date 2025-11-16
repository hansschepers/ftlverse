#' as.characterFormula
#' @description 'deparses' a formula to character string.
#' @export
as.characterFormula <- function(formula){
  log_info("using aph method as.character.formula")
  Reduce(paste, deparse(formula, width.cutoff = 500))
}



#' trainModel
#' @examples \dontrun{
#'   source("inst/example/exmpl_trainModel.R")
#' }
#' @export
trainModel <- function(formula = makeFormula(lhs, rhs)
                       , data
                       , lhs 
                       , rhs
                       , modelEngine = c("lm", "svm", "nls")[1]
                       # , bycols = "plot_syn"
){
  # dtw <- copy(.ddd)
  dtw <- copy(data)
  if (missing(formula)){
    formula <- as.formula(makeFormula(lhs, rhs))
  }
  if (!inherits(formula, "formula")){
    formula <- as.formula(formula)
  }
  if (missing(lhs)){
    lhs <- all.vars(formula[[2]])
  }
  if (missing(rhs)){
    rhs <- all.vars(formula[[3]])
  }
  if (("weekno" %in% rhs) & !"weekno" %in% names(dtw)){
      dtw[, weekno := lubridate::isoweek(dateTime)]
  }
  
  formulaCHAR0 <- as.characterFormula(formula)
  formulaCHAR <- makeFormula(lhs, rhs)
  stopifnot(all.equal(formulaCHAR0, formulaCHAR0))
  
  modelId <- paste0(gsub(" ", "", formulaCHAR), "__", modelEngine)
  modelId
  
  # check variables ----
  stopifnot(all(c(lhs, rhs) %in% names(dtw)))
  
  # train
  engineArgList <- list(
    formula = as.formula(formula)
       , data = dtw
       , na.action = na.exclude
  )
  if (modelEngine == "nls"){
    modelEngine <- hnls
    engineArgList$start <- list()
  }
  
  # if (modelEngine == "svm") require(e1071)
  useDoCall <- FALSE
  if(useDoCall){
    MODEL <- do.call(match.fun(modelEngine), engineArgList)
  } else {
    if (modelEngine %in% c("lm", "svm")){
      callText <- paste0(
        modelEngine
        , "(formula = ", formulaCHAR, ", "
        , "data = dtw, "
        , "na.action = na.exclude)")
      log_info({"callText {callText}"})
      MODEL <- eval(parse(text = callText))
    }
  }
  .MODEL <<- MODEL
  res <- structure(MODEL
                   , class = c(class(MODEL), "aphMODEL")
                   , lhs = lhs, rhs = rhs, modelId = modelId
                   , modelEngine = modelEngine, formula = formula
                   , origData = data)
  # setNames(list(x = res), modelId)
  return(res)
}
