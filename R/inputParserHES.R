#' inputParserHES
#' @examples \dontrun{
#'   inputParserHES(list(variety_name = "strabini", not_runfun_argument = 5))
#'   inputParserHES(list(variety_name = "strabini", not_runfun_argument = 5), pos = -1)
#' }
#' @export
inputParserHES <- function(input = list()
                        , FUN = "runFunHES"
                        , CALLnames = "input"# character()
                        , pos = c(-1, 0)[2]
                        , pfx = "qzqz"
                        , allowedUnexpectedArguments = character()
                        , verbosity = logger::log_threshold()
){
  if (pos == 0){
    targetEnv <- new.env()
  } else {
    targetEnv <- as.environment(pos)
  }
  
  if (!length(input)){
    log_trace("inputParserHES| input has no length")
    return(input)
  }
  
  ############################# begin paste
  {
    FUNC <- get(FUN)
    funFormals <- formals(FUNC)
    if (!"list" == class(input)[1]) {
      log_debug("inputParserHES| input not a normal list")
      # input <- shiny::isolate(shiny::reactiveValuesToList( input ))
    }
    names(input) <- gsub(pfx, "", names(input))
    
    # if (!"none" %in% allowedUnexpectedArguments)
    {
      allowedItems <- setdiff(c(names(funFormals), allowedUnexpectedArguments), "extraSpecs")
      extraSpecs <- input[setdiff(names(input), allowedItems)]
      input <- input[allowedItems]
    }
    ignoredItems <- "timesSim"
    input <- input[setdiff(names(input), ignoredItems)]
    # str(input)
    
    inputNamesValid <- character()
    for (nm in names(input)) {
      if (!nm %in% CALLnames) {  # don't use if specified in Call
        log_debug("inputParserHES| using from input: {nm}")
        
        if (is.list(input[[nm]])){
          
          ll <- length(input[[nm]])
          log_debug("inputParserHES | length of list {nm}: {ll}")
          inputNamesValid <- c(inputNamesValid, nm)
          assign(nm, input[[nm]], targetEnv)
          
        } else {
          
          if (length(input[[nm]]) > 0){
            
            # ww <- is.na(input[[nm]])
            # message(nm) ; str(ww)
            
            # if (!is.na(input[[nm]])){
            inputNamesValid <- c(inputNamesValid, nm)
            assign(nm, input[[nm]], envir = targetEnv)
            # } else {
            #   log_debug("input '{nm}' is NA, not used")
            # }
            
          } else {
            
            log_debug("inputParserHES| input '{nm}' has length 0")
            
          }
        }
      } else {
        log_debug("inputParserHES| _not_ using from input as it appears in CALL itself: {nm}")
      }
    }
  }
  ############################# end paste
  inputNames <- names(input)
  if (pos == 0){
    res <- as.list(targetEnv)
  } else {
    return(inputNamesValid)
  }
  res
}
