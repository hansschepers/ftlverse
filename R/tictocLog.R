#' Time code using a timing stack system and log the results.
#' 
#' The function \code{\link{ticLog}} and \code{\link{tocLog}} time portions of the code using
#' a stack based system from the package \link[tictoc]{tictoc}. The functions
#' \code{\link{ticLog}} and \code{\link{tocLog}} are respectively wrappers for the functions
#' \code{\link[tictoc]{tic}} and \link[tictoc]{toc} with the additional enhancement of logging the timings 
#' with custom messages.
#' 
#' @param msg a single-length character string, specifying the message of the timing stack item.
#' @param ns a shiny namespace function (output from \code{\link[shiny]{NS}}). The function is used
#'        to prefix the shiny-module namespace in front of the message. Making clear from which 
#'        shiny-module the timing came from.
#' @param quiet single-length logical. Default = TRUE. See quiet argument 
#'        \code{\link[tictoc]{tic}}
#' @param ... other arguments passed onto \code{\link[tictoc]{tic}} and \code{\link[tictoc]{toc}}. 
#' 
#' @details
#' 
#' \code{\link{ticLog}} pushes a timing element on the stack with the provided \code{msg}.
#' In case a shiny namespace function is provided in \code{ns},
#' the message of the stack item will also be prefixed with: \code{paste0("MODULE-", ns(""), " ")}.
#' 
#' When a timing gets popped from the stack with \code{\link{tocLog}},
#' the following message gets logged (at INFO level): "TIMING: \emph{msg} : \emph{timing}".
#' Where \emph{msg} is the message of the stack item that was constructed message 
#' using \code{\link{ticLog}}. \emph{timing} is the elapsed time (in seconds) 
#' between the stack-item creation, with \code{\link{ticLog}}, and when it got popped with
#' \code{\link{tocLog}}.
#' 
#' The elapsed time is \link[base:round]{rounded} to 5 digits to avoid long numbers duo precision errors.
#' 
#' When \code{\link{tocLog}} wants to pop an empty stack, the following warning message
#' will then get logged: \code{TIMING: Timing stack is imbalanced! : 0}.
#' 
#' @return to be ignored.
#' @author ltuijnder
#' @name tictocLog
#' 
#' @examples \dontrun{
#' ticLog("Testing")
#' tocLog()
#' 
#' ns <- shiny::NS("myModule")
#' ticLog("Testing",ns)
#' tocLog()
#' }
NULL


#' ticLog
#' @rdname tictocLog
#' @export
ticLog <- function(msg, ns = NULL, ...){
  if(is.null(ns)){
    tictoc::tic(msg, ...)
  }else{
    tictoc::tic(paste0("MODULE-", ns(""), " ", msg), ...)
  }
}


#' tocLog
#' @rdname tictocLog
#' @export
tocLog <- function(quiet = TRUE, level = "success", ...){
  
  out <- tictoc::toc(quiet = quiet, ...)
  
  if(is.null(out)){
    logger::log_warn("TIMING: Timing stack is imbalanced! : 0")
  } else {
    msg <- paste0("TIMING: ", out$msg," : ", round(out$toc - out$tic, 5))
    switch(level
           , fatal = logger::log_fatal(msg)
           , success = logger::log_success(msg)
           ,    logger::log_info(msg)
    )
  }
}
