#' openScreen
#'
#' Open Windows external screen (faster than within RStudio plotting pane)
#'
#' @author Hans Schepers, \email{hans.schepers@@monsanto.com}
#' @references \url{http://www.advancedgrowers.info}
#' @keywords Windows
#' @export
openScreen <- function(width=15, height=10, closeAll=TRUE){
  if (!.Platform$OS.type == "windows") return(invisible(NULL))
  if (closeAll) while(length(dev.list())) dev.off()
  windows(record=TRUE, width=width, height=height, restoreConsole=TRUE)  # History --> Recording
}





#' hsystem.file
#' 
#' @export
hsystem.file <- function(pack.oi = "aphReferenceData", path = ""){
  # instead of
  # file.path(Sys.getenv("APHPACK"), pack.oi, "inst", path)
  file.path(dirname(system.file("NAMESPACE", package=pack.oi)), path)  
}



#' hstr
#' 
#' base::str with modified defaults
#' 
#' @author Hans Schepers, \email{hans.schepers@@monsanto.com}
#' @export
hstr <- function(x, vec.len=1, max.level = 2, nchar.max=20, give.attr=FALSE, ...) {
  str(x, vec.len = vec.len, max.level = max.level, give.attr=give.attr,
      nchar.max=nchar.max, ...)
}


#' as.numeric2
#' 
#' @examples \dontrun{
#'    str(as.numeric2(c("3.4 ", "5,66 ", NA, "3-4")))
#'    str(as.numeric2(c("3.4 ", "5,66 ", NA, "3-4"), dec=","))
#' }
#' @export
as.numeric2 <- function(x, dec=".", verbose = FALSE){
  na1 <- is.na(x)
  x2 <- x
  if(dec[1] == ",") {
    x2 <- as.numeric(gsub("\\s", "", gsub(",", ".", x2)))
  }
  x2 <- as.numeric(x2)
  na2 <- is.na(x2)
  tt <- na2 & (!na1)
  if(sum(tt)){
    if(verbose){
      message(paste0("Note: NA's increased from ",sum(na1), " to ", sum(na2)))
      message("for these raw values")
      print(x[tt])
    }
    x2 <- structure(x2, newNA=x[tt])
  }
  x2
}
