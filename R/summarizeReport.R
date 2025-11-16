#' summarizeReport
#' 
# @import data.tree
#' @export
summarizeReport <- function(reportList, doField=FALSE){
  # hstr(reportList, 1, 2)
  # return(invisible("ok"))
  if (doField){
    print(data.tree::FromListSimple(reportList), fields = function(n) paste(n$fields, collapse = ", "))
  } else {
    print(data.tree::FromListSimple(reportList))
  }
}
