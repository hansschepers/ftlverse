#' newls
#' @examples
#' \dontrun{
#'   newls()
#'   newls(NAMES00)
#'   newls(NAMES0)
#' }
#' @export
newls <- function(NAMES0 = character()
                  , envir = .GlobalEnv){
  setdiff(ls(envir = envir), union(c("NAMES0", "newls"), NAMES0))
}
