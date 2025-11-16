#' makeNamesAph
#'
#' @param nam input string to fix
#'
#' @return string with some characters replaced
#' @export
#'
#' @examples
#' \dontrun{
#'
#' nam <- ".naam_/...  .;"
#' makeNamesAph(nam)
#' }
makeNamesAph <- function(nam) {
  nam <- gsub("[:;]", "_", nam)
  nam <- gsub("/", "_", nam)
  nam <- gsub("\u2026", "", nam, fixed = TRUE)
  nam <- gsub(" ", "", nam)
  nam <- make.names(nam, allow_ = TRUE)
  nam <- gsub("^X", "", nam)
  # . are used to seperate files from folders
  gsub("\\.", "_", nam)
}
