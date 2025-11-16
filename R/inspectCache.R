#' inspectCache
#' 
#' @examples \dontrun{
#'   inspectCache()
#'   inspectCache("1005/plot_clean/kbb_tuin3_23/kbb_22_tuin_3.rds")
#' }
#' @export
inspectCache <- function(path = "lastMeta.sqlite"){
  message(path)
  rbindlist(lapply(c("PROD", "DEV", "")
                   , function(x) {
                     as.data.table(
                       file.info(file.path(getCacheDir(x), path))
                       , keep.rownames = TRUE)[
                         ,c("rn", "size", "mtime")]
                   }))
}
