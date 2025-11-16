# options(datatable.print.topn = 4)
# options(datatable.print.nrows = 20)
# options(datatable.print.class = TRUE)
# options(datatable.print.keys = TRUE)
# usethis::edit_r_profile()
#' hhead
#' @importFrom data.table setDT
#' @export
hhead <- function(dt
                  , topn = 3, nrows = 30, class=TRUE, keys = TRUE
                  , trunc.cols = TRUE, timezone = TRUE){
  setDT(dt)
  print(dt, topn = topn, nrows = nrows, class=class, keys = keys, trunc.cols = trunc.cols, timezone = timezone)
}
