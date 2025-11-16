#' aphReadExcel
#' 
#' @export
aphReadExcel <- function(ffxlsx, sheetId = 1){
  dt <- suppressMessages({
    suppressWarnings({
      dt <- readxl::read_excel(ffxlsx, sheet = sheetId, na = c("", "NA", "na"))
      dt <- readxl::read_excel(ffxlsx, sheet = sheetId, na = c("", "NA", "na"), guess_max = nrow(dt))
      dt <- as.data.table(dt)
      dt
    })
  })
  names(dt) <- make.names2(names(dt))
  structure(dt, ffxlsx = ffxlsx)
}