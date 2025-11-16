#' xlsx2csv
#' @examples \dontrun{
#'   xlsx2csv()
#' }
#' @export
xlsx2csv <- function(fileName = "model_info"
                     , metaDataDir
                     , sheet.s = "auto"
                     , verbosity = 0){
  xlsFiles <- list.files(metaDataDir
                         , pattern = "\\.xlsx"
                         , full.names = TRUE
                         )
  file.info(xlsFiles[1])
  stopifnot("readxl" %in% list.files(.libPaths()))
  xlsxpath <- file.path(metaDataDir, paste0(fileName, ".xlsx"))
  print(hfile.info(xlsxpath))

  stopifnot(file.exists(xlsxpath))
  tryCatch({sheetAvailable.s <- readxl::excel_sheets(xlsxpath)}
           , error = function(e)
             stop("file exists, but can't open it, likely you have the file open in excel?!")
  )
  if ("all" %in% sheet.s){
    sheet.s <- sheetAvailable.s
  }
  if ("auto" %in% sheet.s){
    sheet.s <- setdiff(sheetAvailable.s
                       , c("TypeOfModel", "Sheet1", "Sheet2"))
  }
  sheet.s
  sheet <- sheet.s[1]
  for (sheet in sheet.s){
    message(sheet)
    sheetContents <- readxl::read_excel(xlsxpath, sheet = sheet)
    csvPath <- file.path(metaDataDir, paste0(fileName, "__", sheet, ".csv"))
    ok <- write.csv(sheetContents, file = csvPath, row.names = FALSE)
    hfile.info(csvPath)
  }
}

