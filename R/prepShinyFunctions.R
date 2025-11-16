#' hsaveShinyInput
#' @importFrom shiny reactiveValuesToList
#' @export
hsaveShinyInput <- function(input = shiny::reactiveValues(test = 1)
                            , lastInputFileName = tempfile(fileext = "rds")
                            , S3path = file.path("DigiTwin"
                                                 , Sys.getenv("USERNAME")
                                                 , "autoSaveShinyInputs")
                            , store = list()
                            , nms
){
  nms <- file.path(getCacheDir()
            , paste0(hdatestamp(), "lastShinyInput.rds"))
  
  message("saving to ", lastInputFileName)
  hinput <- shiny::reactiveValuesToList(input)
  saveRDS(hinput, lastInputFileName)
  if (dir.exists(getCacheDir())){
    message("a copy to ", nms)
    file.copy(lastInputFileName, nms)
  }
  if (length(store)){
    message("saving on S3 to ", S3path)
    writeObject(store, hinput, S3path)
  }
  lastInputFileName
}




#' hstopApp
#' 
#' @export
hstopApp <- function(users_data = list()
                     ) {
  users_data$END <- Sys.time()
  # Write a file in your working directory
  write.table(x = users_data
              , file = file.path(getCacheDir(), "users_data.txt")
              , append = TRUE
              , row.names = FALSE
              , col.names = FALSE, sep = "\t")
  hsaveShinyInput(input, lastInputFileName)
  print(lastInputFileName)
  shiny::stopApp(hinput)
}
