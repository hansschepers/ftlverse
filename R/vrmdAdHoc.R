#' vrmdAdHoc
#' 
#' @export
vrmdAdHoc <- function(productId = "p0"
                      , p_List = list()
                      , repId = paste(productId, htimestamp(digits.secs = 3), sep = "__")
                      , docuFun = NULL
                      , scriptPackage = "ftlPlot"
                      , localPath = NULL
                      # , folderS3 = NULL  # NOT copied
                      , showCode = FALSE
                      , reportTitle = productId
                      , doView = TRUE
                      , doRender = doView
                      , appendOnly = FALSE
                      , chunkPrefix = ""
                      # , section = list(openingSection = productId)
                      , ...
){
  productId <- gsub("__", "_", productId)
  if (!appendOnly){
    # init
    if (exists("myGlobalRmd", envir = .GlobalEnv)) rm(myGlobalRmd, envir = .GlobalEnv)
    if (exists("g.repId", envir = .GlobalEnv)) rm(g.repId, envir = .GlobalEnv)
    vrmdQuick(repId, reportTitle = reportTitle
              ,  ...
    )
    g.repId <<- repId
  } else {
    doView = FALSE
  }
  
  # vrmd(section = list(s1 = productId))
  vrmd(chapter = productId)
  g.doTabbed <- 3
  g.doTabbed <<- 3
  # if (!is.null(section)) vrmd(section = section)
  
  log_debug("------------------------------------------------------------39")
  nn <- names(p_List)
  obj <- seq_along(p_List)[1]
  for (obj in seq_along(p_List)){
    cat(names(p_List)[obj]) ; cat(", ")
    nm <- nn[[obj]]
    if (is.null(nm)) nm <- htimestamp()
    nm <- paste0(nm, chunkPrefix)
    
    log_debug("{obj}: for nm = {nm}")
    
    if (grepl("(^df)|(^dt)|(^DT)|(^d_)", nm)){
      vrmd("dfg"
           , dfg = p_List[[obj]]
           , sDom = "splfrt"
           , chunkId = nm #paste(productId, nm, sep = "__")
      )
    }
    if (grepl("^rh_", nm)){
      log_debug("detected rh (rhandsontable) for nm = {nm}")
      vrmd("add"
           , p = p_List[[obj]]
           , chunkId = nm
      )
    }
    if (grepl("(^chapter)", nm)){
      vrmd(chapter = sub("^chapter", "", sub("^chapter_", "", nm)))
    }
    if (inherits(p_List[[obj]], "ggplot")){
      if (!inherits(p_List[[obj]], "waiver")){
        vrmd("add"
             , p = p_List[[obj]]
             , chunkId = nm)
      }}
  }
  log_debug("------------------------------------------------------------73")
  
  if (!is.null(localPath)){
    if (file.exists(localPath)){
      vrmd(chapter = "code")
      vrmd("docuFun", docuFun = localPath, chunkId = "Code")
    }
  } else {
    vrmd("docuFun", docuFun = docuFun
         , scriptPackage = scriptPackage, showCode = showCode)
  }
  
  if (!appendOnly){
    if (doRender) renderPath <- vrmd("render")
    if (doView) vrmd("view")
    # if (!is.null(folderS3)) vrmd("copyS3", folderS3 = folderS3)
    # print(vrmd("info"))
  }
  repId
}
