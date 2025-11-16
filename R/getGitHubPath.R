#' getGitHubPath
#' @examples \dontrun{
#'   getGitHubPath(docuFun = "pggs")
#' }
#' @export
getGitHubPath <- function(docuFun
                          , scriptPackage = basename(getwd())
                          , githubPath = "https://github.com/bayer-int/"){

  ffrLocal <- ""
  if (file.exists(docuFun)){
    # a script
    ffrLocal <- docuFun
  } else {
    # a function
    FUN <- match.fun(sub("Sim$", "", docuFun))
    ffrLocal <- utils::getSrcFilename(FUN, full.names = TRUE)
  }

  ffr <- ffrLocal

  if (substr(scriptPackage, 1, 4) != "aph-"){
    scriptPackage <- paste0("aph-", scriptPackage)
  }

  ffr <- sub("~/aphDH", githubPath, ffr)
  # ffr <- sub("/inst/",  "/blob/main/inst/", ffr)
  ffr <- sub("^inst/",  paste0(githubPath, scriptPackage,"/blob/main/inst/"), ffr)
  ffr <- sub("^(\\.\\./)(.*/)(inst/)",  paste0(githubPath, "\\2", "blob/main/inst/"), ffr)
  ffr <- sub("^(\\.\\./)(.*/)(R/)",     paste0(githubPath, "\\2", "tree/main/R/"), ffr)
  ffr <- sub("^R/",  paste0(githubPath, scriptPackage, "/tree/main/R/"), ffr)
  log_trace("ffr: {ffr}")
  if (!length(ffr)) {
    log_error("no meaningful docufun filename found..")
  }
  if (grepl("^https", ffr)) {
    # ffr <- sub("/R/", "/tree/main/R/", ffr)
    log_info("linking to file location on Github: {ffr}")
  } else {
    ffr <- paste0("file:///", ffr)
    log_info("linking to file location on disk: {ffr}")
  }
  structure(ffrLocal, githubPath = ffr)
}
