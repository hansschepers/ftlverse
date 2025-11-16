#' getCacheDir
#' 
#' can own local cachedir
#' @examples \dontrun{
#'   getCacheDir()
#'   getCacheDir("DEV")
#'   getCacheDir(cacheType = "winTilde")
#'   getCacheDir(cacheType = "public")
#'   getCacheDir(user = "tdoes1", dircrea = FALSE)
#' }
#' @return [string]
#' @export
getCacheDir <- function(ext = ""
                        , cacheType = c("winLocal", "winTilde")[1]
                        , user = tolower(tag)
                        , tag = Sys.getenv("USERNAME")
                        , dircrea = TRUE
){
  if(F){
    # u1 <- list.files(file.path("C:", "Users", tolower(tag)))
    # u2 <- list.files(file.path("C:", "Users", tolower(tag), "Documents"))
    # 
    # cacheDir <- ifelse(.Platform$OS.type == "windows"
    #                    , switch(cacheType,
    #                             winLocal = file.path("C:", "Users", user, "Documents", "_ftlCache")
    #                             , winTilde = file.path("~", "_ftlCache")
    #                             , public = "."
    #                             , social = "."
    #                    )
    #                    , tempdir()  # FIXME
    # )
    # cacheDir <- paste0(cacheDir, toupper(ext))
  } else {
    cacheDir <- "ReportsVRMD"
  }
  # print(sys.frames())
  # stop()
  
  if (dircrea){
    if (!dir.exists(cacheDir)){
      message("creating caching directory!: ", cacheDir)
      dir.create(cacheDir, recursive = TRUE)
    }
  }
  log_debug(" ******************** cacheDIR: {cacheDir}")
  cacheDir
}


# , gsub("~", Sys.getenv("R_USER"), "~/_ftlCache")
