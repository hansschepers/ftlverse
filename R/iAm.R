#' iAm
#' @export
iAm <- function(where = c("home", "office", "cloud")[1]
){
  if (tolower(Sys.getenv("USERNAME")) != "hhsche2"){
    message("this function was made for HHSCHE2 to manage proxy & git-config settings.")
  }
  if (where == "cloud"){
    Sys.setenv(USEPROXY = 0)
    Sys.setenv(WHEREAMI = "cloud")
  }
  
  if (where == "home"){
    Sys.setenv(USEPROXY = 0)
    Sys.setenv(WHEREAMI = "home")
    file.copy(paste0("C:/Users/", Sys.getenv("USERNAME"), "/.ssh/config")
              , paste0("C:/Users/", Sys.getenv("USERNAME"), "/.ssh/configINACTIVE")
              , overwrite = TRUE)
    ff <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/.ssh/config")
    if (file.exists(ff)) file.remove(ff)
  }
  
  if (where == "office"){
    Sys.setenv(USEPROXY = 1)
    Sys.setenv(WHEREAMI = "office")
    
    file.copy(paste0("C:/Users/", Sys.getenv("USERNAME"), "/.ssh/configINACTIVE")
              , paste0("C:/Users/", Sys.getenv("USERNAME"), "/.ssh/config")
              , overwrite = TRUE)
    # file.remove("C:/Users/", Sys.getenv("USERNAME"), "/.ssh/configINACTIVE")
  }
  UseProxy <<- as.numeric(Sys.getenv("USEPROXY", "0"))
  useProxy <<- UseProxy
  useProxy
}