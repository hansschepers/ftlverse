hsource1 <- function(file
                     , dir = Sys.getenv(dirId, ".")
                     , dirId = c("DEV_CYCLISTPATH", "APP_CYCLISTPATH")[1]
                     , subdir = "src"
                     , ...){
  source(file.path(dir, subdir, file), ...)
}


# prep_appLaunch( devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
#                 , appDir = Sys.getenv("APP_CYCLISTPATH", ".")
#                 , overwrite = FALSE)
#' prep_appLaunch
#' @examples \dontrun{
#'   prep_appLaunch()
#' }
#' 
#' @export
prep_appLaunch <- function(devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
                           , appDir = Sys.getenv("APP_CYCLISTPATH", ".")
                           , overwrite = TRUE){
  srcdir <- file.path(devDir, "src")
  stopifnot(dir.exists(srcdir))
  ffr.s <- list.files(srcdir, pattern = "\\.R$", full.names = FALSE)
  message("copying those files after 5 secs...")
  .ffr.sPREP <<- ffr.s
  
  print(file.path(srcdir, ffr.s))
  Sys.sleep(5)
  for (ffr in ffr.s){
    devFfr <- file.path(devDir, "src", ffr)
    appFfr <- file.path(appDir, "src", ffr)
    if (file.exists(devFfr)){
      message("copying ", devFfr)
      print(file.copy(devFfr, appFfr, overwrite = overwrite))
    }
  }
  print(file.copy(file.path(devDir, "www/dc/attributedTo.md")
                  , file.path(appDir, "www/dc/attributedTo.md")
                  , overwrite = overwrite))
}


mask_old_dir <- function(devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
                         , overwrite = TRUE){
  olddir <- file.path(devDir, "old")
  stopifnot(dir.exists(olddir))
  ffr.s <- list.files(olddir, pattern = "\\.R$", full.names = FALSE)
  message("changing extention of those files")
  print(file.path(olddir, ffr.s))
  print(sub("\\.R$", ".RZ", file.path(olddir, ffr.s)))
  for (ffr in ffr.s){
    devFfr <- file.path(devDir, "old", ffr)
    tmpFfr <- sub("\\.R$", ".RZ", devFfr)
    if (file.exists(devFfr)){
      log_info("renaming  {devFfr} to {tmpFfr}")
      file.rename(devFfr, tmpFfr)
    }
  }
}


unmask_old_dir <- function(devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
                         , overwrite = TRUE){
  olddir <- file.path(devDir, "old")
  stopifnot(dir.exists(olddir))
  ffr.s <- list.files(olddir, pattern = "\\.RZ$", full.names = FALSE)
  message("changing extention of those files")
  print(file.path(olddir, ffr.s))
  print(sub("\\.RZ$", ".Z", file.path(olddir, ffr.s)))
  for (ffr in ffr.s){
    devFfr <- file.path(devDir, "old", ffr)
    tmpFfr <- sub("\\.RZ$", ".R", devFfr)
    if (file.exists(devFfr)){
      log_info("renaming  {devFfr} to {tmpFfr}")
      file.rename(devFfr, tmpFfr)
    }
  }
}
