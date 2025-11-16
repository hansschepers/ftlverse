#' copyLibraryToS3
#' 
#' @export
copyLibraryToS3 <- function(pack.oi = "aphPredict"
                            , Rdir = file.path("C:/Users", Sys.getenv("USERNAME")
                                               ,"Documents/aphDH", pack.oi, "R")
                            , ffr.s = list.files(Rdir, pattern=".R", full.names = FALSE)
                            , configList = list(bucket="aph-data", prefix = "hhsche2")
                            , verbosity = 1){
  
  ffzipLocal <- tempfile(pattern=paste0("TMP_PACK",pack.oi), fileext=".zip")
  # system(paste0("jar -cMf ", ffzipLocal, " ", Rdir))
  owd <- setwd(Rdir)
  zip(zipfile=ffzipLocal, files=ffr.s) # [1:2]
  file.info(ffzipLocal)
  
  ffzipS3 <- paste0("TMP_PACK", pack.oi, ".zip")
  
  if(verbosity > 0) {
    message("copying file: ", ffzipLocal)
    message("to: S3://", file.path(configList$bucket
                                   , configList$prefix, "Rsource"
                                   , pack.oi, ffzipS3))
  }
  aws.s3::put_object(file = ffzipLocal
                     , bucket = configList$bucket
                     , object = file.path(configList$prefix, "Rsource"
                                          , pack.oi, ffzipS3)
                     , overwrite = TRUE)
  setwd(owd)
}
