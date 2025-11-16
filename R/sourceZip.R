#' sourceZip
#' 
#' @importFrom aws.s3 save_object
#' @export
sourceZip <- function(pack.oi = "aphPredict"
                      , configList = list(bucket="aph-data", prefix = "hhsche2")
){
  
  ffzipJustRead <- paste0("TMP_PACK", pack.oi, ".zip")
  
  ffzipS3 <- file.path(configList$prefix, "Rsource", pack.oi, ffzipJustRead)
  
  if(verbosity > 0) {
    message("creating file: ", ffzipJustRead)
    message("from: S3://", file.path(configList$bucket
                                     , configList$prefix, "Rsource"
                                     , pack.oi, ffzipS3))
  }
  
  aws.s3::save_object(
    bucket = configList$bucket
    , object = ffzipS3
    , file = ffzipJustRead
  )
  
  tmpDir <- tempdir()
  unpackDir <- file.path(tmpDir, "tmpUnzip")
  dir.create(unpackDir)
  
  unzip(ffzipJustRead, exdir=unpackDir)
  unPackffr.s <- list.files(unpackDir, full.names = TRUE)
  sourceLibrary(ffr.s = unPackffr.s)
}
