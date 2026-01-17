#' grepCode
#' @examples \dontrun{
#'   grepCode("importFrom")
#'   grepCode("vrmdApp")
#'   grepCode("cache")
#'   grepCode("78")
#'   grepCode("APHHOME")
#'   grepCode("FTLHOME")
#'   grepCode("fitList")
#'   grepCode("ReportsVRMD")
#'   
#'   packsSource <- grepCode(rege = "version", ignore.case = TRUE, pack.oi="", patt = "^DESCRIPTION$")
#'   aphVersionsS <- grep("/aph", packsSource, value=TRUE)
#'   
#'   grepCode(rege = "version", ignore.case = TRUE, pack.oi="", patt = "^DESCRIPTION$")
#'   packsInstalled <- grepCode(rege = "version", ignore.case = TRUE, Rdir=.libPaths(), patt = "^DESCRIPTION$")
#'   aphVersions <- grep("/aph", packsInstalled, value=TRUE)
#'   
#'   grepCode(rege = "image", pack.oi="", patt = "^Jenkinsfile$")
#'   #grepCode(rege = "PostgreSQLConnection", pack.oi="aphMetadata", patt="Postgres")
#'   #grepCode(rege = "makeCircadianProcess <-", pack.oi="aphAnalysis")
#'   
#'   grepCode(rege = "Version", Rdir = "/usr/local/lib/R/site-library", patt = "^DESCRIPTION$")
#'   grepCode(rege = "image", Rdir = "/usr/local/lib/R/site-library"
#'            , pack.oi="", patt = "^Jenkinsfile$")
#'   grepCode(rege = "runFun", pack.oi = "", patt = "server")
#'            
#' }
#' @export
grepCode <- function(rege = c("redux::", "<-function\\(")[1]
                     , pack.oi = ""
                     , FTLHOME = file.path("C:/Users", Sys.getenv("USERNAME")
                                           ,"Documents/R")
                     , Rdir =  file.path(FTLHOME, pack.oi)
                     , patt = c("\\.[rR]$", "DESCRIPTION")[1]
                     , ffr.s = list.files(Rdir
                                          , pattern = patt
                                          , recursive = TRUE
                                          , full.names = TRUE)
                     , maxFiles = 10000
                     , noCommented = TRUE
                     , pathBlocker = c("old/")
                     , limit2R = FALSE
                     , ignore.case = TRUE
                     , hideDir = TRUE
                     , spacesTo = " "
){
  ffr.s <- ffr.s[!dir.exists(ffr.s)]
  if (limit2R) {
    ffr.s <- grep("/R/", ffr.s, value=TRUE)
  }
  
  ffr.s <- ffr.s[!is.na(ffr.s)]
  if(!length(ffr.s)) {message("no files found") ; return(character())}
  
  # .ffr.s <<- ffr.s
  x <- ffr.s[1]
  nffr.s <- length(ffr.s)
  .ffr.s <<- ffr.s
  if (nffr.s > maxFiles){
    log_info("scanning only {maxFiles} out of possible {nffr.s} files, see .ffr.s")
  } else {
    log_info("scanning {nffr.s} files, see .ffr.s")
  }
  print(paste("looking for: ", rege))
  
  ffr.sToScan <- ffr.s[seq(min(nffr.s, maxFiles))]
  .ffr.sToScan <<- ffr.sToScan
  wwu <- sapply(ffr.sToScan, function(x) {
    codeLines <- readLines(x, warn = FALSE)
    codeLines <- paste0(seq_along(codeLines), "%@%:", codeLines)
    # .codeLines <<- codeLines
    codeLinesFiltered <- gsub(" ", spacesTo, codeLines, useBytes = T)
    grep(rege[1], codeLinesFiltered, value = TRUE
         # , ignore.case = ignore.case
         , fixed = TRUE
         # , useBytes = T
         )
  }, simplify = FALSE, USE.NAMES = TRUE)
  # .wwu <<- wwu
  ok <- sapply(wwu, \(x) {isTRUE(nchar(as.character(x)) > 0)}
               , USE.NAMES = TRUE, simplify = TRUE)
  ok2 <- sapply(wwu, \(x) {isTRUE(length(x) > 0)}
               , USE.NAMES = TRUE, simplify = TRUE)
  ok <- ok | ok2
  wwu <- wwu[ok]
  # .wwu1 <<- wwu
  wwu <- sapply(wwu, \(x) data.table(res = x)
                , USE.NAMES = TRUE, simplify = F)
  .wwu2 <<- wwu
  if (length(wwu) == 0){
    message("not found")
    return(data.table())
  }
  dwwu <- rbindlist(wwu, idcol = "path")
  
  xxFileInfo <- hfile.info(dwwu$path, basenaming = FALSE)
  xxFileInfo[, modified := as.numeric(modified)]
  xxFileInfo[, accessed := NULL]
  # dwwu2 <- xxFileInfo[dwwu, on = c(file = "path")]
  dwwu <- cbind(dwwu, xxFileInfo)
  if (all(dwwu$path == dwwu$file)){
    dwwu[, (c("path")) := NULL]
  }
  
  if (hideDir){
    dwwu[, file := sub(FTLHOME, "", file, fixed = T)]
    dwwu[, file := sub(Rdir, "", file, fixed = T)]
  }
  dwwu
  dwwu[, (c("line", "res")) := tstrsplit(res, "%@%:")]
  
  dwwu[, commented := grepl("\\s*#", res)+0]
  dwwu
  
  for (bb in pathBlocker) dwwu <- dwwu[!grepl(bb, file)]
  if (noCommented){
    dwwu <- dwwu[commented == 0]
    dwwu[, commented := NULL]
  }
  dwwu <- dwwu[order(modified)]
  dwwu[, modified := hprettyNum(modified)]
  for (regeMore in rege[-1]){
    nn0 <- nrow(dwwu)
    dwwu <- dwwu[grepl(regeMore, res)]
    nn <- nrow(dwwu)
    log_info("reducing further with {regeMore} from {nn0} to {nn} lines" )
  }
  
  .gg <<- copy(dwwu)
  dwwu[]
}


#' fe
#' 
#' @export
fe <- function(row, gg = .gg){
  pre <- "../"
  if (length(grep("miscPackages", getwd(), value = TRUE))) {
    pre <- "../../aphDH/"
  }
  file.edit(paste0(pre, gg[row, file]))
}


#' hfile.info
#' @export
hfile.info <- function(ff, units = "hours", basenaming = TRUE){
  ff <- unlist(ff)
  .ff0 <<- ff
  if (length(ff) > 1){
    res <- lapply(ff, hfile.info, units = units, basenaming = basenaming)
    return(rbindlist(res, fill = TRUE, idcol = "id"))
  }
  info <- file.info(ff)
  ff <- ifelse(basenaming, basename(row.names(info))
               , row.names(info))
  
  res <- list(file = ff
              , size = info$size
              # , created = difftime(Sys.time(), info$ctime, units = units)
              , modified = difftime(Sys.time(), info$mtime, units = units)
              , accessed = difftime(Sys.time(), info$atime, units = units)
              , mtime = info$mtime
  )
  as.data.table(res)
}
