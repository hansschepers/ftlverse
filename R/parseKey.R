#' parseKey
#' @examples \dontrun{
#'   parsedIndexClean <- parseKey(lastIndexPROD, method = "clean")
#'   parsedIndexCleanN <- parsedIndexClean[, .N, by = .(api, file)]
#'   parsedIndexCleanN[N > 1]
#'   parsedIndexCleanN[grepl("nofield", tolower(file))]
#'   parsedIndexCleanN[grepl("nocycle", tolower(api))]
#'   singles <- parsedIndexCleanN[N == 1 & !grepl("nofield", tolower(file)) & !grepl("nocycle", tolower(api))]
#'   timeStamped <- parsedIndexCleanN[grepl("[0-9]{8}_[0-9]{6}", file)]
#' }
#' @param x Key array or lastIndex-DT of S3
#' @param convertDate boolean
#' @author Hans Schepers
#' @export
parseKey <- function(x
                       , convertDate = TRUE
                       , method = c("full", "clean", "cycle")[1]
                       , keepLastModified = TRUE
                       , keepKey = FALSE
                       , doFilter = TRUE
){
  wasDT <- FALSE
  if (is.data.table(x)){
    if ("Key" %in% names(x)){
      wasDT <- TRUE
      DTorig <- copy(x)
      x <- DTorig$Key
    }
  }
  if(method == "full"){
    chk.s <- c("account_id", "api", "mod_id", "year", "mod_id2", "date")
    ref <- paste0("([0-9]{2}.*)/(.*)/(.*)/([12][0-9]{3})/(.*)_([12][0-9]{3}-[01][0-9]-[0123][0-9].*)\\.rds")
    ref
    if (wasDT & doFilter){
      DTorig <- DTorig[grepl(ref, Key)]
      x <- DTorig$Key
    }
    res <- setNames(
      lapply(seq_along(chk.s), function(chk)
        gsub(ref, paste0("\\",chk), x)
      ), chk.s)
    # str(res)
    res$mod_id2 <- NULL
  }
  
  if(method == "clean"){
    chk.s <- c("account_id", "api", "file")
    ref <- paste0("([0-9]{2}.*)/(.*)/([a-zA-Z].*)\\.rds")
    ref
    if (wasDT & doFilter){
      DTorig <- DTorig[grepl(ref, Key)]
      x <- DTorig$Key
    }
    res <- setNames(
      lapply(seq_along(chk.s), function(chk)
        gsub(ref, paste0("\\",chk), x)
      ), chk.s)
  }

  if(method == "cycle"){
    chk.s <- c("account_id", "cycle_syn", "plot_syn")
    ref <- paste0("([0-9]{2}.*)/cycleData/(.*)/(.*)\\.rds")
    ref
    if (wasDT & doFilter){
      DTorig <- DTorig[grepl(ref, Key)]
      x <- DTorig$Key
    }
    res <- setNames(
      lapply(seq_along(chk.s), function(chk)
        gsub(ref, paste0("\\",chk), x)
      ), chk.s)
  }
  
  if(method == "week"){
    chk.s <- c("account_id", "cycle_syn", "plot_syn")
    ref <- paste0("([0-9]{2}.*)/weekdata/cleanData/(.*)/(.*)\\.rds")
    ref
    if (wasDT & doFilter){
      DTorig <- DTorig[grepl(ref, Key)]
      x <- DTorig$Key
    }
    res <- setNames(
      lapply(seq_along(chk.s), function(chk)
        gsub(ref, paste0("\\",chk), x)
      ), chk.s)
  }
  
  res <- data.table::as.data.table(res)
  res
  
  if (convertDate & "date" %in% names(res)) {
    res$date <- as.POSIXct(strptime(res$date, "%Y-%m-%d"))
  }
  if (wasDT) {
    if ("date" %in% names(res) | keepLastModified) {
      res$LastModified <- DTorig$LastModified
    }
    res$Size <- DTorig$Size
  }
  if (keepKey & ! doFilter) {
    res$Key <- DTorig$Key
  }
  res
}
