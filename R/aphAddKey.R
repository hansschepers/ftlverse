#' aphAddKey
#' @examples \dontrun{
#'   dt <- data.table(a=LETTERS[1:3], b=LETTERS[2:4], c=LETTERS[3:5])
#'   # direct naming
#'   dt[, d := paste(a,b,c, sep="-")]
#'   dt
#'   fois <- c("a", "b", "c")
#'   aphAddKey(dt, fois)
#'   aphAddKey(dt, fois, keyId = "d2", sep = "-")
#'   aphAddKey(dt, fois, keyId = "d3", sep = "-")
#' }
#' @author Hans.Schepers@@gmail.com
#' @export
aphAddKey <- function(dfg
                      , fois
                      , rn = FALSE
                      , sep = "_"
                      , keyId = paste(fois, collapse = sep)
){
  isDT <- inherits(dfg, "data.table")
  missingfois <- setdiff(fois, names(dfg))
  if (length(missingfois)) {
    message("missingfois in haddKey: ", paste(missingfois, collapse="|"))
  }
  fois <- intersect(fois, names(dfg))
  if (!length(fois)) {
    dfg[,keyId] <- "dummy"
  } else {
    e <- paste0(keyId, " := paste(", paste(fois, collapse = ","), ", sep = '",sep,"')")
    dfg[, eval(parse(text=e))]
  }
  if (isDT) {
    return(keyId)
  } else {
    data.table::setDF(dfg)
    if (rn) {
      dfg <- as.data.frame(dfg)
      row.names(dfg) = dfg[, keyId, drop=TRUE]
    }
    return(dfg)
  }
}
