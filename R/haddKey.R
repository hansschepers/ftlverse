# added by autoDocument, row.1 | Tue Mar 28 16:32:46 2017
#' haddKey
#' @examples \dontrun{
#'   dt <- data.table(a=LETTERS[1:3], b=LETTERS[2:4], c=LETTERS[3:5])
#'   # direct naming
#'   dt[, d := paste(a,b,c, sep="-")]
#'   dt
#'   fois <- c("a", "b", "c")
#'   haddKey(dt, fois)
#'   haddKey(dt, fois, keyID = "d2", sep = "-")
#'   ww <- haddKey(as.data.frame(dt), fois, keyID = "e2", sep = "-")
#'   str(ww)
#'   str(dfg)
#'   # aphAddKey(dt, fois, keyID = "d3", sep = "-")
#' }
#' @author Hans.Schepers@@gmail.com
#' @export
haddKey <- function(dfg
                    , fois
                    , rn = FALSE
                    , sep = "_"
                    , keyID = paste(fois, collapse = sep)
                    , remove = (keyID %in% fois)){
  isDT <- inherits(dfg, "data.table")
  dfg <- as.data.frame(dfg)
  # .Deprecated(aphAddKey)
  if(keyID %in% names(dfg)) if (!keyID %in% fois) dfg[,keyID] = NULL
  missingfois <- setdiff(fois, names(dfg))
  if (length(missingfois)) message("missingfois in haddKey: ", paste(missingfois, collapse="|"))
  fois <- intersect(fois, names(dfg))
  if (!length(fois)) {
    dfg[,keyID] <- "dummy"
  } else {
    dfg <- as.data.table(dfg)
    e <- paste0(keyID, " := paste(", paste(fois, collapse = ","), ", sep = '",sep,"')")
    dfg[, eval(parse(text=e))]
    dfg <- as.data.frame(dfg)
    if (rn) row.names(dfg) = as.data.frame(dfg)[,keyID, drop=TRUE]
  }
  if (isDT) {
    data.table::setDT(dfg)
  }
  return(dfg)
}
