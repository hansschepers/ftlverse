#' makeFormula
#' 
#' @examples \dontrun{
#'   castFormu <- makeFormula(c("accountId", "differentiator", "doyhr"), "variable")
#'   makeFormula(lhs = c("A", "B"), rhs = c("C", "D"))
#' }
#' @param lhs `character()`. The LHS of the formula.
#' @param rhs `character()`. The RHS of the formula.
#' @param swap `logical(1)`. swaps lhs and rhs
#' @export
makeFormula <- function(lhs, rhs, swap = FALSE, intercept = ""){
  lhs <- unique(lhs)
  rhs <- unique(rhs)
  if (swap){
    aa <- rhs
    rhs <- lhs
    lhs <- aa
  }
  lhs <- setdiff(lhs, rhs)
  hasSpace <- grepl(" ", lhs)
  lhs[hasSpace] <- paste0("`", lhs[hasSpace], "`")
  hasSpace <- grepl(" ", rhs)
  rhs[hasSpace] <- paste0("`", rhs[hasSpace], "`")
  lhs <- paste(lhs, collapse = " + ")
  rhs <- paste(rhs, collapse = " + ")
  intercept
  paste(lhs, "~", rhs, intercept)
}


#' formu2terms
#' @examples \dontrun{
#'   inst/example/e_formula.R
#' }
#' @export
formu2terms <- function(formu){
  tt <- terms.formula(as.formula(formu))
  # attributes(tt)
  # class(tt)
  # .class2(tt)
  rhs2 <- sapply(strsplit(as.character(tt)[3], "\\+")[[1]], trimws, USE.NAMES = F)
  lhs2 <- sapply(strsplit(as.character(tt)[2], "\\+")[[1]], trimws, USE.NAMES = F)
  list(lhs = lhs2, rhs = rhs2)
}


#' makeNlsFormula
#' 
#' @examples \dontrun{
#'   # linear model
#'   makeNlsFormula(lhs = "A", rhs = c("C", "D"))
#'   makeNlsFormula(lhs = "A", rhs = c("C", "D"), intercept = "")
#'   # mixed model:
#'   makeNlsFormula(lhs = "A", rhs = c("C", "D"), coefNames = c("p_C[variety]", "p_D"))
#' }
#' @param lhs `character()`. The LHS of the formula.
#' @param rhs `character()`. The RHS of the formula.
#' @export
makeNlsFormula <- function(lhs
                           , rhs
                           , intercept = "icpt"
                           , coefNames = paste0("p_", unique(rhs))
                           ){
  lhs <- unique(lhs)
  stopifnot(length(lhs) == 1)
  rhs <- unique(rhs)
  lhs <- setdiff(lhs, rhs)
  rhs <- paste(paste(coefNames, rhs, sep = " * "), collapse = " + ")
  if (nchar(intercept) > 0){
    rhs <- paste0(intercept, "  +  ", rhs)
  }
  # rhs <- paste(rhs, collapse = " + ")
  paste(lhs, "~", rhs)
}

