#' wrapCamel
#' @examples \dontrun{
#'    x <- c("aaaBBBCaaC_K.", "Ave rageFruitWeight_hanging.afterHarvest")
#'    (x2 <- wrapCamel(x = x))
#'    (x2 <- wrapCamel(x = x, space = T))
#'    cat(x2[1])
#'    cat(x2[2])
#'    wrapCamel(x, sep = " ")
#' }
#' 
#' @export
wrapCamel <- function(x
                      , regex = "(\\.)|(_)|(?<=[a-z])(?=[A-Z])"
                      , space = F
                      , sep = "\n") {
  if (space){
    regex <- paste0("( )|", regex)
  }
  x <- as.character(x)
  unlist(lapply(strsplit(x, regex, perl = TRUE)
                , paste, collapse = sep))
}


#' spaceCamel
#' @examples \dontrun{
#'    x <- c("aaaBBBCaaC_K.", "AverageFruitWeight_hanging.afterHarvest")
#'    (x2 <- wrapCamel(x = x))
#'    cat(x2[1])
#'    cat(x2[2])
#'    spaceCamel(x, sep = " ")
#' }
#' 
#' @export
spaceCamel <- function(x, sep = " ", ...) {
  wrapCamel(x, sep = sep, ...)
}
