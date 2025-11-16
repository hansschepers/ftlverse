#' allPars25AsRH
#' @examples \dontrun{
#'   allPars25AsRH()
#'   rhList <- allPars25AsRH(pois25 = "TSET")
#'   class(rhList[["TSET"]])
#' }
#' @returns rhandsontable htmlwidget
#' @export
allPars25AsRH <- function(CONSTANTS = cyclist03Constants()
                          , pois25 = "all"
                          , modelId = "cyclist03"
){
  if ("all" %in% pois25){
    pois25 <- c("TC", "CC", "TSET", "QB", "EB", "BFB")
  }
  pois25 <- intersect(pois25, names(CONSTANTS))
  pois25
  stopifnot(length(pois25) > 0)
  
  ParsAsMatrix <- toRHmatrices(CONSTANTS[pois25])
  
  rhList <- list()
  poi <- pois25[1]
  for (poi in pois25){
    dd <- ParsAsMatrix[[poi]]
    # drh <- rhandsontable(dd, rowHeaders = NULL)
    # yy <- unname(unlist(dd[, -1]))
    # fv <- fivenum(unname(yy))
    # tableHtml <- hot_cols(drh
    #                       , renderer = paste0("
    #        function (instance, td, row, col, prop, value, cellProperties) {
    #          Handsontable.renderers.NumericRenderer.apply(this, arguments);
    #          if (col == 0) {
    #            td.style.background = 'lightgray';
    #          } else if (value < ", fv[2],") {
    #           td.style.background = 'pink';
    #          } else if (value > ", fv[4],") {
    #           td.style.background = 'lightgreen';
    #          } else {
    #           td.style.background = 'white';
    #          }
    #         }"))
    rhList[[poi]] <- hrh(dd)
  }
  rhList
}

#' hrh
#' @examples \dontrun{
#'   dd <- data.frame(text = LETTERS[1:3], num1 = 1:3, num2 = 2*3:1)
#'   hrh(dd)
#'   hrh(CONSTANTS$TSET)
#' }
#' @export
hrh <- function(dd){
  # log_warn("hrh| input dd:")
  # str(dd)
  dd <- data.table::as.data.table(dd)
  .dd <<- dd
  # dd <- .dd
  require(rhandsontable)
  drh <- rhandsontable(dd, rowHeaders = NULL)
  isanum <- sapply(dd, is.numeric)
  isanum <- names(isanum)[isanum]
  yy <- unname(unlist(dd[, ..isanum]))
  # yy
  fv <- fivenum(as.numeric(yy))
  hot_cols(drh
           , renderer = paste0("
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col == 0) {
               td.style.background = 'lightgray';
             } else if (value < ", fv[2],") {
              td.style.background = 'pink';
             } else if (value > ", fv[4],") {
              td.style.background = 'lightgreen';
             } else {
              td.style.background = 'white';
             }
            }"))
}


#' hrhs
#' @examples \dontrun{
#'   dd <- data.frame(text = LETTERS[1:3], num1 = 1:3, num2 = 2*3:1)
#'   dd
#'   hrhs(dd)
#' }
#' @export
hrhs <- function(dd){
  # log_warn("hrh| input dd:")
  # str(dd)
  dd <- data.table::as.data.table(dd)
  .dd <<- dd
  # dd <- .dd
  require(rhandsontable)
  drh <- rhandsontable(dd, rowHeaders = NULL)
  # drh
  isanum <- sapply(dd, is.numeric)
  isanum <- names(isanum)[isanum]
  yy <- unname(unlist(dd[, ..isanum]))
  # yy
  fv <- fivenum(as.numeric(yy))
  hot_cols(drh
           , renderer = paste0("
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col == 0) {
               td.style.background = 'lightgray';
             } else if (value < ", fv[2],") {
              td.style.background = 'pink';
             } else if (value > ", fv[4],") {
              td.style.background = 'lightgreen';
             } else {
              td.style.background = 'white';
             }
            }"))
}
