#' list2title
#' @examples \dontrun{
#'   modelId
#'   Drivers <- do.call(get(paste0(modelId, "Drivers")), list())
#'   Params <- do.call(get(paste0(modelId, "Parms")), list())
#'   cat(list2title(x=Params, grouping = 6))
#'   cat(list2title(Params, skipMore = names(Drivers), grouping = 6))
#'   cat(list2title(Params, skipMore = names(Drivers)))
#' }
#' 
#' @export
list2title <- function(x
                       , collap = ", "
                       , skip = c(names(universalConstants()))
                       , skipMore = character(0)
                       , grouping = 8) {
  x <- sapply(x, format)
  x <- unlist(x)
  nn <- length(x)
  x <- x[setdiff(names(x), c(skip, skipMore))]
  nn <- length(x)
  suppressWarnings({
    gg <- as.list(as.data.table(matrix(seq(nn)
                                       , ncol = max(1, ceiling(nn / (grouping + .01) )))))
  })
  # print(gg)
  ind <- 1
  kk <- lapply(gg, function(ind) {
    paste0(paste(names(x[ind])
                 , hprettyNum(x[ind]), sep = " = ")
           , collapse = collap)
  }
  )
  paste(kk, collapse = "\n")
}
