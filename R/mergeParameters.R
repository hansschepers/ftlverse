#' none
#' @export
none <- function(x, ...) {
  x
}

#' mergeLists
#' @seealso mergeParameters
#' @export
mergeLists <- function(todo
                       , todo2add = list("user" = "hans")
                       , force = FALSE){
  if (is.null(todo)) todo <- list()
  todo2add <- as.list(todo2add)
  for (nn in names(todo2add)){
    # print(todo2add[[nn]])
    if (force | (!nn %in% names(todo))) {
      todo[[nn]] <- todo2add[[nn]]
    }
  }
  todo
}


#' mergeParameters
#' @examples \dontrun{
#'   mergeParameters(list(a = 1, b = 2, d = list(e = 1, g = 11)), list(b = 22, c = 33, d = list(e = 9)), 0)
#' }
#' 
#' @export
mergeParameters <- function(p0, p1, enrichFUN = NULL
                            , verbosity = log_threshold()
){
  p0 <- as.list(p0)
  p1 <- as.list(p1)
  if (verbosity >= 500){
    nms <- setdiff(intersect(names(p0), names(p1)), c(NA))
    sapply(nms, \(x) {
      new <- p1[[x]]
      old <- p0[[x]]
      print(glue::glue("replacing parameter {nms} from {old} to {new}"))
    })
  }
  if (verbosity >= 500){
    nms <- setdiff(names(p1), union(names(p0), c(NA)))
    sapply(nms, \(x) {
      new <- p1[x]
      vv <- dput(new)
      print(glue::glue("adding parameter {nms} to {vv}"))
    })
  }
  # p0[nms] <- p1[nms]
  # p0
  p <- modifyList(p0, p1)
  if (!is.null(enrichFUN)){
    if (is.character(enrichFUN)) {
      if (!exists(enrichFUN, mode = "function")){
        warn("mergeParameter: enrichFUN ", enrichFUN, " not found!")
      } else {
        enrichFUN <- get(enrichFUN, mode = "function")
      }
    }
    p <- enrichFUN(p)
  }
  p
}


#' editParameters
#' @export
editParameters <- mergeParameters

