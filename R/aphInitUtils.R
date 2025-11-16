#' Turn list into a data.table
#' @examples \dontrun{
#'   list2df(lst = list(a= 5, b = "b"))
#'   list2df(list())
#' }
#' @param lst list
#' @param empty empty data.table to return if list is of length zero
#' @importFrom data.table as.data.table
#' @export
list2df <- function(lst, V1 = "parameter", empty = data.table()) {
  if (length(lst) > 0) {
    # lst <- as.list(lst)
    dt <- do.call(rbind,
              lapply(names(lst), \(x)
                     data.table(par = x, value = lst[[x]]))
            )
    if ("par" %in% names(dt)) {
      setnames(dt, "par", V1)
    }
    dt
  } else {
    empty
  }
}


assertNotNull <- function(x, name) {
  if (is.null(x)) stop(sprintf("`%s` cannot be NULL", name))
}

assertNChar <- function(x, name, minChar = 1, maxChar = +Inf) {
  if (nchar(x) < minChar)
    stop(sprintf("`%s` needs to contain at least %s characters", name, minChar))
  if (nchar(x) > maxChar)
    stop(sprintf("`%s` can contain at most %s characters", name, maxChar))
}

assertType <- function(x, name, type) {
  if (!typeof(x) == type)
    stop(sprintf("`%s` has to be of type: %s", name, type))
}


nd <- function(...) paste(collapse = "\n", c(...))

withDefault <- function(x, default) if (!is.null(x)) x else default

continued <- function(...) paste(sep = " \\\n\t", collapse = " \\\n\t", ...)

containerImageTag <- function(registry, name, tag) {
  sprintf("%s/%s:%s", registry, name, tag) 
}
