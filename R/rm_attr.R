#' Remove Attributes
#'
#' Recursively remove attributes from an object.
#'
#' @param object The object to remove the attributes from.
#' @param list_levels `numeric(n)`. The amount of levels to remove the attributes from. Must be >= 0.
#' @param protected `character(n)`. The names of protected attributes that should not be removed.
#'
#' @examples
#' x1 <- structure(1, attrA = 11)
#' rm_attr(x1)
#'
#' x2 <- structure(list(a = 2), attrA = 22)
#' rm_attr(x2)
#'
#' x3 <- structure(list(a = list(b = structure(3, attrB = 33))), attrA = 333)
#' rm_attr(x3)
#'
#' rm_attr(mtcars)
#'
#' x4 <- data.frame(x = 1)
#' attr(x4, "example") <- "to_remove"
#' attributes(x4)
#' x4
#' x4 <- rm_attr(x4)
#' attributes(x4)
#' x4
#'
#' @export
rm_attr <- function(
    object,
    list_levels = Inf,
    protected = c("class", "dim", "names", "dimnames", "row.names", "colnames")
) {
  
  if (!is.numeric(list_levels)) stop("Input list_levels should be a single non-negative integer")
  if (length(list_levels) != 1) stop("Input list_levels should be a single non-negative integer")
  if (list_levels < 0) stop("Input list_levels should be a single non-negative integer")
  if (!is.character(protected)) stop("Input protected should be a character vector")
  
  # Remove unprotected attributes from object
  attr_names <- names(attributes(object))
  
  attributes(object)[setdiff(attr_names, protected)] <- NULL
  
  # Recursively remove unprotected attributes from elements
  if (list_levels > 0 && is.list(object)) {
    object[] <- lapply(object, rm_attr, list_levels = list_levels - 1, protected = protected)
  }
  
  object
}
