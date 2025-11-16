#' Handle Lists with Duplicate Names and Expression Dependencies
#'
#' @description
#' Creates a named list from expressions that may contain duplicate argument names,
#' with configurable precedence rules for variable resolution. Particularly useful
#' for parameter specification in mathematical models where you want to test
#' different parameter precedence scenarios (e.g., default vs. override values).
#'
#' @details
#' This function allows duplicate argument names and resolves variable references
#' in expressions based on the specified order. When `order = "normal"` (default),
#' the first occurrence of each variable name is used for evaluation, matching
#' R's standard list behavior where `list(x = 1, x = 2)$x` returns 1.
#' When `order = "reverse"`, the last occurrence is used.
#'
#' The function uses iterative evaluation to handle dependencies between variables,
#' making multiple passes through the expressions until all dependencies are resolved
#' or no further progress can be made.
#'
#' Common use cases in biological modeling include:
#' - Testing parameter precedence in ODE systems
#' - Combining default physiological parameters with experimental overrides
#' - Sensitivity analysis with different parameter resolution strategies
#'
#' @param ... Named and unnamed expressions. Expressions can reference other
#'   variables defined in the same call. Duplicate names are allowed and handled
#'   according to the `order` parameter.
#' @param order Character string specifying precedence rule for duplicate names.
#'   Either `"normal"` (first occurrence wins, default) or `"reverse"` 
#'   (last occurrence wins).
#'
#' @return A named list containing the evaluated expressions. Each unique variable
#'   name appears only once in the output, with the value determined by the
#'   precedence rule. The order of elements follows the first appearance of
#'   each unique name in the input.
#'
#' @examples
#' # Basic usage with duplicate names
#' hlst(b = 5 + a, a = 4, a = 2, order = "normal")
#' # Returns: list(b = 9, a = 4)  # uses first 'a'
#'
#' hlst(b = 5 + a, a = 4, a = 2, order = "reverse") 
#' # Returns: list(b = 7, a = 2)  # uses last 'a'
#'
#' # Default behavior (normal order)
#' hlst(x = a + b, a = 10, b = 3, a = 1)
#' # Returns: list(x = 13, a = 10, b = 3)
#'
#' # Complex dependencies
#' hlst(clearance_rate = k_el * volume_dist, 
#'      k_el = 0.5,           # elimination rate (1/h)
#'      volume_dist = 70,     # volume of distribution (L)
#'      k_el = 0.3,           # alternative elimination rate
#'      order = "normal")
#' # hslt0 does not remove duplicate entries (and )
#' hlst0(clearance_rate = k_el * volume_dist, 
#'      k_el = 0.5,           # elimination rate (1/h)
#'      volume_dist = 70,     # volume of distribution (L)
#'      k_el = 0.3,           # alternative elimination rate
#'      order = "normal")
#' # Returns: list(clearance_rate = 35, k_el = 0.5, volume_dist = 70)
#'
#' @export
hlst <- function(..., order = "normal") {
  # Capture the unevaluated expressions
  call_obj <- match.call()
  args <- as.list(call_obj)[-1]  # Remove function name
  
  # Remove the 'order' parameter from args if present
  if ("order" %in% names(args)) {
    args <- args[names(args) != "order"]
  }
  
  arg_names <- names(args)
  
  # Handle unnamed arguments
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  } else {
    arg_names[is.na(arg_names)] <- ""
  }
  
  # Build evaluation environment based on order parameter
  eval_env <- new.env(parent = parent.frame())
  
  if (order == "normal") {
    # Use FIRST occurrence rule (standard R behavior)
    seen_names <- character(0)
    
    # Multiple passes to handle dependencies
    max_passes <- length(args)
    for (pass in 1:max_passes) {
      made_progress <- FALSE
      
      for (i in seq_along(args)) {
        name <- arg_names[i]
        if (name != "" && !name %in% seen_names) {
          # Try to evaluate this expression
          tryCatch({
            eval_env[[name]] <- eval(args[[i]], envir = eval_env)
            seen_names <- c(seen_names, name)
            made_progress <- TRUE
          }, error = function(e) {
            # Can't evaluate yet - dependencies not ready
          })
        }
      }
      
      # If no progress was made, we're done
      if (!made_progress) break
    }
    
  } else if (order == "reverse") {
    # Use LAST occurrence rule
    # Collect all variable names and their LAST expressions
    var_exprs <- list()
    for (i in seq_along(args)) {
      name <- arg_names[i]
      if (name != "") {
        var_exprs[[name]] <- args[[i]]  # Overwrite with last occurrence
      }
    }
    
    # Multiple passes to evaluate variables
    max_passes <- length(var_exprs)
    for (pass in 1:max_passes) {
      made_progress <- FALSE
      
      for (name in names(var_exprs)) {
        if (!exists(name, envir = eval_env, inherits = FALSE)) {
          tryCatch({
            eval_env[[name]] <- eval(var_exprs[[name]], envir = eval_env)
            made_progress <- TRUE
          }, error = function(e) {
            # Can't evaluate yet - dependencies not ready
          })
        }
      }
      
      if (!made_progress) break
    }
    
  } else {
    stop("order must be either 'normal' or 'reverse'")
  }
  
  # Create result list with unique variable names only
  # Get unique names in order of first appearance
  unique_names <- unique(arg_names[arg_names != ""])
  
  result <- list()
  for (name in unique_names) {
    result[[name]] <- eval_env[[name]]
  }
  
  return(result)
}

# Function to handle duplicate named arguments with configurable evaluation order
# if(F) {
#   hlst(b = 5 + a + x, a = 4, a = 2, order = "normal")
#   ww <- hlst(b = 5 + a + x, a = 4)
# 
#   parameters <- list(x = 10)
#   with(parameters, hlst(b = 5 + a + x, a = 4, a = 2, order = "normal")) # should give list(b = 19, a = 4)
#   with(parameters, hlst(b = 5 + a + x, a = 4, a = 2, order = "reverse")) # should give list(b = 19, a = 4)
# 
#   ww <- substitute(hlst(b = 5 + a + x, a = 4))
#   with(parameters, eval(ww))
# 
#   hlst(b = 5 + a, a = 4, a = 2, order = "normal")   # list(b = 9, a = 4)
#   hlst(b = 5 + a, a = 4, a = 2, order = "reverse")  # list(b = 7, a = 2)
#   hlst(b = 5 + a, a = 4, a = 2)                     # list(b = 9, a = 4) [default]
# }

