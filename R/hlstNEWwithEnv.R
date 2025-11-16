#' Build Lists with Environment-Based Progressive Evaluation
#'
#' @description
#' Creates a named list by progressively evaluating expressions in a custom 
#' environment, allowing for complex dependencies between variables and 
#' integration with existing function libraries. Particularly useful for 
#' biological and physiological modeling where parameters have intricate 
#' interdependencies.
#'
#' @details
#' This function builds up variables incrementally using an environment-based
#' approach with iterative dependency resolution. Unlike standard list creation,
#' it can handle cases where variables are defined after they are referenced,
#' and can integrate with existing function libraries through the `priorEnv`
#' parameter.
#'
#' The evaluation process:
#' 1. Sets up an evaluation environment that inherits from `priorEnv` and the calling frame
#' 2. Iteratively attempts to evaluate each expression until all dependencies resolve
#' 3. Returns a list containing all successfully evaluated variables
#'
#' Common applications in biological modeling:
#' - Pharmacokinetic parameter specification with complex dependencies
#' - Physiological model parameterization using organ-specific functions
#' - Integration of baseline parameters with experimental modifications
#' - ODE system parameter building with transformation functions
#'
#' @param priorEnv An environment, list, or NULL. If an environment, it becomes
#'   the parent of the evaluation environment. If a list, it's converted to an
#'   environment first. If NULL, only the calling frame is used as parent.
#'   This allows access to predefined functions and variables.
#' @param ... Named expressions to evaluate. Can reference each other and
#'   functions/variables from `priorEnv`. Dependencies are resolved iteratively,
#'   so order of definition doesn't matter for most cases.
#'
#' @return A named list containing all evaluated expressions, ordered by their
#'   first appearance in the argument list.
#'
#' @examples
#' # Basic dependency resolution
#' lstUseEnv(a = 4, b = 5 + c, c = 2, c = 6, d = function(x) {x+11}, e = d(11))
#' # Returns: list(a = 4, b = 11, c = 6, d = function(x){x+11}, e = 22)
#'
#' # Using external function library
#' priorList <- list(f2 = function(x) {-x})
#' lstUseEnv(priorEnv = priorList, b = f2(a), a = 4)
#' # Returns: list(b = -4, a = 4)
#'
#' # Function defined after its use (dependency resolution)
#' lstUseEnv(a = 4, b = 5 + c, c = 2, c = 6, e = d(11), d = function(x) {x+11})
#' # Returns: list(a = 4, b = 11, c = 6, e = 22, d = function(x){x+11})
#'
#' # Pharmacokinetic modeling example
#' baseline_physiology <- list(
#'   liver_volume = 1.5,      # L
#'   kidney_volume = 0.3,     # L  
#'   transform_clearance = function(vol, k) vol * k
#' )
#' 
#' pk_params <- lstUseEnv(
#'   priorEnv = baseline_physiology,
#'   total_clearance = liver_clearance + kidney_clearance,    # L/h
#'   liver_clearance = transform_clearance(liver_volume, k_liver),  # L/h
#'   kidney_clearance = transform_clearance(kidney_volume, k_kidney), # L/h
#'   k_liver = 0.8,          # elimination rate liver (1/h)
#'   k_kidney = 1.2,         # elimination rate kidney (1/h) 
#'   half_life = 0.693 / (total_clearance / volume_dist),  # h
#'   volume_dist = 70        # volume of distribution (L)
#' )
#'
#' # Using environment instead of list
#' penv <- as.environment(priorList)
#' lstUseEnv(priorEnv = penv, b = f2(a), a = 4)
#'
#' @export
lstUseEnv <- function(priorEnv = NULL, ...) {
  # Capture the unevaluated expressions
  call_obj <- match.call()
  args <- as.list(call_obj)[-1]  # Remove function name
  
  # Remove priorEnv from args if present
  if ("priorEnv" %in% names(args)) {
    args <- args[names(args) != "priorEnv"]
  }
  
  arg_names <- names(args)
  
  # Handle unnamed arguments
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  } else {
    arg_names[is.na(arg_names)] <- ""
  }
  
  # Set up evaluation environment with proper parent chain
  if (is.null(priorEnv)) {
    eval_env <- new.env(parent = parent.frame())
  } else if (is.environment(priorEnv)) {
    eval_env <- new.env(parent = priorEnv) 
  } else if (is.list(priorEnv)) {
    # Convert list to environment
    temp_env <- new.env(parent = parent.frame())
    list2env(priorEnv, envir = temp_env)
    eval_env <- new.env(parent = temp_env)
  } else {
    eval_env <- new.env(parent = parent.frame())
  }
  
  # Use iterative evaluation like in hlst to handle dependencies
  max_passes <- length(args) + 2  # Extra passes for complex dependencies
  assigned_vars <- character(0)
  
  for (pass in 1:max_passes) {
    made_progress <- FALSE
    
    for (i in seq_along(args)) {
      name <- arg_names[i]
      if (name != "" && !name %in% assigned_vars) {
        tryCatch({
          # Evaluate expression in the growing environment
          value <- eval(args[[i]], envir = eval_env)
          assign(name, value, envir = eval_env)
          assigned_vars <- c(assigned_vars, name)
          made_progress <- TRUE
        }, error = function(e) {
          # Dependencies not ready yet, skip this iteration
        })
      }
    }
    
    # If no progress was made, we're done (or have circular dependencies)
    if (!made_progress) break
  }
  
  # Return all variables as a list (like mget result)
  var_names <- ls(eval_env, all.names = TRUE)
  result <- mget(var_names, envir = eval_env)
  
  # Maintain original order of unique argument names
  unique_arg_names <- unique(arg_names[arg_names != ""])
  ordered_result <- result[unique_arg_names]
  
  return(ordered_result)
}




#' Build Lists with Enhanced Context Awareness
#'
#' @description
#' Enhanced version of `lstUseEnv` that better handles evaluation contexts,
#' particularly when used within `with()` statements or other scoping constructs.
#' This version maintains better access to the calling environment chain.
#'
#' @details
#' This function extends `lstUseEnv` by improving environment chain management
#' to ensure that variables and functions from the calling context (such as 
#' those provided by `with()`) are properly accessible during evaluation.
#'
#' The key difference from `lstUseEnv` is in how it handles the parent environment
#' chain, ensuring that both `priorEnv` and the calling frame are accessible
#' during expression evaluation. This is particularly important when using
#' `with()` to provide context variables.
#'
#' @inheritParams lstUseEnv
#'
#' @return A named list containing all evaluated expressions, identical to
#'   `lstUseEnv` but with improved context handling.
#'
#' @examples
#' # Works better with with() contexts
#' priorList <- list(f2 = function(x) {-x}, baseline_param = 10)
#' 
#' # Using with() context
#' result <- with(priorList, 
#'   lstUseEnvWithContext(
#'     adjusted_value = f2(baseline_param * factor),
#'     factor = 1.5,
#'     final_calc = adjusted_value + baseline_param
#'   )
#' )
#' # Returns: list(adjusted_value = -15, factor = 1.5, final_calc = -5)
#'
#' # Physiological modeling with context
#' organ_functions <- list(
#'   cardiac_output = function(hr, sv) hr * sv,  # L/min
#'   renal_flow = function(co) co * 0.2,         # 20% of cardiac output
#'   hepatic_flow = function(co) co * 0.25       # 25% of cardiac output  
#' )
#'
#' hemodynamics <- with(organ_functions, 
#'   lstUseEnvWithContext(
#'     total_co = cardiac_output(heart_rate, stroke_volume),     # L/min
#'     kidney_flow = renal_flow(total_co),                      # L/min
#'     liver_flow = hepatic_flow(total_co),                     # L/min
#'     heart_rate = 70,          # beats/min
#'     stroke_volume = 0.07,     # L/beat (70 mL)
#'     perfusion_ratio = kidney_flow / liver_flow               # dimensionless
#'   )
#' )
#'
#' @export
lstUseEnvWithContext <- function(priorEnv = NULL, ...) {
  # Get the calling environment to capture with() context
  calling_env <- parent.frame()
  
  call_obj <- match.call()
  args <- as.list(call_obj)[-1]
  
  if ("priorEnv" %in% names(args)) {
    args <- args[names(args) != "priorEnv"]
  }
  
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  } else {
    arg_names[is.na(arg_names)] <- ""
  }
  
  # Create evaluation environment that can see calling context
  if (is.null(priorEnv)) {
    eval_env <- new.env(parent = calling_env)
  } else if (is.environment(priorEnv)) {
    eval_env <- new.env(parent = priorEnv)
    parent.env(eval_env) <- calling_env  # Fallback to calling env
  } else if (is.list(priorEnv)) {
    temp_env <- new.env(parent = calling_env)
    list2env(priorEnv, envir = temp_env)
    eval_env <- new.env(parent = temp_env)
  } else {
    eval_env <- new.env(parent = calling_env)
  }
  
  # Same iterative evaluation as before
  max_passes <- length(args) + 2
  assigned_vars <- character(0)
  
  for (pass in 1:max_passes) {
    made_progress <- FALSE
    
    for (i in seq_along(args)) {
      name <- arg_names[i]
      if (name != "" && !name %in% assigned_vars) {
        tryCatch({
          value <- eval(args[[i]], envir = eval_env)
          assign(name, value, envir = eval_env)
          assigned_vars <- c(assigned_vars, name)
          made_progress <- TRUE
        }, error = function(e) {
          # Skip if dependencies not ready
        })
      }
    }
    
    if (!made_progress) break
  }
  
  var_names <- ls(eval_env, all.names = TRUE)
  result <- mget(var_names, envir = eval_env)
  unique_arg_names <- unique(arg_names[arg_names != ""])
  ordered_result <- result[unique_arg_names]
  
  return(ordered_result)
}
