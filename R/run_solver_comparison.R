# Comparing solvers performance
#' run_solver_comparison
#' @examples \dontrun{
#' library(ftlverse)
#' SIMS <- runFun()
#' run_solver_comparison(y = SIMS$initial_conditions
#'    , times = SIMS$times
#'    , func = get(paste0(SIMS$modelId, "Ode"))
#'    , parms = SIMS$params)
#' 
#' }
#' @export
run_solver_comparison <- function(...
                                  , rtol = 1e-6
                                  , atol = 1e-6) {
  # deSolve methods
  methods <- c("lsoda", "bdf", "vode", "adams", "rk4", "ode45")
  methods <- c("lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk"
               , "euler", "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d", "adams"
               , "impAdams", "impAdams_d"
               # , "iteration"
  )
  
  method <- methods[1]  # Initialize for debugging
  res <- list()
  for (method in methods) {
    str(method)
    start_time <- Sys.time()
    
    # Standard solver
    sol <- ode(
      ...
      # y = y0,
      # times = times,
      # func = minmod_desolve,
      # parms = params,
      , method = method
      , rtol = rtol, atol = atol)
    
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    res[[method]] <- data.table(
      Solver = paste("deSolve", method),
      RunTime = elapsed
    )
    # str(attributes(sol))
    # if (!method %in% c("rk4", "rk45")) {
    # results$NFunctionEvals = attributes(sol)$istate[3] #nfe
    # }
  }
  results = rbindlist(res, fill = TRUE)
  setkey(results, RunTime)
  # Add r2sundials results if available
  # This would require running the r2sundials code and capturing timing info
  # Plot comparison
  p_compare <- ggplot(solver_comparison, aes(x = Solver, y = RunTime, fill = Solver)) +
    geom_bar(stat = "identity") +
    labs(
      title = "Solver Performance Comparison",
      x = "Solver Method",
      y = "Runtime (seconds)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(results = results, p = p_compare))
}
