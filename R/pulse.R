# ============================================================================
# Helper Functions
# ============================================================================

# Pulse function for discrete events (dimensionless)
#' pulse
#' @examples
#' \dontrun{
#'   sapply(0:40, pulse, value = 2, first = 10, interval = 6)
#' }
#' 
pulse <- function(time_val, value, first, interval) {
  if (time_val < first) return(0)
  cycle_time <- (time_val - first) %% interval
  if (cycle_time < 1) return(value)
  return(0)
}
