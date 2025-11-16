#' pars2dt
#' @examples \dontrun{
#'   pars2dt(pars = .parSet$field)
#' }
#' 
#' @export
pars2dt <- function(pars = list(RTR = 2.5)){
  # if (shiny::is.reactivevalues(pars)){
  #   pars <- shiny::reactiveValuesToList(pars)
  # }
  data.table(parameter = names(pars), value = unname(unlist(pars)))
}