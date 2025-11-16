#' simListFilter
#'
#' @export
simListFilter <- function(simsList
                          , focus = c("pb_driv", "pb_crop", "sosi")#[3]
                          , yois = aphKpis(focus)
){
  if (inherits(simsList, "SIMS")){
    # simsList is a single SIMS
    simsList <- list(s1 = simsList)
  }

  simsList |>
    lapply(getElement, "cropLong") |>
    lapply(\(x) x[processName %in% yois]) |>
    rbindlist(idcol = "scenId")
}
