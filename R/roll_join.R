#' roll_join
#' @examples \dontrun{
#'   inst/example/e_roll_join.R
#' }
#' @export
roll_join <- function(dt1     # e.g. csd data
                      , dt2   # e.g. DTN drivers
                      , doi = c("local_time", "x")[1]
                      , doi_by = c("1 day")[0]
                      , on = doi
                      , interpolate = TRUE
                      , rule = 1
                      , leftJoin = TRUE
                      , doMelt = FALSE
                      , nms = c("d1", "d2")){
  # print(aphTimes(dt1))
  # print(aphTimes(dt2))
  .dt1 <<- copy(dt1)
  .dt2 <<- copy(dt2)
  # dt1 <<- copy(.dt1)
  # dt2 <<- copy(.dt2)
  stopifnot(doi %in% names(dt1))
  stopifnot(doi %in% names(dt2))
  d2levels <- unique(dt2[, ..doi])

  log_debug("  *********************************** roll_join ***************")

  if (length(doi_by)){
    # dt1[, dateTime := NULL]
    # dt1[, cropseason_id := NULL]
    # dt1[, local_time := NULL]
    # aphKey(dt1)
    # aphKey(dt2)
    dt1a <- changeTimeResolution(dt1, doi = doi, doi_by = doi_by, on = on
                                 , interpolate = interpolate, rule = rule, doplot = F)
    dt2a <- changeTimeResolution(dt2, doi = doi, doi_by = doi_by, on = on
                                 , interpolate = interpolate, rule = rule, doplot = F)
    # dt1a[, ..doi]
    # dt1a[, outside_temp]
    if (doMelt){
      dt1a <- aphMelt(dt1a, value.name = nms[1])
      dt2a <- aphMelt(dt2a, value.name = nms[2])
      on <- union(on, "processName")
    }
    res <- dt1a[dt2a, on = on]
  } else {
    if (doMelt){
      dt1 <- aphMelt(dt1, value.name = nms[1])
      dt2 <- aphMelt(dt2, value.name = nms[2])
      on <- union(on, "processName")
    }
    res <- dt1[dt2, on = on]
  }
  # head(dt1a, 11)
  # compareNames(dt1a, dt2a)
  aphKey(res)

  if (leftJoin) {
    res <- res[get(doi) %in% d2levels[, get(doi)]]
  }
  # print(key(res))
  setkeyv(res, doi)
  res[]
}
