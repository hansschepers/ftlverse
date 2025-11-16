#' parseAddMult
#'
#' @examples \dontrun{
#'   planParms <- getPlanParms("1024")
#'   planParms
#'   parseAddMult(planParms)
#'   parseAddMult(list(RTR = 4, RTR_add = 2))
#'   parseAddMult(list(RTR = 4, RTR_mult = 2))
#'   parseAddMult()
#'   parseAddMult(ops = c("_mult", "_add"))
#' }
#' @export
parseAddMult <- function(Pars = list(RTR = 2, RTR_add = 1, RTR_mult = 1.1
                                     , fw_max = 12, fw_max_add = 3
                                     , t_mult = 8
                                     , w_add = 99
                                     , q = 8
                                     )
                         , ops = c("_add", "_mult")
                         ){
  for (suff in ops){
    nmsA <- grep(paste0(suff, "$"), names(Pars), value = TRUE)
    if (!length(nmsA)) next
    nms <- unique(sub(paste0(suff, "$"), "", nmsA))
    nms
    if (!length(nms)){
      log_warn("parseAddMult| no names to process for  {ops}")
      return(Pars)
    }

    log_trace("Parameters with {suff}: {nms}")
    ok <- nms %in% names(Pars)
    # if (length(ok) == length(nms)){
    #   log_trace("leaving out {nms[!ok]}")
    # }
    nms <- nms[ok]
    nms
    np <- length(nms)
    Center <- Pars[nms]
    Modif <- Pars[paste0(nms, suff)]

    Modif = rep(0, np)
    ok <- paste0(nms, suff) %in% names(Pars)
    Modif[ok] <- Pars[paste0(nms[ok], suff)]
    Modif

    ii <- 1
    for (ii in seq_along(nms)){
      nm <- nms[ii]
      if (suff == "_add"){
        Pars[[ nms[[ii]] ]] <- Center[[ii]] + Modif[[ii]]
      }
      if (suff == "_mult"){
        Pars[[ nms[[ii]] ]] <- Center[[ii]] * Modif[[ii]]
      }
      Pars[[ paste0(nms[[ii]], suff) ]] <- NULL
    }
  }
  return(Pars)
}
