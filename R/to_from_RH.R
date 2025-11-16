#' vec2mat
#' @examples \dontrun{
#'   dd <- vec2mat(CONSTANTS$TSET)
#'   dd <- vec2mat(CONSTANTS$BFB)
#'   dd
#' }
#' @export
vec2mat <- function(yy = 25:1
                    , nrow = 6
                    , ncol = 4
                    , segment.s = c(head = "hd"
                                    , trunk = "tr"
                                    , arm = "arm"
                                    , hand = "hand"
                                    , leg = "leg"
                                    , feet = "feet")
                    , layer.s = c(core = "core"
                                  , muscle = "muscle"
                                  , fat = "fat"
                                  , skin = "skin")
                    , dimnames = list(segment.s, layer.s)
                    # , pois25 = c("TC", "CC", "TSET", "QB", "EB", "BFB")
){
  
  
  mm <- matrix(yy[1:24], byrow = T
               , nrow = nrow, ncol = ncol
               , dimnames = dimnames)
  dd <- as.data.frame(mm)
  
  if (length(yy) == 25){
    dd["blood", "core"] <- yy[25]
    segment.ois <- c(segment.s, c(blood = "blood"))
  } else {
    segment.ois <- segment.s
  }
  dd <- as.data.table(dd, keep.rownames = T)
  setnames(dd, "rn", "segment")
  dd[, segment := names(segment.ois)]
  dd[]
}


#' toRHmatrices
#' @examples \dontrun{
#'   # makeTables(CONSTANTS, segment.s)
#'   ParsAsMatrix <- toRHmatrices(parList = CONSTANTS)
#'   ParsAsMatrix
#'   ParsAsMatrix$BFB[2, "fat"] <- 3
#'   ParsAsMatrix$TSET[1, core := 1]
#'   ParsAsMatrix$TSET
#'   cat(rh_differences(CONSTANTS, ParsAsMatrix))
#' }
#' @export
toRHmatrices <- function(parList
                         , pois25 = c("TC", "CC", "TSET", "QB", "EB", "BFB")
){
  ParsAsMatrix <- list()
  pois25 <- intersect(pois25, names(parList))
  poi <- pois25[1]
  for (poi in pois25){
    yy <- parList[[poi]]
    dd <- vec2mat(yy)
    ParsAsMatrix[[poi]] <- dd
  }
  .ParsAsMatrix00 <<- ParsAsMatrix
  ParsAsMatrix
}


#' rh_differences
#' @export
rh_differences <- function(CONSTANTS
                           , CONSTANTS_OLD
                           , slotId = "all"
                           , asTable = TRUE
){
  segment.s <- c(head = "hd"
                 , trunk = "tr"
                 , arm = "arm"
                 , hand = "hand"
                 , leg = "leg"
                 , feet = "feet")
  layer.s <- c(core = "core"
               , muscle = "muscle"
               , fat = "fat"
               , skin = "skin")
  item.s <- c(paste(rep(names(segment.s), each = 4)
                    , rep(unname(layer.s), 6))
              , "blood")
  
  # pois25 = c("TC", "CC", "TSET", "QB", "EB", "BFB")
  if ("all" %in% slotId) {
    slotId.s <- intersect(names(CONSTANTS), names(CONSTANTS_OLD))
  } else {
    slotId.s <- slotId
    slotId.s <- intersect(slotId.s, names(CONSTANTS_OLD))
    slotId.s <- intersect(slotId.s, names(CONSTANTS))
  }
  log_debug("slotId.s: {slotId.s}")
  stopifnot(length(slotId.s) > 0)
  
  newList <- CONSTANTS[slotId.s]
  oldList <- CONSTANTS_OLD[slotId.s]
  
  summ <- character()
  ii <- 0
  for (slotId in slotId.s){
    
    newVals <- newList[[slotId]]
    oldVals <- oldList[[slotId]]
    if (inherits(oldVals, "data.frame")){
      stopifnot(all(layer.s %in% names(oldVals)))
      oldVals <- as.numeric(t(as.matrix(oldVals[, ..layer.s])))
    }
    oldVals <- oldVals[!is.na(oldVals)]
    newVals <- newVals[!is.na(newVals)]
    
    log_debug("length of newValues {length(newVals)}")
    log_debug("length of oldValues {length(oldVals)}")
    same <- newVals == oldVals
    
    differences <- which(!same, arr.ind = TRUE)
    
    if (!length(differences)) next
    
    log_debug("{slotId} differences: {differences}")
    # diffs <- differences[1]
    for (diffs in differences){
      ii <- ii + 1
      log_debug("differences {ii} {diffs}")
      summ[ii] <- paste0(slotId, ": ", item.s[diffs], " was changed from "
                         , oldVals[diffs], " to ", newVals[diffs])
    }
    # ii <- ii + 1
    # summ[ii] <- ""
  }
  if (length(summ) == 0){
    summ <- "No parameter changes done"
  }
  if (asTable){
    data.table(changes = summ)
  } else {
    paste(summ, collapse = "\n")
  }
}




#' rh_differences2
#' @examples \dontrun{
#'   li1 <- list(slot1 = list(a1 = 4, b1 = 6, c1 = 9))
#'   li2 <- list(slot1 = list(a1 = 41, b1 = 6, c1 = 91))
#'   rh_differences2(CONSTANTS = li1, CONSTANTS_OLD = li2)
#' }
#' 
#' @export
rh_differences2 <- function(CONSTANTS
                            , CONSTANTS_OLD
                            , slotId = "all"
                            , asTable = TRUE
){
  if ("all" %in% slotId) {
    slotId.s <- intersect(names(CONSTANTS), names(CONSTANTS_OLD))
  } else {
    slotId.s <- slotId
    slotId.s <- intersect(slotId.s, names(CONSTANTS_OLD))
    slotId.s <- intersect(slotId.s, names(CONSTANTS))
  }
  log_debug("slotId.s: {slotId.s}")
  stopifnot(length(slotId.s) > 0)
  
  newList <- CONSTANTS[slotId.s]
  oldList <- CONSTANTS_OLD[slotId.s]
  
  summ <- character()
  ii <- 0
  slotId <- slotId.s[1]
  for (slotId in slotId.s){
    
    newVals <- newList[[slotId]]
    oldVals <- oldList[[slotId]]
    newVals <- unlist(newVals)
    oldVals <- unlist(oldVals)
    oldVals <- oldVals[!is.na(oldVals)]
    newVals <- newVals[!is.na(newVals)]
    
    log_debug("length of newValues {length(newVals)}")
    log_debug("length of oldValues {length(oldVals)}")
    same <- newVals == oldVals
    differences <- which(!same, arr.ind = TRUE)
    if (!length(differences)) next
    
    log_info("{slotId} differences: {differences}")
    item.s <- names(newVals)
    log_info("{slotId} differences: {item.s}")
    
    # diffs <- differences[1]
    for (diffs in differences){
      ii <- ii + 1
      log_debug("differences {ii} {diffs}")
      summ[ii] <- paste0(slotId, ": ", item.s[diffs], " was changed from "
                         , oldVals[diffs], " to ", newVals[diffs])
    }
  }
  
  if (length(summ) == 0){
    summ <- "No parameter changes done"
  }
  if (asTable){
    data.table(changes = summ)
  } else {
    paste(summ, collapse = "\n")
  }
}

