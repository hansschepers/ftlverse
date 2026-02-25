#' diff2title
#' @export
diff2title <- function(parameterDifferences
                       , digits = 3
                       , width = floor(0.9 * getOption("width"))
                       ){
  data.table::setDT(parameterDifferences, keep.rownames=TRUE)
  if (nrow(parameterDifferences) == 0) return("no differences")
  list1 <- as.list(setNames(parameterDifferences[[2]], parameterDifferences[[1]]))
  list2 <- as.list(setNames(parameterDifferences[[3]], parameterDifferences[[1]]))
  
  txt <- paste0(parameterDifferences$parameter
    , " (", hprettyNum(list1, digits = digits)
    , " -> ", hprettyNum(list2, digits = digits)
    , ")")
  
  txt <- paste(txt, collapse = ", ")
  txt <- paste(stringi::stri_wrap(txt, width = width), collapse = "\n")
  # txt <- structure(txt, dt = parameterDifferences)
  return(txt)
}


#' compareParms
#' 
#' @export
compareParms <- function(parmList = list()
                         , keepParsRegex = c("(\\.sens)|(expo)|(Strength)")[0]
                         , keepPars = c("plantingFirstFloweringDelay"
                                        , "fw_max"
                                        , "AFW_ref"
                                        , "temp_ref"
                                        , "RTR"
                                        , "lue_ref"
                                        , "radiation2LED_thr")[0]
){
  if (is.list(keepParsRegex)){
    log_debug("second argument is a list, assuming it is parameters.. ('keepParsRegex' made empty)")
    parmList <- list(p1 = parmList, p2 = keepParsRegex)
    keepParsRegex <- character()
  }
  
  #skip empty list elements
  parmList <- suppressWarnings(sapply(parmList, \(x) sapply(x, as.numeric, simplify = FALSE)
                     , simplify = FALSE))
  lsens <- sapply(parmList, length)
  parmList <- parmList[lsens > 0]
  # str(parmList)
  
  nms <- names(parmList)
  dd <- parmList |>
    lapply(list2df) |>
    rbindlist(idcol = "parm", fill = TRUE)
  res <- dcast(dd, parameter ~ parm)
  naCount <- apply(res[, ..nms], 1, sumna)
  valueRange <- apply(res[, ..nms], 1, hdiffrange)
  
  if (!length(keepParsRegex)) keepParsRegex <- ""
  if (nchar(keepParsRegex) > 0){
    subs <- substitute(
      grepl(x, parameter, ignore.case = TRUE)
      , list(x = keepParsRegex))
    keepPars <- union(keepPars, whichParms(subset = subs))
  }
  res <- res[valueRange > 0 | naCount > 0 | (parameter %in% keepPars),]
  res
}


#' compareVarieties
#' @examples \dontrun{
#'   compareVarieties("breonice", "merlice")
#'   compareVarieties("strabena", "strabini")
#'   compareVarieties("drtc8270", "juanita")
#' }
#' @export
compareVarieties <- function(variety_name
                             , variety_name2
                             , nms = c(variety_name, variety_name2)
                             , ...){
  compareLists(getVarietyParms(variety_name)
               , getVarietyParms(variety_name2)
               , nms = nms
               , ... )
}


#' compareCountries
#' @examples \dontrun{
#'   compareCountries("netherlands", "sweden")
#' }
#' @export
compareCountries <- function(variety_name
                             , variety_name2
                             , nms = c(variety_name, variety_name2)
                             , ...){
  compareLists(countryParms(variety_name)
               , countryParms(variety_name2)
               , nms = nms
               , ... )
}


#' compareNames
#' @examples \dontrun{
#'   compareNames(c(a = 3, b = 4, c = 2), c(b=3, c=4, d=5))
#'   compareNames(c("B", "c"), c("w", "B", NA))
#' }
#' @export
compareNames <- function(..., namesOnly = TRUE){
  compareLists(..., namesOnly = namesOnly)
}


#' compareLists
#' @examples \dontrun{
#'   compareLists(list(a=3, b=4), list(a=8))
#'   compareLists(list(a=3, b=4), list(a=8, b = 4))
#'   modelId <- "sourceSinkModel12"
#'   list1 <- do.call(get(paste0(modelId, "Parms")), list())
#'   list2 <- readVensimParms("sourceSinkModel06")
#'   compareLists(list1, list2)
#'   compareLists(list1, list2, drivers = NULL)
#'   list1_Cherry <- mergeParameters(list1, cherryParms())
#'   compareLists(list1_Cherry, list2)
#'   list1_CherryLit <- mergeParameters(list1_Cherry, winterParms())
#'   compareLists(list1_CherryLit, list2)
#'   compareNames(list1_CherryLit, list2)
#' }
#' @export
compareLists <- function(list1
                         , list2
                         , exclude = c("TIME", "^fix"#, "mask"
                                       , "unitOf", "SAVEPER","\\["
                                       , "verbosity")
                         , skip = c("auto", "none")[1]
                         , drivers = NULL
                         , modelId = "sourceSinkModel12"
                         , maxShow1 = Inf
                         , maxShow2 = maxShow1
                         , digits = 3
                         , namesOnly = FALSE
                         , width = floor(0.9 * getOption("width"))
                         , tol = 1e-5
                         , nms = "auto"
                         , context = "_"
                         , verbosity = log_threshold()
){
  if (namesOnly){
    if (is.null(names(list1)) | !is.list(list1)) list1 <- as.list(setNames(list1, list1))
    if (is.null(names(list2)) | !is.list(list2)) list2 <- as.list(setNames(list2, list2))
    # str(list1)
    # list1 <- unname(unlist(list1))
    # list2 <- unname(unlist(list2))
  }
  if (nms[1] == "auto"){
    nms = c(deparse(substitute(list1)), deparse(substitute(list2)) )
    # nms = c(substitute(list1), substitute(list2) )
  }
  if (is.null(names(list1))) list1 <- setNames(rep(1, length(list1)), unlist(list1))
  if (is.null(names(list2))) list2 <- setNames(rep(1, length(list2)), unlist(list2))
  list1 <- as.list(list1)
  list2 <- as.list(list2)
  # nms <- c(as.name(substitute(list1)), as.name(substitute(list2)) )
  # nms <- c(as.character(substitute(list1)), as.character(substitute(list2)) )
  # nms <- c(as.character(unlist(substitute(list1)))
  #          , as.character(unlist(substitute(list2))) )
  
  if (!"none" %in% skip){
    if (exists("universalConstants", mode = "function")){
      skip <- c(skip, names(universalConstants()))
    }
    if (exists("x01PdeConstants", mode = "function")){
      skip <- c(skip, names(x01PdeConstants()))
    }
  }
  
  if ("drivers" %in% skip){
    drivers <- do.call(get(paste0(modelId, "Drivers")), list())
    skip <- c(skip, names(drivers))
  }
  
  l1names <- setdiff(names(list1), skip)
  l2names <- setdiff(names(list2), skip)
  
  excludeRegex <- paste0("(", paste(exclude, collapse = ")|("), ")")
  exclude1 <- grepl(excludeRegex, l1names)
  l1names <- l1names[!exclude1]
  
  exclude2 <- grepl(excludeRegex, l2names)
  l2names <- l2names[!exclude2]
  
  l1ml2 <- setdiff(l1names, l2names)
  l2ml1 <- setdiff(l2names, l1names)
  uni1 <- l1ml2[seq_len(min(length(l1ml2), maxShow1))]
  uni2 <- l2ml1[seq_len(min(length(l2ml1), maxShow2))]
  if (verbosity > 500){
    # print(l1ml2)
    if (length(l1ml2) > 0){
      message(paste0(context, ": number of unique items in ", nms[1], ": "), length(l1ml2))
      # print(list1[l1ml2])
      # print(uni1)
    }
    if (length(l2ml1) > 0){
      message(paste0(context, ": number of unique items in ", nms[2], ": "), length(l2ml1))
      # print(list2[l2ml1])
      # print(uni2)
    }
  }
  # toUse <- union(l1names, l2names)
  toUse <- intersect(l1names, l2names)
  # print(toUse)
  # print(list1[toUse])
  # print(list2[toUse])
  if (namesOnly){
    return(list(common = toUse
                , unique1 = uni1
                , unique2 = uni2 ))
  }
  toUse <- toUse[!sapply(toUse, \(x) inherits(list1[[x]], "list"))]
  toUse <- toUse[!sapply(toUse, \(x) inherits(list2[[x]], "list"))]
  toUse <- toUse[!sapply(toUse, \(x) inherits(list1[[x]], "data.frame"))]
  toUse <- toUse[!sapply(toUse, \(x) !inherits(list1[[x]], "numeric"))]
  toUse <- toUse[!sapply(toUse, \(x) !inherits(list2[[x]], "numeric"))]
  # ww <- all.equal(list1[toUse], list2[toUse])
  # message("differences")
  # print(ww)
  # str(list1)
  # str(uni2)
  if (length(uni2)) list1[uni2] <- NA
  # str(list2)
  # str(uni1)
  if (length(uni1)) list2[uni1] <- NA
  # different <- abs(unlist(list1[toUse]) != unlist(list2[toUse])
  # print(toUse)
  # .cc1 <<- sapply(list1[toUse], class)
  # .cc2 <<- sapply(list2[toUse], class)
  different <- abs(unlist(list1[toUse]) - unlist(list2[toUse])) > tol
  # str(different)
  # if (verbosity > 400) {
  #   different <- !isFALSE(different)
  # }
  # str(different)
  
  parameterDifferences <- data.frame(list1 = unlist(list1[toUse][different])
                                     ,  list2 = unlist(list2[toUse][different]))
  .parameterDifferences <<- parameterDifferences
  if(!NROW(parameterDifferences)){
    # .parameterDifferences <<- data.table(rn="", currentParms=0, previousParms=0, defaultParms=0)[0]
    return("no changes in numeric values")
  }
  
  # turn parameterDifferences into "title"text string
  .nms <<- nms
  if (nms[1] == nms[2]){
    nms[2] <- paste0(nms[2], "_dummy")
  }
  parameterDifferencesDF <- parameterDifferences
  names(parameterDifferencesDF) <- unlist(lapply(nms, deparse))
  data.table::setDT(parameterDifferencesDF, keep.rownames=TRUE)
  # parameterDifferencesDF[, I := seq(.N)]
  if (verbosity > 400){
    print(parameterDifferencesDF)
  }
  .parameterDifferences <<- parameterDifferencesDF
  # return(.parameterDifferences[])
  
  # if (T){
  data.table::setDT(parameterDifferences, keep.rownames=TRUE)
  txt <- parameterDifferences[, txt := paste0(
    rn
    , " (", hprettyNum(list1, digits = digits)
    , " -> ", hprettyNum(list2, digits = digits)
    , ")")]$txt
  txt <- paste(txt, collapse = ", ")
  txt <- paste(stringi::stri_wrap(txt, width = width), collapse = "\n")
  txt <- structure(txt, dt = parameterDifferencesDF)
  return(txt)
  # }
}
