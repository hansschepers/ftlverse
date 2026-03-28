#' compareSIMS
#' @examples \dontrun{
#'   SIMS <- runFun()
#'   SIMS2 <- runFun(variety_name = "marinice")
#'   SIMSdiffs <- compareSIMS(SIMS, SIMS2)
#' }
#' @export
# compareSIMS <- function(SIMS0
#                         , SIMS
#                         , skip = c("toReturn"
#                                    , "extraSpecs"
#                                    , "sessionI", "store", "packageVersion.s"
#                                    , "parameterComparison")
#                         , width = 120#floor(0.9 * getOption("width"))
#                         , nms = c(substitute(SIMS0), substitute(SIMS) )
#                         , verbosity = 0
#                         ){
#   drvdiff <- compareNames(SIMS0$driverList, SIMS$driverList)
#   print(drvdiff)
#   pardiff <- compareParms(SIMS0$parms, SIMS$parms, keepPars = "")
#   res <- list(diff_title = diff2title(pardiff))
#   res
# }


# if(F){
#   nms <- as.character(nms)
#   res <- list()
#   SIMS_elements_to_scan <- setdiff(intersect(names(SIMS00), names(SIMS))
#                                    , c("locationWeather", "argListStates"))
#   for (nm in SIMS_elements_to_scan){
#     if (nm %in% skip){
#       next
#     }
#     log_trace("compareSIMS| nm {nm}")
#     if (nm == "packageVersion.s"){
#       res[[nm]] <- mapply(compareNames, SIMS00[[nm]], SIMS[[nm]])
#       # res[[nm]] <- as.data.table(res[[nm]])
#     } else {
#       if (nm == "store"){
#         
#         res[[nm]] <- mapply(compareLists, SIMS00[[nm]], SIMS[[nm]])
#         
#       } else {  
#         
#         if (inherits(SIMS00[[nm]], "list")){
#           log_trace("list nm {nm}")
#           # print(length(SIMS00[[nm]]))
#           # print(length(SIMS[[nm]]))
#           parmList <- list(x1 = SIMS00[[nm]], x2 = SIMS[[nm]])
#           names(parmList) <- nms
#           dd <- compareLists(SIMS00[[nm]], SIMS[[nm]]
#                              , nms = nms
#                              , width = width
#                              , context = nm
#                              , verbosity = verbosity)
#           if (dd[1] != "no changes in numeric values") res[[nm]] <- dd
#           
#         } else {
#           
#           if (length(SIMS00[[nm]]) > 1){
#             # vector
#             if (inherits(SIMS00[[nm]], "character")){
#               dd <- compareNames(SIMS00[[nm]], SIMS[[nm]])
#               if (!length(dd$common)) dd$common <- NULL
#               .dd <<- dd
#               if (length(dd$unique1) + length(dd$unique2) == 0) {
#                 dd <- "no diffs"
#               } else {
#                 res[[nm]] <- unlist(dd)
#               }
#             }
#             
#           } else {
#             
#             log_trace("vector nm {nm}")
#             dd <- compareLists(SIMS00[[nm]], SIMS[[nm]]
#                                , nms = nms
#                                , width = width
#                                , context = nm
#                                , verbosity = verbosity)
#             if (dd[1] != "no changes in numeric values") res[[nm]] <- dd
#             # res[[nm]] <- unlist(dd)
#           }
#         }
#       }
#     }
#   }
#   .comp <<- res
#   if ("usedParms" %in% names(res)){
#     res$summaryDiff <- hprettyNum(attr(res$usedParms, "dt"))
#   }
#   
#   if ("fitFileParms" %in% names(res)){
#     res$summaryDiff_fit <- attr(res$fitFileParms, "dt")
#   }
#   
#   if ("locParms" %in% names(res)){
#     res$summaryDiff_locParms <- attr(res$locParms, "dt")
#   }
#   
# }
