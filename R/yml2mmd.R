#' yml2mmd
#' @examples \dontrun{
#'   (ffyml.s <- list.files("../aphConfig/inst/", pattern = ".*clean.*.yml", full.names = TRUE))
#'   j <- 6
#'   ffyml.s[j]
#'   MMD <- yml2mmd(ffyml = ffyml.s[j], sensorType.s = c("stem_load_cell", "wire_load_cell"
#'                          , "substrate_scale", "temperature", "humidity")[1:5], preview = TRUE)
#' }
#' @export
yml2mmd <- function(ffyml = "../aphConfig/inst/cleanSensor.yml"
                    , sensorType.s = c("stem_load_cell", "wire_load_cell"
                                     , "substrate_scale", "temperature"
                                     , "humidity"
                                     , "any", "all")[1:5]
                    , wrapSep = "<BR>"
                    , orientation = c("TB", "LR")[1]
                    , preview = F
){
  file.exists(ffyml)
  rec <- yaml::read_yaml(ffyml)
  # names(rec)
  # names(rec$default)
  # names(rec$default[[1]])
  # names(rec$default[[2]])
  mmd <- paste("graph", orientation)
  sensorType <- sensorType.s[1]
  for (sensorType in sensorType.s){
    message("  =========== sensorType ", sensorType)
    sensorTypeRecipe <- rec$default[[sensorType]]
    stepLabs <- names(sensorTypeRecipe)
    stepFuns <- lapply(sensorTypeRecipe, names)
    stepArgs <- lapply(sensorTypeRecipe, function(x) lapply(x, names))

    lim <- stepFuns == "enrichDT"
    lim <- rep(TRUE, length(stepFuns))
    # stepFuns[lim]
    {
      outs <- lapply(sensorTypeRecipe[lim], function(x) x[[1]]["newProcessName"])
      outs <- lapply(outs, `[[`, 1)
      # lapply(outs, function(x) isTRUE(grepl("\\(", x)))
      # paste0('list(', x,')')
      outs <- lapply(outs, function(x) ifelse(is.null(x), "NULL", ifelse(isFALSE(grepl("\\(", x)), x, unlist(eval(parse(text = x )) ) )))
      # outs
    }
    {
      ins <- lapply(sensorTypeRecipe[lim], function(x) x[[1]]["DTargs"])
      ins <- lapply(ins, `[[`, 1)
      # ins <- lapply(ins, function(x) {print(x) ; ifelse(is.null(x), "NULL", unname(unlist(eval(parse(text = x)) )) )})
      ins <- lapply(ins, function(x) ifelse(is.null(x), "NULL", unname(unlist(eval(parse(text = x)) )) ))
      # ins
    }
    commonObj <- setdiff(intersect(unique(unlist(ins)), unique(unlist(outs))), "NULL")

    {
      mmddeclare <- unname(unlist(sapply(commonObj, function(x) paste0(x, "((", x, "))"))))
      mmd <- c(mmd, mmddeclare)

      s <- stepLabs[3]
      for (s in stepLabs){
        stopifnot(!is.list(mmd))
        message(s)
        if (ins[s] != "NULL"){
          mmddeclare <- unname(unlist(sapply(setdiff(ins[s], commonObj), function(x) paste0(s, "{", x, "}"))))
          mmd <- c(mmd, mmddeclare)
          mmdin <- unname(unlist(sapply(ins[s], function(x) paste(unname(x), "-->", s))))
          mmd <- c(mmd, mmdin)
          mmdout <- unname(unlist(sapply(outs[s], function(x) paste(s, "-->", unname(x)))))
          mmd <- c(mmd, mmdout)
        } else {
          #   print(sensorTypeRecipe[s])
          #   print(ins[s])
          #   print(outs[s])
        }
      }
      # mmd
    }

  }

  mmd <- unique(mmd)

  if (preview){
    p_mmd <- DiagrammeR::mermaid(paste(mmd, collapse = "\n"))
    print(p_mmd)
    .p_mmd <<- p_mmd
  }
  mmd
}
