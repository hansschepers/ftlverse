#' addYgroups
#' @description Add a 'panel' column to a long DT, with grouped processNames
#' 
#' Grouping can occur in multiple ways:
#' 
#' @param yGroups gives a specific list of groupings
#' @param patterns provides a character vector of regular expressions for grep
#' @param rest can group all processNames not grouped in a 'rest' panel
#' 
#' @examples \dontrun{
#'   pattern <- as.list(c("r2", "fruit", "setting", "temp"))
#'   
#' }
#' @return the same data table dtm, with extra column 'panel' added
#' @export
addYgroups <- function(
  dtm
  , pattern = unique(sapply(strsplit(as.character(unique(dtm[,get(variable.name)]))
                                     , "[042_\\.]"), `[[`, 1))
  , rest = character(0)
  , yGroups = list(SourceSink = c("surplus", "sink", "source", "photosynthesis")
                   , fractions = c("maintenance.frac", "effect"
                                   , "fruit2plant.perc", "radiation.satu", "settingsuccess")
                   , temperatures = c("temp_air", "tem24hr", "temp")
                   , radiation = "radiation.driver", "radiation.led", "radiation.tot")
  , groups = list()
  , removeZeroRange = TRUE
  , panel.out = character(0)
  , restInRest = TRUE
  , variable.name = "processName"
){
  setDT(dtm)
  dtm <- copy(dtm)
  if (length(pattern)){
    if ("yGroupPatterns" %in% names(yGroups)){
      log_warn("overwriting yGroups$yGroupPatterns, was {yGroups$yGroupPatterns}")
    }
    yGroups$yGroupPatterns <- pattern
  }
  if (length(rest) | restInRest){
    yGroups$rest <- rest
  }
  pattern <- yGroups$yGroupPatterns
  rest <- yGroups$rest
  
  if (!variable.name %in% names(dtm)){
    variable.name <- "variable"
  }
  if (!variable.name %in% names(dtm)){
    stop('please specify variable name')
  }
  
  if (removeZeroRange){
    dtsum <- dtm[, .(ran = hdiffrange(value)), by = c(variable.name)]
    zeroRangeVars <- unlist(dtsum[ran == 0, ..variable.name])
    dtm <- dtm[!get(variable.name) %in% zeroRangeVars]
  }
  
  # build groups from patterns ----
  unspecified <- vvOrig <- as.character(unique(dtm[, base::get(variable.name)]))
  vv <- tolower(vvOrig)
  pattern <- as.list(tolower(pattern))
  # print(pattern)
  patternMatches <- list()
  if (length(pattern)){
    if(is.null(names(pattern))){
      log_trace("adding default names to pattern")
      names(pattern) <- make.names2(pattern)  #TODO# make.names2 or 3!?
    }
    # log_trace("vvOrig")
    pat <- names(pattern)[1]
    for (pat in names(pattern)){
      variableLevels <- vvOrig[grep(tolower(pat), vv)]
      patternMatches[[pat]] <- variableLevels
      vvOrig <- setdiff(vvOrig, variableLevels)
      vv <- tolower(vvOrig)
      
      # variableLevels <- vvOrig[grep(pattern[[pat]], vv)]
      # print(variableLevels)
      if (length(variableLevels)){
        unspecified <- setdiff(unspecified, variableLevels)
        toAdd <- setNames(list(a = variableLevels), pat)
        # log_trace("toAdd")
        groups <- c(groups, toAdd)
      }
    }
  }
  groups
  
  # use groups and Ygroups to specify panel name ----
  unspecified <- setdiff(unspecified, unique(unlist(yGroups)))
  if (length(rest) | restInRest){
    # message("putting in 'rest fraction' ", rest)
    # print(unspecified)
    groups[["rest"]] <- unspecified
    # str(groups)
  }
  
  groups <- c(groups, yGroups)
  groups$yGroupPatterns <- NULL
  .groups <<- groups
  if ("panel" %in% names(dtm)){
    dtm[, panelOrig := panel]
    # setnames(dtm, "panel", "panelOrig")
  }
  dtm[, panel := base::get(variable.name)]
  groupID <- 1
  for (groupID in seq_along(groups)){
    if (!any(c("out", "omit") %in% groups[[groupID]])){
      dtm[base::get(variable.name) %in% groups[[groupID]]
          , panel := names(groups)[groupID]]
    }
  }
  
  # remove unwanted panels ----
  dtm <- dtm[!panel %in% panel.out]
  # log_trace("fixing panels as factor...")
  dtm[, panel := factor(panel, levels = unique(panel), ordered = TRUE)]
  
  setattr(dtm, "panel.out", panel.out)
  setattr(dtm, "groups", groups)
  setattr(dtm, "patternMatches", patternMatches)
  
  dtm[]
}
