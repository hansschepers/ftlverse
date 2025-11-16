#' extractSourceCodeHighlight
#' @examples \dontrun{
#'   library(logger)
#'   extractSourceCodeHighlight("sourceSinkModel12", showComments = "none")
#'   extractSourceCodeHighlight("sourceSinkModel12", toCiteStart = "# OdeStart ", prepForLatex = TRUE)
#'   extractSourceCodeHighlight("sourceSinkModel12", toCiteStart = "# ObsStart", prepForLatex = TRUE)
#'   
#'   extractSourceCodeHighlight("SIR01OdeSim")
#'   #scriptExample <- list.files(#system. #file("plantActivity", package = "aphModels")
#'   #                             , full.names = TRUE)[1]
#'   extractSourceCodeHighlight(FILEorFUN = scriptExample)
#' }
#' @importFrom stringr str_trim
#' @export
extractSourceCodeHighlight <- function(
  FILEorFUN
  , showComments = c("none", "all", "partial")[2]
  , keepRoxygen = TRUE
  , keepLatexComments = TRUE
  , dropLatexDrop = FALSE
  , toCiteStart = "# toCiteStart"
  , toCiteEnd = "auto" # e.g. "toCiteEnd"
  , addLineNumbers = c("clean", "orig", "none")[2]
  , trimRegex = character(0)  # c("^\\s", "^,", "^\\s")
  , omitEmpty = FALSE
  , prepForLatex = FALSE
  , addLatexDollars = TRUE
  , removeEmptyLines = TRUE
  # , symbols = character(0)
  , symbols = character(0)
  , autoAbbreviate = 4
  , verbosity = logger::log_threshold()
){
  if (showComments == "none"){
      keepLatexComments <- FALSE
      keepRoxygen  <- FALSE
}
  if (toCiteEnd == "auto"){
    toCiteEnd <- sub("Start$", "End", stringr::str_trim(toCiteStart))
  }
  symbols <- c(PlantLoad = "L"
               , Strength = "S"
               , Maturity = "M", maturity = "M"
               , Radiation = "R", radiation = "R"
               , maintenance = "m"
               , Temperature = "T", temp = "T"
               , trussSpeed = "V"
               , DWpercPlant = "DW_{\\mbox{plant}}"
               , perc = "fr"
               , weight = "W", Weight = "W"
               , floweringFailureRisk = "\\mbox{f}_{\\mbox{risk}}"
               , flowering = "\\mbox{f}"
               , photosynthesis = "\\Phi"
               , source = "SRC"
               , sink = "SNK"
               , LightUseEfficiency = "\\Lambda", LUE = "\\Lambda"
               , headPresent = "hp(t)"
               , timeSinceHeadRemoval = "ht(t)"
               , degrees10 = "10"
               , JoulePerMJ = "1000000"
               , cm2PerM2 = "10000"
               , gFWPerM2Leaf = "SLA"
               , daysPerWeek = "7"
  )
  if(prepForLatex){
    showComments <- "none"
    addLineNumbers <- "none"
    keepRoxygen <- FALSE
    omitEmpty <- TRUE
  }
  
  # get right blocks of code from file ----
  {
    toCiteStart <- stringr::str_trim(toCiteStart)
    toCiteEnd <- stringr::str_trim(toCiteEnd)
    if (file.exists(FILEorFUN)){
      fileName <- FILEorFUN
    } else {
      FILEorFUN <- sub("Sim$", "", FILEorFUN)
      fileName <- utils::getSrcFilename(match.fun(FILEorFUN), full.names = TRUE)
    }
    str(fileName)
    
    sourceCodeLines <- readLines(fileName)
    # sourceCodeLines <- paste0(sourceCodeLines, "\\n")
    # filter, e.g. trim with series of regex
    for (rege in trimRegex){
      sourceCodeLines <- sub(rege, "", sourceCodeLines)
    }
    
    if ("orig" %in% addLineNumbers){
      sourceCodeLines <- paste(seq(length(sourceCodeLines)), sourceCodeLines)
    }
    if (removeEmptyLines){
      sourceCodeLines <- sourceCodeLines[nchar(trimws(sourceCodeLines)) > 0]
    }
    
    .sourceCodeLines <<- sourceCodeLines
    # sourceCodeLines <- .sourceCodeLines
    # (tagStarts <- grep(toCiteStart, sourceCodeLines))
    # (tagEnds   <- grep(toCiteEnd, sourceCodeLines))
    (tagStarts <- grep(paste0("(#')*.*", toCiteStart), sourceCodeLines))
    (tagEnds   <- grep(paste0("(#')*.*", toCiteEnd), sourceCodeLines))
    
    coreCode <- character(0)
    
    if (length(tagStarts) == 0){
      log_info("no tagStarts found, reading whole file..")
      tagStarts <- 1
      tagEnds <- length(sourceCodeLines)
    }
    
    for (ii in seq_along(tagStarts)){
      if (is.na(tagEnds[ii])){
        log_error("extractSourceCodeHighlight|not enough ending code tags for {toCiteEnd} in file {fileName}")
        break
      }
      from <- tagStarts[ii] + ifelse(tagStarts[ii] == 1, 0, 1)
      to <- tagEnds[ii]   - 1
      if(to >= from){
        coreCode <- c(coreCode
                    , sourceCodeLines[seq(from = from, to = to)])
      } else {
        log_debug("from, to ignored: {from}-{to}")
      }
    }
  }
  coreCode
  # remove comments ----
  if (showComments != "all"){
    log_trace("-----------------------------------handling comments")
    
    .coreCode0 <<- coreCode
    # coreCode <- .coreCode0
    isComment <- grepl("^\\s*#", coreCode)   # hash after code is left in
    if (showComments[1] == "none"){
      coreCode <- gsub("#.*$", "", coreCode) # also drop these line endings
    }
    if (dropLatexDrop){
      isComment <- isComment | grepl("#) omit.*$", coreCode) # also drop these equations
    }
    # coreCode[isComment]
    if (showComments[1] == "partial"){
      isComment <- isComment & !grepl("--$", coreCode)
    }
    if (keepLatexComments){
      isComment <- isComment & !grepl("^\\s*#)", coreCode)
    }
    
    if (keepRoxygen){
      isComment <- isComment & !grepl("^#'", coreCode)
    }
    coreCode <- coreCode[!isComment]
  }
  
  if (omitEmpty){
    hasIF <- grepl("^\\s*if", coreCode)
    codeTmp <- sub("\\s*[ {}\\.,-_]{1,}", "", coreCode)
    coreCode <- coreCode[nchar(codeTmp) > 0 & !hasIF]
  }
  
  if ("clean" %in% addLineNumbers){
    coreCode <- paste(seq(length(coreCode)), coreCode)
  }
  
  ################################################################ prepForLatex
  
  coreCode <- stringr::str_trim(coreCode)
  .coreCode <<- coreCode
  .coreCode[1:4]
  
  if(prepForLatex){
    message("for latex...")
    coreCode <- rev(coreCode)
    
    if (autoAbbreviate > 0){  # cm2PerM2
      # all.names(parse(text=.coreCode))
      codeTmp <- coreCode[!grepl("^\\s#)", coreCode)]
      # codeTmp = paste(codeTmp, collapse = "\n")
      # str(codeTmp)
      # codeTmp[25:33]
      vv <- all.vars(parse(text = codeTmp))
      vv <- vv[!tolower(vv) %in% tolower(names(symbols))]
      .vv0 <<- vv
      # symbols <- character(0)
      # vv <- .vv0
      {
        abb <- 4
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.risk$", repl = "_{\\\\mbox{risk}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.week$", repl = "_{\\\\mbox{wk}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.init$", repl = "_{\\\\mbox{init}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.satu$", repl = "_{\\\\mbox{perc}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.perc$", repl = "_{\\\\mbox{perc}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.success$", repl = "_{\\\\mbox{perc}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.ratio$", repl = "_{\\\\mbox{r}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.rel$", repl = "_{\\\\mbox{r}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.ref$", repl = "_{\\\\mbox{ref}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.effect$", repl = "_{\\\\mbox{r}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.fruits$", repl = "_{\\\\mbox{fr}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.plant$", repl = "_{\\\\mbox{pl}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.plant.ref$", repl = "_{\\\\mbox{pl}}^{\\\\mbox{ref}}", abb = abb ))
        
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.air$", repl = "_{\\\\mbox{air}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.driver$", repl = "_{\\\\mbox{u}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.base$", repl = "_{\\\\mbox{b}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.c$", repl = "_{\\\\mbox{c}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.s$", repl = "_{\\\\mbox{s}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.in$", repl = "_{\\\\mbox{in}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.req$", repl = "_{\\\\mbox{req}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.tot$", repl = "_{\\\\mbox{total}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.conc$", repl = "_{\\\\mbox{conc}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.dependence$", repl = "_{\\\\mbox{Q10}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.exponent$", repl = "_{\\\\mbox{e}}", abb = abb ))
        
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.obs$", repl = "_{\\\\mbox{obs}}", abb = abb ))
        symbols <- c(symbols, autoSymbolize(vv, regex = "\\.equ$", repl = "_{\\\\mbox{eq}}", abb = abb ))
        symbols
      }
      vv <- vv[!tolower(vv) %in% tolower(names(symbols))]
      .symbols1 <<- symbols
      .vv <<- vv
      # automaticSymbols <- setNames(substr(vv, 1, autoAbbreviate), vv)
      # symbols <- c(symbols, automaticSymbols)
      .symbols <<- symbols
    }
    
    ind <- grepl("^\\s*#)", coreCode)
    ind.cu <- cumsum(ind)
    # print(coreCode)
    # message("+++++++++++++++")
    # coreCodeBlocks <- split(coreCode, ind.cu)
    
    # w1 <<- lapply(coreCode, function(x) {
    #   ifelse(grepl("^\\s*#)", x)
    #          , character(0)
    #          , all.names(parse(text=x)) 
    #   )}
    # )
    
    equal <- ifelse(addLatexDollars, "=", "&=")
    # fractions
    log_trace("=================================================================")
    coreCode[!ind] <- sub("#.*$", "", coreCode[!ind])  # remove trailing comment  #[^)]??
    coreCode <- gsub("=",  equal, coreCode)
    coreCode <- gsub("<-", equal, coreCode)
    coreCode <- gsub(paste0("d_(.*\\s*)",equal,"(.*)")
                     , paste0("\\\\frac{d \\1}{d t} ",equal," \\2"), coreCode)
    coreCode <- gsub("d_(.*)(\\s{1,})", "\\\\frac{d \\1}{d t}\\2", coreCode)
    # functions
    coreCode <- gsub("modelReduction::zidz\\((.*),\\s*(.*)))\\s*(.*)\\s*$"
                     , "\\\\frac{\\1}{\\2}\\3", coreCode)
    coreCode <- gsub("pmax", "\\\\mbox{pmax}", coreCode)
    coreCode <- gsub("pmin", "\\\\mbox{pmin}", coreCode)
    # whitespace
    coreCode <- gsub(" {2,}", " ", coreCode)
    # coreCode <- gsub("^  ", " ", coreCode)
    #TODO
    coreCode <- gsub("\\*", " \\\\cdot ", coreCode)
    # coreCode <- gsub("\\^", "\\\\mbox{^}", coreCode)
    if (addLatexDollars){
      # message("----------")
      # print(ind)
      # print(coreCode)
      # print(ind.cu)
      # message("----------")
      coreCode[!ind] <- paste("$$", coreCode[!ind], "$$")
      coreCode[ind] <- paste0("\n\n", sub("#)", "", coreCode[ind]), "\n\n")
      if (verbosity > 400){print(coreCode[ind])}
      coreCode[ind] <- sub("_", "\\\\mbox{}", coreCode[ind])
    } else {
      coreCode <- paste0(coreCode, "\\\\\n")
      coreCode <- c("\\begin{align}", coreCode, "\\end{align}")
      # coreCode <- paste0("\n\n$$\n", paste(coreCode, collapse = "\n"), "\n$$\n\n")
    }
    # indCom <- grepl("\\s*#)", coreCode)
    # coreCode[!indCom] <- sub("#).*$", "", coreCode[!indCom])
    
    # print(coreCode)
    if (length(symbols)){
      .coreCode2 <<- coreCode
      toAutoReSymbolize <- grepl("$$", coreCode, fixed = TRUE)
      # & prepForLatex?
      symbols <- rev(symbols[order(nchar(names(symbols)))])
      # print(symbols)
      # message("===============================")
      skip <- character(0)
      for (word in setdiff(names(symbols), skip)){
        # message(word)
        # print(symbols[word])
        # print(coreCode)
        coreCode[toAutoReSymbolize] <- gsub(word
                         # , unname(symbols[word])
                         , paste0("{", unname(symbols[word]), "}")
                         , coreCode[toAutoReSymbolize]
                         , fixed = TRUE) # , ignore.case = TRUE (overriden by fixed = TRUE)
        if (log_threshold() >= 400){
          cat(".")
        }
      }
      cat("\n")
      .symbolsOrdered <<- symbols
    }
  }
  coreCode
}


# # extractSourceCodeHighlight("sourceSinkModel01Ode", showComments = "none")
# wwA <- extractSourceCodeHighlight("sourceSinkModel01Ode", toCiteEnd   = "# OdeStart ", prepForLatex = TRUE)
# wwX <- extractSourceCodeHighlight("sourceSinkModel01Ode", toCiteStart = "# OdeStart", prepForLatex = TRUE)
# wwY <- extractSourceCodeHighlight("sourceSinkModel01Ode", toCiteStart = "# ObsStart", prepForLatex = TRUE)
# ww <- c(wwA, "Core Dynamics", wwX, "non-core KPIS, Observed variables", wwY)
# print(ww)
# 
# # lapply(ww, function(x) all.names(parse(text=x)) )
# writeLines(ww, file.path(getCacheDir(), "sourceSinkModel01Ode.Rmd"))



# cleanSourceCodeHightlight
# 
# cleanSourceCodeHightlight <- function(xin
#                                       , removeStrings = c("parms <- c\\(", "parms\\$")
# ){
#   xx <- strsplit(xin, "\\n")[[1]]
#   for (ii in seq_along(xx)){
#     if (length(grep("```", xx[ii])) == 0) xx[ii] <- gsub(",", "", xx[ii])
#   }
#   for (ii in removeStrings) xx <- gsub(ii, "", xx)
#   xx <- trimws(xx, which = "both")
#   x <- paste(xx, collapse = "\n")
#   x
# }



