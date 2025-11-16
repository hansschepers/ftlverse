#' readKB_LIST
#' @examples \dontrun{
#'   KB_LIST <- readKB_LIST()
#'   KB_LIST <- readKB_LIST(doAttach = TRUE)
#'   names(KB_LIST)
#'   search()
#'   KB_LIST$KBDB
#'   #compareNames(KB_LIST$sim2data, SIM2PROCESS)
#'   KB_LIST$sim2displayEN
#'   KB_LIST$KBDB$unitsLabelEN
#'   sim2displayEN
#'   find("sim2displayEN")
#'   search()
#'   KB_LIST <- readKB_LIST(doAttach = TRUE, removeFromGlobal = FALSE)
#'   sim2displayEN
#' }
#' @export
readKB_LIST <- function(doAttach = FALSE
                        , removeFromGlobal = TRUE
                        , metaDataDir = Sys.getenv("CYCLISTPATH")){
  log_debug("readKB_LIST| reading KBDB")
  # FITKPI <- readFITKPI()
  KBDB <- readKBDB(metaDataDir = metaDataDir)
  KB_LIST <- list()
  KB_LIST$KBDB = KBDB
  KB_LIST$dcPars25 <- readParDB(sheet = "dcPars25")
  KB_LIST$dcPars6 <- readParDB(sheet = "dcPars6")
  KB_LIST$dict <- readParDB(sheet = "dict")
  KB_LIST$huez <- readParDB(sheet = "huez")
  
  if(T){
    # KBDB$frontendName
    extraDictNL <- c(Fruits = "Vruchten", Production = "Produktie"
                     , labour = "Arbeid", temperatures = "Temperaturen"
                     , radiation = "Instraling", CO2 = "CO2"
                     , Yield = "Week-Produktie", sensors = "Sensoren")
    extraDictEN <- c(Fruits = "Fruits", Production = "Production"
                     , labour = "Labour", temperatures = "Temperatures"
                     , radiation = "Radiation", CO2 = "CO2"
                     , Yield = "Weekly-Production", sensors = "Sensors")
    # write.csv(cbind(extraDictEN,extraDictNL), "../tmp.csv")
    KB_LIST$sim2displayEN <- c(setNames(KBDB$labelEN, KBDB$simName), extraDictEN)
    KB_LIST$sim2displayNL <- c(setNames(KBDB$labelNL, KBDB$simName), extraDictNL)

    KB_LIST$sim2data <- setNames(KBDB$processName, KBDB$simName)
    KB_LIST$data2sim <- setNames(KBDB$simName, KBDB$processName)

    # frontendName
    # KB_LIST$sim2frontend <- setNames(KBDB$frontendName, KBDB$simName)
    # KB_LIST$frontend2sim <- setNames(KBDB$simName, KBDB$frontendName)

    KB_LIST$sim2displayXX <- KB_LIST$data2sim
    KB_LIST$sim2unitsNL <- setNames(KBDB$unitsLabelNL, KBDB$simName)
    KB_LIST$sim2unitsEN <- setNames(KBDB$unitsLabelEN, KBDB$simName)


    # KB_LIST <- lapply(KB_LIST, \(x) x[!his.na(x)])
    # KB_LIST <- lapply(KB_LIST, \(x) x[!is.na(names(x))])


    KB_LIST$data2displayEN <- KB_LIST$sim2displayEN[KB_LIST$data2sim]
    names(KB_LIST$data2displayEN) <- names(KB_LIST$data2sim)
    KB_LIST$data2displayEN

    KB_LIST$data2displayNL <- KB_LIST$sim2displayNL[KB_LIST$data2sim]
    names(KB_LIST$data2displayNL) <- names(KB_LIST$data2sim)
    KB_LIST$data2displayNL


    KB_LIST$data2displayENwithUnits <- setNames(paste0(KB_LIST$data2displayEN
                                                       , "(", KBDB$unitsLabelEN, ")")
                                                , names(KB_LIST$data2displayEN))

    KB_LIST$data2displayNLwithUnits <- setNames(paste0(KB_LIST$data2displayNL
                                                       , "(", KBDB$unitsLabelNL, ")")
                                                , names(KB_LIST$data2displayNL))

    # KB_LIST$FITKPI = FITKPI
    # 
    # KB_LIST$extraDictEN <- c(setNames(KB_LIST$aphGroupings$labelEN
    #                                   , KB_LIST$aphGroupings$grouping), extraDictEN)
    # KB_LIST$extraDictNL <- c(setNames(KB_LIST$aphGroupings$labelNL
    #                                   , KB_LIST$aphGroupings$grouping), extraDictNL)
  }
  if (doAttach){
    while ("HES_KB_LIST" %in% search()){
      detach("HES_KB_LIST", unload = TRUE)
    }
    if (removeFromGlobal){
      suppressWarnings(
        rm(list = names(KB_LIST), envir = .GlobalEnv)
      )
    }
    attach(KB_LIST, name = "HES_KB_LIST")
    return(invisible(KB_LIST))
  }
  KB_LIST
}


# readFITKPI
#
# @export
# readFITKPI <- function(){
#   log_trace("readreadFITKPI| reading readFITKPI")
#   readParDB(sheet = "FITKPI")
# }


#' readKBDB
#'
#' @export
readKBDB <- function(metaDataDir = Sys.getenv("CYCLISTPATH")){
  log_trace("readKBDB| reading KBDB")
  variables <- readParDB(sheet = "variables", metaDataDir = metaDataDir)
  params <- readParDB(sheet = "parameters", metaDataDir = metaDataDir)
  keep <- names(variables)#[1:maxCol]
  stopifnot(all(keep %in% names(params)))
  # rbind(variables, params[, ..keep], fill=TRUE)
  KBDB <- rbindlist(list(vars = variables
                         , pars = params[, ..keep])
                    , fill = TRUE, idcol = "type")
  KBDB
}


#' readParDB
#' @examples \dontrun{
#'   parDT <- readParDB()
#'   parDT2 <- readParDB(metaDataDir = Sys.getenv("CYCLISTPATH"))
#'   parDT2[grepl("^T", simName, ignore.case = TRUE)]
#'   compareNames(names(parDT), names(parDT2))
#'   all.equal(parDT[1:44], parDT2[1:44])
#'   #diffdfs::diffdfs(parDT[1:99], parDT2[1:99], verbose = TRUE, key_cols = "simName")
#'   #diffdfs::diffdfs(parDT2[1:99], parDT[1:99], verbose = TRUE, key_cols = "simName")
#' }
# @importFrom readxl excel_sheets read_excel
#' @export
readParDB <- function(parDBname = "model_info"
                      , metaDataDir = Sys.getenv("CYCLISTPATH")
                      , expr = NULL
                      , sheet = "parameters"
                      , verbosity = 0
){
  # xlsNames <- list.files(metaDataDir, pattern = "\\.xlsx", full.names = TRUE)
  if ("readxl" %in% list.files(.libPaths())){
    xlsxpath <- file.path(metaDataDir, paste0(parDBname, ".xlsx"))
    xlsxpath <<- xlsxpath
    hfile.info(xlsxpath)
    # readxl::excel_sheets(parDB)
    stopifnot(file.exists(xlsxpath))
    tryCatch({sheetAvailable.s <- readxl::excel_sheets(xlsxpath)}
             , error = function(e)
               stop("file exists, but can't open it, likely you have the file open in excel?!")
    )
    
    parDT <- readxl::read_excel(xlsxpath, sheet = sheet)
  } else {
    csvPath <- file.path(metaDataDir, paste0(parDBname, "__", sheet, ".csv"))
    .csvPath <<- csvPath
    hfile.info(csvPath)
    parDT <- read.csv(csvPath)
  }
  parDT <- as.data.table(parDT)
  if ("value" %in% names(parDT)){
    parDT[, valueOrig := value]
    suppressWarnings({
      parDT[, value := as.numeric(value)]
    })
    if (verbosity > 600){
      print(parDT[is.na(value), .(simName, valueOrig)])
    }
    parDT[, valueOrig := NULL]
  }
  if (!is.null(expr)){
    parDT <- parDT[eval(expr)]
  }
  attr(parDT, "metaDataDir") <- metaDataDir
  attr(parDT, "parDBname") <- parDBname
  attr(parDT, "sheet") <- sheet
  parDT[]
}


#' queryParSets
#' @examples \dontrun{
#'   parDT = readParDB()
#'   attributes(parDT)
#'   parSet <- getParSets(parDT = parDT)
#'   parSet %>% lapply(unlist) %>% lapply(names)
#'   strList(parSet)
#'   hprettyNum(parSet$planCycle)
#'   queryParSets(segment = "cherry")
#' }
#'
#' @export
queryParSets <- function(parSet
                         , modelId = c("sourceSinkModel12")[1]
                         , purpose = c("plan", "optimize", "risk")[0]
                         , segment = c("large", "cherry")[0]
                         , cycle = c("lit", "yr")[0]
){
  list()
}

