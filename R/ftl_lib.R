#' readAndUpdatePRIORSIMS
#' 
#' @export
readAndUpdatePRIORSIMS <- function(context = NULL
                                   , SIMS = NULL
                                   , ddsens = NULL){
  if (file.exists("data/PRIORSIMS.rds")){
    PRIORSIMS <- readRDS("data/PRIORSIMS.rds" )
    # PRIORSIMS_JSON <- readLines("data/PRIORSIMS.json" )
    # PRIORSIMS <- jsonlite::fromJSON(PRIORSIMS_JSON)
  } else {
    PRIORSIMS <- list()
  }
  # SIMS <- .SIMS
  log_info("contexts in file: {paste(names(PRIORSIMS), collapse = ',')}")
  if (is.null(SIMS)){
    if (!is.null(context)){
      SIMS <- PRIORSIMS[[context]]$SIMS
    } else {
      SIMS <- PRIORSIMS[["baserun6min"]]$SIMS
    }
  }
  # reclean for saving 
  {
    context <- SIMS$context
    SIMS$session <- NULL
    SIMS$progress <- NULL
    class(SIMS) <- "list"
    SIMS
    PRIORSIMS[[context]]$SIMS <- SIMS
  }
  
  if (!is.null(ddsens)){
    PRIORSIMS[[context]]$ddsens <- ddsens
  }
  PRIORSIMS[[selected$context]]$varFocus.all <- PRIORSIMS[[selected$context]]$ddsens[, unique(kpi)]
  PRIORSIMS[[selected$context]]$parFocus.all <- aphVariableLevels(PRIORSIMS[[selected$context]]$ddsens)
  # print( compareNames(varFocus.s, PRIORSIMS[[selected$context]]$ddsens[, unique(kpi)]    ) )
  # print( compareNames(parFocus.s, aphVariableLevels(PRIORSIMS[[selected$context]]$ddsens)) )
  
  saveRDS(PRIORSIMS, "data/PRIORSIMS.rds" )
  
  # PRIORSIMS_JSON <- jsonlite::toJSON(PRIORSIMS)
  # object.size2(PRIORSIMS_JSON)
  # writeLines(PRIORSIMS_JSON, "data/PRIORSIMS.json" )
  invisible(PRIORSIMS)
}


#' ftlPACKINIT
#' @export
ftlPACKINIT <- function(dddir = "."
                        , re_SOURCE = FALSE
                        , pack.s = c("ftlHES", "ftlLite", "ftlPlot", "ftlOde")
                        , middleDir = "src"
){
  searchPathOLD <- search()
  lsPACKENV <- list()
  getwd()
  ftlSRCdir <- file.path(dddir, middleDir)
  # dir.exists(ftlSRCdir)
  
  pack.oi <- pack.s[1]
  for (pack.oi in pack.s){
    srcdir <-  file.path(ftlSRCdir, pack.oi, "R")
    dir.exists(srcdir)
    if(re_SOURCE){
      ENV_ftl <- new.env()
      
      ffr.s <- list.files(srcdir, pattern = "\\.R$")
      ffr.s
      message("reading ", paste(length(ffr.s), " files from ", srcdir))
      for (ffr in ffr.s){
        if (file.exists(file.path(srcdir, ffr))){
          source(file.path(srcdir, ffr), local = ENV_ftl)
        } else {
          message("file not found ", file.path(srcdir, ffr))
        }
      }
      lsPACKENV[[pack.oi]] <- ls(envir = ENV_ftl)
      ENVNAME <- paste0("ENV_", pack.oi)
      while(ENVNAME %in% search()) {
        message("detaching", ENVNAME)
        detach(ENVNAME, unload = TRUE, character.only = TRUE, force = TRUE)
      }
      message("attaching ", ENVNAME)
      attach(as.list(ENV_ftl), name = ENVNAME)
    }
  }
  searchPathNEW <- search()
  message("added to search list:")
  print(setdiff(searchPathNEW, searchPathOLD))
  list(dddir = dddir
       , lsPACKENV = lsPACKENV
       , searchPathOLD = searchPathOLD
       , searchPathNEW = searchPathNEW)
}


#' hsource1
#' @export
hsource1 <- function(file
                     , dir = Sys.getenv(dirId, ".")
                     , dirId = c("DEV_CYCLISTPATH", "APP_CYCLISTPATH")[1]
                     , subdir = "src/ftlHES/R"
                     , ...){
  source(file.path(dir, subdir, file), ...)
}



# prep_appLaunch( devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
#                 , appDir = Sys.getenv("APP_CYCLISTPATH", ".")
#                 , overwrite = FALSE)
#' prep_appLaunch
#' @examples \dontrun{
#'   prep_appLaunch()
#' }
#' 
#' @export
# prep_appLaunch <- function(devDir = Sys.getenv("DEV_CYCLISTPATH")
#                            , appDir = Sys.getenv("APP_CYCLISTPATH")
#                            , pack.s = c("ftlHES", "ftlLite", "ftlPlot")
#                            , overwrite = TRUE){
#   
#   print(file.copy(file.path(devDir, "ftlPACKINIT.R")
#                   , file.path(appDir, "ftlPACKINIT.R")
#                   , overwrite = overwrite))
#   
#   pack.oi <- pack.s[1]
#   for (pack.oi in pack.s){
#     fromDir <-  file.path(devDir, "src", pack.oi, "R")
#     stopifnot(dir.exists(fromDir))
#     ffr.s <- list.files(fromDir, pattern = "\\.R$", full.names = FALSE)
#     message("copying those files after 5 secs... ********************* ", pack.oi)
#     .ffr.sPREP <<- ffr.s
#     print(ffr.s)
#     Sys.sleep(5)
#     
#     destDir <-  file.path(appDir, "src", pack.oi, "R")
#     stopifnot(dir.exists(destDir))
#     ffr <- ffr.s[1]
#     for (ffr in ffr.s){
#       devFfr <- file.path(fromDir, ffr)
#       appFfr <- file.path(destDir, ffr)
#       if (file.exists(devFfr)){
#         ok <- file.copy(devFfr, appFfr, overwrite = overwrite)
#         if (!ok) message(" ******************************* FAILED copying ", ffr)
#       }
#     }
#     
#   }
#   
#   print(file.copy(file.path(devDir, "www/dc/attributedTo.md")
#                   , file.path(appDir, "www/dc/attributedTo.md")
#                   , overwrite = overwrite))
# }


#' mask_old_dir
#' @export
mask_old_dir <- function(devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
                         , overwrite = TRUE){
  olddir <- file.path(devDir, "old")
  stopifnot(dir.exists(olddir))
  ffr.s <- list.files(olddir, pattern = "\\.R$", full.names = FALSE)
  message("changing extention of those files")
  print(file.path(olddir, ffr.s))
  print(sub("\\.R$", ".RZ", file.path(olddir, ffr.s)))
  for (ffr in ffr.s){
    devFfr <- file.path(devDir, "old", ffr)
    tmpFfr <- sub("\\.R$", ".RZ", devFfr)
    if (file.exists(devFfr)){
      log_info("renaming  {devFfr} to {tmpFfr}")
      file.rename(devFfr, tmpFfr)
    }
  }
}

#' unmask_old_dir
#' @export
unmask_old_dir <- function(devDir = Sys.getenv("DEV_CYCLISTPATH", ".")
                           , overwrite = TRUE){
  olddir <- file.path(devDir, "old")
  stopifnot(dir.exists(olddir))
  ffr.s <- list.files(olddir, pattern = "\\.RZ$", full.names = FALSE)
  message("changing extention of those files")
  print(file.path(olddir, ffr.s))
  print(sub("\\.RZ$", ".Z", file.path(olddir, ffr.s)))
  for (ffr in ffr.s){
    devFfr <- file.path(devDir, "old", ffr)
    tmpFfr <- sub("\\.RZ$", ".R", devFfr)
    if (file.exists(devFfr)){
      log_info("renaming  {devFfr} to {tmpFfr}")
      file.rename(devFfr, tmpFfr)
    }
  }
}

