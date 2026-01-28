#' ftlr_init
ftlr_init <- function(){
  owd <- getwd()
  owd <<- owd
  Sys.setenv(FTLR_ROOT = "C:/Users/Lenovo/Documents/R/model_UQ/inst/FTLR")
  setwd(Sys.getenv("FTLR_ROOT"))
  
  source("utils/initFTLR.R")
  initFTLR()
  am <<- function() {source("apps/autoModeller.R")}
  af <<- function() {source("apps/autoFocus.R")}
  rv <<- function() {source("apps/app_repoViewer.R")}
  log_threshold(INFO)
}


#' run_ftlr_model
#' @examples \dontrun{
#'   ftlr_init()
#'   ww <- run_ftlr_model(model_oi = "indeterminate_crop")
#'   ww$p
#'   run_ftlr_model(model_oi = "compost001", editList = list(p_a = 30))
#' }
#' @export
run_ftlr_model <- function(model_oi
                           , editList0 = list()
                           , editList = list()
                           # , model_dir = "models"
                           ){
  if (!exists("initFTLR", mode = "function")){
    ftlr_init()
  }
  log_threshold(FATAL)
  {
    SIMS_0 <- runODEModel(model_oi
                          , editList = editList0)
    print(hprettyNum(SIMS_0$parms))
  }
  .SIMS_0 <<- SIMS_0
  source("C:/Users/Lenovo/Documents/R/model_UQ/inst/FTLR/utils/plot2sims.R", local = TRUE)
  source("C:/Users/Lenovo/Documents/R/model_UQ/inst/FTLR/utils/plot2sims.R", local = FALSE)
  if(!length(editList)){
    p <- plot(SIMS_0)
  } else {
    SIMS <- runODEModel(model_name = model_oi
                        , editList = editList)
    p <- plot2sims(SIMS, SIMS_0)
  }
  invisible(mget(ls()))
}