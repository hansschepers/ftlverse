#' SIMSwrapper
#' 
#' @export
SIMSwrapper <- function(x = list()
                        , SIMS
                        , times = SIMS$times
                        , y = SIMS$y_init00
                        , kpi.s = "all"
                        , driv_funs = SIMS$driv_funs
                        , CONSTANTS = SIMS$CONSTANTS
                        , session = NULL
                        , progress = NULL
                        , verbosity = 400){
  if (!exists(".iisw")) .iisw <- 0
  .iisw <- .iisw + 1
  .iisw <<- .iisw
  cat("o")
  if (verbosity > 400){
    log_info("SIMSwrapper y :  +++++++++++++++++++++++++++++++++++++++++ {.iisw}")
    log_info("SIMSwrapper parameter list:  --------------------- {x}")
    log_info("SIMSwrapper y :  -------------- {y[2]}")
  }
  SIM2 <- runFunHES(editList = x
                    , times = times
                    , y = SIMS$y_init00
                    , driv_funs = SIMS$driv_funs
                    , CONSTANTS = SIMS$CONSTANTS
  )
  .SIM2 <<- SIM2
  
  if (!is.null(session)){
    if (exists(".iiswmax")){
      progress$set(message = paste0("GSAJac called SIMSwrapper ", .iisw, " / ", .iiswmax)
                   , value = .iisw / .iiswmax)
    } else {
      log_debug("no .iiswmax in .GlobalEnv for progress")
    }
  }
  
  # SIM2 <<- .SIM2
  dd2use <- copy(SIM2$out)
  # message("kpis") ; print(dd2use)
  if (!"all" %in% kpi.s){
    kpi.s <- union("time", kpi.s)
    dd2use <- dd2use[, ..kpi.s]
  }
  dd2use
  # message("kpis") ; print(dd2use)
  
  yy <- aphMelt(dd2use)
  yy[, nm := paste(processName, time, sep = "__")]
  yythin <- yy[, setNames(value, nm)]
  .yythin <<- yythin
  yythin
}


#' GSAjac
#' @examples
#' \dontrun{
#' }
#' 
#' @export
GSAjac <- function(SIMS
                   , pois = names(SIMS$Pars)                  # parFocus.all
                   , kpi.s = setdiff(names(SIMS$out), "time") # varFocus.all
                   , times = SIMS$times
                   , method ="simple"
                   , method.args = list(eps = 1e-4)
                   , session = NULL
){
  # if (missing(SIMS)){
  #   # SIMS <- .SIMS
  #   SIMS <- runFunHES(times = c(0, 0.05, 0.2, 0.6))
  # }
  
  if (!is.null(session)){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "setting up jacobian..", value = 0)
  }
  
  
  .iisw <<- 0
  .iiswmax <<- 2 + length(pois)
  # vector output for jacobian
  out_y <- SIMSwrapper(x = list(T_air = 23)
                       , kpi.s = kpi.s
                       , SIMS = SIMS
                       , times = times)
  .out_y <<- out_y
  cat("\n")
  for (ii in seq(1+length(pois))) cat("|")
  cat("\n")
  # .SIMS$Pars[pois]
  
  
  # SIMSwrapperwrapper <- function(x = list(), params = list()){
  #   do.call(SIMSwrapper, c(list(x = x), params))
  # }
  # require(calculus)
  # SI <- SIMSwrapperwrapper(SIMS$Pars[pois][1], params = list(SIMS = SIMS))
  # jacobian_matrix1 <- calculus::jacobian(f = SIMSwrapperwrapper
  #                                        , var = SIMS$Pars[pois]
  #                                        , params = list(SIMS = SIMS
  #                                                        , times = SIMS$times
  #                                                        , y = SIMS$y_init00
  #                                                        , kpi.s = "all"
  #                                                        , driv_funs = SIMS$driv_funs
  #                                                        , CONSTANTS = SIMS$CONSTANTS
  #                                                        , session = NULL
  #                                                        , progress = NULL
  #                                                        , verbosity = 400)  )
  # .jacobian_matrix1 <<- jacobian_matrix1
  
  require(numDeriv)
  jacobian_matrix2 <- numDeriv::jacobian(func = SIMSwrapper
                                         , x = SIMS$Pars[pois]
                                         , method = method
                                         , method.args = method.args
                                         , SIMS = SIMS
                                         , times = times
                                         , kpi.s = kpi.s
                                         , session = session
                                         , progress = progress
  )
  .jacobian_matrix2 <<- jacobian_matrix2
  # jacobian_matrix2 <<- .jacobian_matrix2
  # dim(.jacobian_matrix2)
  # jacobian_matrix2 <- .jacobian_matrix2
  log_info("number of calls to wrapper: {.iisw}")
  
  ddsens <- jac2ddsens(jacobian_matrix2
                       , out_y = out_y
                       , pois = pois)
  setattr(ddsens, "pois", pois)
  setattr(ddsens, "kpis", kpi.s)
  .ddsens <<- copy(ddsens)
  ddsens[]
}


#' jac2ddsens
#'
#'@export
jac2ddsens <- function(jacobian_matrix2
                       , out_y
                       , pois){
  ddsens <- as.data.table(jacobian_matrix2)
  # pois <- attr(.ddsens, "pois")
  setnames(ddsens, names(ddsens), pois)
  # dim(ddsens)
  ddsens[, kpi := names(out_y)]
  # ddsens
  #TODO SCALING
  # https://8e9035f1f8ad4347a832353be3110ae6.app.posit.cloud/
  # all(names(SIMS$out[.N, -1]) ==names(derivsAll))
  ddsens[, `:=`(c("variable", "time"), tstrsplit(kpi, "__", fixed = TRUE))]
  ddsens[, kpi := NULL]
  setnames(ddsens, "variable", "kpi")
  ddsens[, time := as.numeric(time)]
  ddsens[]
}


#' hmodulo
#' @example hmodulo(times, .1)
#' @export
hmodulo <- function(times, modulo, dig = 1000){
  times[0 == (round(dig * times) %% 
                         round(dig * modulo))]
}



#' scaleSensMat
#' @export
scaleSensMat <- function(ddsens
                         , SA_Pars = list(sa_input_kpis = "all"
                                          , sa_input_pois = "all"
                                          , GSA_modulo = .2
                                          , scaleSens = TRUE)
                         , GSA_modulo = SA_Pars$GSA_modulo
                         , scaleSens = SA_Pars$scaleSens
                         , kpiOUT = c("road_distance", "tfat") ){
  times <- hmodulo(unique(ddsens$time), GSA_modulo)
  ddsens <- ddsens[time %in% times]
  # ddsens[, unique(kpi)]
  
  ##################################### filter (& melt !) ###################### 
  if (!"all" %in% SA_Pars$sa_input_kpis){
    ddsens <- ddsens[kpi %in% SA_Pars$sa_input_kpis]
  }
  # melt to 'dd'
  dd <- aphMelt(ddsens)
  if (!"all" %in% SA_Pars$sa_input_pois){
    dd <- dd[processName %in% SA_Pars$sa_input_pois]
  }
  ##################################### scale ################################
  if (isTRUE(scaleSens)) scaleSens <- "external"
  if (scaleSens == "external"){
    dd[KBDB[type == "vars", .(simName, sd)]
       , sdVars := sd, on = c(kpi = "simName")]
    dd[KBDB[type == "pars", .(simName, sd)]
       , sdPars := sd, on = c(processName = "simName")]
    
    # missing #TODO
    log_info("external scaling")
    log_warn("missing parameter SDs: {dd[is.na(sdPars), unique(processName)]}")
    log_warn("missing variable SDs: {dd[is.na(sdVars), unique(kpi)]}")
    
    dd[, value := value / sdVars * sdPars]
    dd_sc <- dd[!is.na(sdVars) & !is.na(sdPars) & abs(sdVars) > 1e-7]
    dd_sc[, c("sdPars", "sdVars") := NULL]
  }
  
  if (scaleSens == "internal"){
    log_info("internal scaling")
    
    dd_sc <- dd[, .(sc = max(abs(value))), by = .(kpi)]
    noEffectKpis <- dd_sc[sc == 0, kpi]
    dd_sc <- dd_sc[dd[!kpi %in% noEffectKpis], on = "kpi"]
    dd_sc[, value := value / sc]
    dd_sc[, sc := NULL]
  }
  
  ddsens_sc <- hdcast(dd_sc)
  ddsens_sc <- ddsens_sc[!kpi %in% SA_Pars$kpiOUT]
  .ddsens_sc <<- copy(ddsens_sc)
  ddsens_sc
}


#' pcaPlotLister
#' @export
pcaPlotLister <- function(out_pca, input){
  plotList <- list()
  plotList <- append(plotList, list(p_pca = out_pca$plot))
  plotList <- append(plotList, parameterClustering(out_pca
                                                   , nPC = input$nPC
                                                   , kTree = input$kTree))
  plotList <- append(plotList
                     , list(vars_parPanel =
                              pggs(.ddsens_sc, doMelt = T, free_y = F)))
  plotList <- append(plotList
                     , list(vars_varPanel =
                              pggs(.ddsens_sc, doMelt = T, facet_w = "kpi", foi = "processName")))
}
