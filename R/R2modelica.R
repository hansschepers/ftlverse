#TODO
# logistXY()
# radCos()
# aphCumsum() (in postprocess only)
# continuation of lines.
# stop at postprocessing extras


#' R2modelica
#' @examples \dontrun{
#'   KB_LIST <- aphPlot::PackInit()
#'
#'   SIMS = runFun(modelId = "sourceSinkModel15")
#'   SIMS$modelId
#'   ffmo = "../aph-am-Julia/inst/ss15/xss15"
#'   m15.mo <- R2modelica(SIMS, ffmo = ffmo)
#'
#'   SIMS = runFun()
#'   ffmo = "../aph-am-Julia/inst/ss12/nss12"
#'   m12.mo <- R2modelica(SIMS, ffmo = ffmo)
#'   file.info(paste0(ffmo, ".mo"))
#'   m12.mo
#'
#'   str(m12.mo)
#'   grep("[a-zA-Z]\\.", m12.mo, value = TRUE)
#'   grep("\\.[a-zA-Z]", m12.mo, value = TRUE)
#'   grep("0\\.", m12.mo, value = TRUE)
#' }
#' @export
R2modelica <- function(SIMS = runFun()
                       , lang = c("modelica", "julia")[1]
                       , ffmo = NULL){
  if(!dir.exists(dirname(ffmo))){
    dir.create(dirname(ffmo))
  }
  commenter <- switch(lang
                      , modelica = "//"
                      , julia = "#"
  )
  # formals(runFun)[1]
  modelId <- SIMS$modelId
  FUNODE <- switch(modelId
                   , sourceSinkModel15 = "sourceSinkModel15"
                   , "sourceSinkModel12"  # default
                   )
  FUNDAG <- switch(modelId
                   , sourceSinkModel15 = "ss15DAG"
                   , "ss12DAG" # default
  )

  {
    co.mo <- character()
    if (lang == "modelica"){
      co.mo <- c(co.mo, paste0("model Tomato \"", modelId, "\""))
    }
    {
      co.mo <- c(co.mo, paste0(" ", commenter, " ODE states"))
      stateName.s <- names(SIMS$inits)
      stateName <- stateName.s[1]
      for (stateName in names(SIMS$inits)){
        state <- sub("\\.init", "", stateName)
        xx <- hprettyNum(SIMS$inits[[stateName]], digits = 4)
        newLine <- paste0("  Real ", state, "(start=", xx,");")
        if (lang == "julia"){
          # newLine <- paste0(stateName, " = ", SIMS$inits[[stateName]])
          # co.mo <- c(co.mo, newLine)
          newLine <- paste0(state, " = ", SIMS$inits[[stateName]])
        }
        co.mo <- c(co.mo, newLine)
      }
    }

    {
      co.mo <- c(co.mo, paste0("\n", commenter, " Drivers / boundary conditions"))

      driverName.s <- setdiff(names(SIMS$drivers), "Time")
      driverName.s_snake <- gsub("\\.", "_", driverName.s)
      # drivDict <- as.list(setNames(driverName.s_snake, driverName.s))

      if (lang == "modelica"){
        driverName <- driverName.s[1]
        for (driverName in driverName.s_snake){
          newLine <- paste0("  Driver ", driverName,";")
          co.mo <- c(co.mo, newLine)
        }
      }
    }

    {
      # logistXY_doc
      univs <- universalConstants()
      univUnits <- universalUnits()
      if (lang == "modelica"){
        co.mo <- c(co.mo, "\nprotected")
      }
      co.mo <- c(co.mo, paste0("\n", commenter, " Constants"))
      if (lang == "modelica"){
        co.mo <- c(co.mo, '  final constant Real pi(unit="1") = Modelica.Constants.pi;')
      }
      univName.s <- setdiff(names(univs), "pi")
      univName <- univName.s[1]
      for (univName in univName.s){
        un <- univUnits[[univName]]
        value <- univs[[univName]]
        value <- hprettyNum(value)
        newLine <- paste0("  final constant Real ", univName, "(unit=\"", un, "\") = ", value, ";")
        if (lang == "julia"){
          newLine <- paste0(univName, " = ", value)
        }
        co.mo <- c(co.mo, newLine)
      }
    }

    {
      co.mo <- c(co.mo, paste0("\n", commenter, " Parameters"))
      Pars <- SIMS$usedParms
      parName.s <- names(Pars)
      parName.s <- setdiff(parName.s, names(univs))
      parName.s <- setdiff(parName.s, c("iseed"))
      parName.s <- setdiff(parName.s, driverName.s)
      parName <- parName.s[1]
      parsDict <- list()
      for (parName in parName.s){
        mi = KBDB[simName == parName, lb]
        ma = KBDB[simName == parName, ub]
        un = KBDB[simName == parName, units]
        if (!length(mi)){log_warn("no min for {parName}")}
        if (!length(ma)){log_warn("no max for {parName}")}
        if (!length(un)){log_warn("no units for {parName}")}

        parName_snake <- gsub("\\.", "_", parName)
        parsDict[parName] <- parName_snake
        value = unname(unlist(Pars[parName]))
        value <- hprettyNum(value)
        newLine <- paste0("  parameter Real ", parName_snake
                          , "(min=", mi, ", max=", ma
                          , ", unit=\"", un, "\") = ", value, ";")
        if (lang == "julia"){
          number_of_dots_in_string <- sum(strsplit(parName, "")[[1]]== ".")
          if (number_of_dots_in_string > 1){
            log_warn("number_of_dots_in_string > 1 {parName}")
          }
          newLine <- paste0(parName_snake, " = Float64(", value, ")")
        }
        co.mo <- c(co.mo, newLine)
      }
    }

    if (lang != "julia")
    {

      ################################################## DRIVERS
      {
        co.mo <- c(co.mo, "\n//driver equations")
        co.mo <- c(co.mo, paste0("\n", commenter, " drivers"))
        # linesDRIVERS <- as.character(body(get(FUNDAG)))
        linesDRIVERS <- extractSourceCodeHighlight(FILEorFUN = FUNDAG
                                                   , toCiteStart = "# driverStart"
                                                   # , toCiteEnd = "# postProcessing DAG"
                                                   , showComments = "none"
                                                   , keepLatexComments = F
                                                   , keepRoxygen = F
                                                   , addLineNumbers = F)

        linesDRIVERS2 <- sub("(.*).*<-(.*)", "\\1 = \\2", linesDRIVERS)
        condi <- (!grepl("=", linesDRIVERS)) | grepl(")$", linesDRIVERS2)
        linesDRIVERS2[condi] <- paste0(linesDRIVERS2[condi], ";")
        linesDRIVERS2

        for (ii in seq_along(parsDict)){
          qq_dot <- names(parsDict)[[ii]]
          qq_snake <- parsDict[[ii]]
          linesDRIVERS2 <- gsub(qq_dot, qq_snake, linesDRIVERS2)
        }

        co.mo <- c(co.mo, linesDRIVERS2)
      }


      ################################################## ODEs
      {
        co.mo <- c(co.mo, "\n//equation")
        co.mo <- c(co.mo, paste0("\n", commenter, " ODEs"))
        linesODE <- extractSourceCodeHighlight(modelId
                                               , toCiteStart = "# OdeStart"
                                               , showComments = F
                                               , keepLatexComments = F
                                               , keepRoxygen = F
                                               , addLineNumbers = F)

        linesODE2 <- sub("(.*)d_(.*).*<-(.*)", "  der(\\2) = \\3;", linesODE)
        co.mo <- c(co.mo, linesODE2)
      }



      {
        co.mo <- c(co.mo, paste0("\n", commenter, " Aux equations"))
        {
          linesAUX <- extractSourceCodeHighlight(modelId
                                                 , toCiteStart = "# auxStart"
                                                 , showComments = F
                                                 , keepLatexComments = F
                                                 , keepRoxygen = F
                                                 , addLineNumbers = F)

          # ww <- base::parse(text = paste(linesAUX, collapse = "\n"))

          linesAUX <- trimws(linesAUX)
          linesAUX <- sub("#.*$", "", linesAUX)
          .linesAUX <<- linesAUX

          linesAUX0 <- linesAUX#[!grepl("(\\..*)|(log_)|(\\})|(\\{)|(else)]", linesAUX)]
          linesAUX0 <- linesAUX0[ sapply(linesAUX0, nchar) > 0 ]
          .linesAUX0 <<- linesAUX0

          ww <- lapply(lapply(linesAUX0, unBoxExpression), deparse, width.cutoff = 500L)
          ww <- unlist(ww)
          .ww <<- ww
          hasEqual <- grep("=", ww, value = TRUE)
          if (length(hasEqual)){
            log_warn("'=' in Aux equations: {hasEqual}")
          }
          .hasEqual <<- hasEqual

          ww <- sub("(.*)<-(.*)", "  \\1 = \\2;", ww)
          ww <- sub("^  \"", "  ", ww)
          ww <- sub("\";$", ";", ww)
          ww <- sub(" {2,}", " ", ww)

          lhs <- sub("(.*?)(=.*)", "\\1", ww)
          lhs <- trimws(lhs[!grepl('"', lhs)])
          .lhs <<- lhs
          repeats <- table(.lhs)[table(.lhs) > 1]
          if (length(repeats)){
            log_warn("found repeated lhs in Aux equations: {repeats}")
          }
          ww
        }
        # linesAUX2 <- linesAUX[!grepl("(log_)|(if\\()|(if \\()|(else)|(\\})|(\\{)]", linesAUX)]
        co.mo <- c(co.mo, ww)
      }




      {
        FUN <- paste0(modelId, "PostProc")
        linesPP <- extractSourceCodeHighlight(FUN
                                              , toCiteStart = "# PostProcStart"
                                              , showComments = F
                                              , keepLatexComments = F
                                              , keepRoxygen = F
                                              , addLineNumbers = F)
        head(linesPP, 12)
        linesPP <- sub("if \\(doPostProcessingExtras).", "", linesPP)
        linesPP <- sub(", by = byGroup", "", linesPP)
        linesPP <- sub(":=", "=", linesPP)
        linesPP <- sub(" *\\]$", "", linesPP)
        linesPP <- sub("\\];", ";", linesPP)
        linesPP <- linesPP[!grepl("dtw\\[[a-zA-Z]", linesPP)]
        linesPP <- sub("(.*)dtw\\[,(.*)", "\\2;", linesPP)
        linesPP <- sub(" {2,}", " ", linesPP)
        linesPP <- linesPP[nchar(linesPP) > 0]
        linesPP

        # apply dicts . --> _
        # for (ii in seq_along(parsDict)){
        #   qq_dot <- names(parsDict)[[ii]]
        #   qq_snake <- parsDict[[ii]]
        #   linesPP <- gsub(qq_dot, qq_snake, linesPP)
        # }
        # for (ii in seq_along(drivDict)){
        #   qq_dot <- names(drivDict)[[ii]]
        #   qq_snake <- drivDict[[ii]]
        #   linesPP <- gsub(qq_dot, qq_snake, linesPP)
        # }


        lhspp <- sub("(.*?)(=.*)", "\\1", linesPP)
        lhspp <- trimws(lhspp[!grepl('"', lhspp)])
        .lhspp <<- lhspp
        repeats <- table(c(.lhs, .lhspp))[table(c(.lhs, .lhspp)) > 1]
        if (length(repeats)){
          log_warn("found repeated lhs in c(Aux, PP) equations: {repeats}")
        }

        hasEqualPP <- grep("=", linesPP, value = TRUE)
        if (length(hasEqualPP)){
          log_debug("'=' in equations: {hasEqualPP}")
        }
        .hasEqualPP <<- hasEqualPP



        # ww <- lapply(lapply(linesPP, unBoxExpression), deparse, 300)
        # ww <- unlist(ww)
        co.mo <- c(co.mo, linesPP)
      }

    }

    if (lang == "modelica"){
      co.mo <- c(co.mo, "\nend Tomato;")
    }
  }



  # apply dicts . --> _
  # for (ii in seq_along(parsDict)){
  #   qq_dot <- names(parsDict)[[ii]]
  #   qq_snake <- parsDict[[ii]]
  #   co.mo <- gsub(qq_dot, qq_snake, co.mo)
  # }
  # for (ii in seq_along(drivDict)){
  #   qq_dot <- names(drivDict)[[ii]]
  #   qq_snake <- drivDict[[ii]]
  #   co.mo <- gsub(qq_dot, qq_snake, co.mo)
  # }

  .co.mo <<- co.mo
  # co.mo <- .co.mo


  ww <- all.vars(body(get(FUNODE)))
  ww <- union(ww, all.vars(body(get(paste0(FUNODE, "PostProc")))))
  ww <- union(ww, all.vars(body(get(paste0(FUNODE, "Parms")))))
  ww <- union(ww, all.vars(body(get(FUNDAG))))
  ww <- ww[!grepl("^\\.", ww)]
  # ww
  hasP <- grep("\\.", ww, value = T)
  d2sDict <- setNames(hasP, gsub("\\.", "_", hasP))
  # grep("\\.calc", ww, value = T)
  # grep("\\.calc", co.mo, value = T)

  for (ii in seq_along(d2sDict)){
    qq_snake <- names(d2sDict)[[ii]]
    qq_dot <- d2sDict[[ii]]
    co.mo <- gsub(qq_dot, qq_snake, co.mo)
  }
  grep("\\.", co.mo, value = T)


  co.mo <- gsub("paste0\\(", "string(", co.mo)
  # co.mo <- gsub("pmax\\(", "max(", co.mo)
  # co.mo <- gsub("pmin\\(", "min(", co.mo)
  co.mo <- handleLongExpressions(co.mo)
  # co.mo <- linesPP[!grepl("<<", co.mo, fixed = TRUE)]
  if (length(ffmo)){
    ffext <- ifelse(lang == "modelica", ".mo", ".jl")
    writeLines(text = paste(co.mo, collapse = "\n")
               , paste0(ffmo, ffext))
    dd <- copy(SIMS$drivers)
    for (ii in seq_along(d2sDict)){
      qq_snake <- names(d2sDict)[[ii]]
      qq_dot <- d2sDict[[ii]]
      names(dd) <- gsub(qq_dot, qq_snake, names(dd))
    }
    if (lang == "julia"){
      names(dd) <- gsub("\\.", "_", names(dd))
    }
    write.csv(dd
              , file = paste0(ffmo, "_drivers.csv"), row.names = FALSE)
  }
  co.mo
}



#' unBoxExpression
#' @examples \dontrun{
#'   unBoxExpression(ex = expression({r=9;w=8}))
#' }
#' @export
unBoxExpression <- function(ex) {
  qq <- attr(ex, "srcref")
  if (is.null(qq)) return(ex)
  exVector <- unlist(lapply(qq, as.character))
  # str(exVector[1])
  if (exVector[1] == "{") {
    exVector <- exVector[-1]
    res <- base::parse(text = paste0(exVector, collapse = "\n"))
  } else {
    res <- ex
  }
  res
}


handleLongExpressions <- function(co.mo){
  res <- co.mo[1]
  stackpresent <- FALSE
  follower <- sapply(co.mo, \(xx) substr(xx, 1, 1) == '"')
  co.mo
  ii <- 2
  for (ii in seq(2, length(co.mo)-1)){
    xx <- co.mo[ii]
    xx <- gsub('"', "", xx)
    xx <- gsub(' {2,}', " ", xx)

    if (follower[ii]) {

      if (!follower[ii-1]) {
        stack <- sub(";$", "", co.mo[ii-1])
      }
      stack <- paste0(stack, xx)
      stackpresent <- TRUE

    } else {

      if (stackpresent) {
        stack <- paste0(stack, ";")
        res <- c(res, stack)
      }
      stackpresent <- FALSE

    }

    if (!follower[ii+1]) {
      res <- c(res, xx)
    }

  }
  res <- c(res, co.mo[length(co.mo)])
  xx <- gsub('^ {2,}', " ", xx)
  res
}






# # length(ww)
# # str(ww[[1]])
# # str(ww[[2]])
#
# qq <- attr(ww[[1]], "srcref")
# ex <- ww[[1]]
# lapply(ww[1], unBoxExpression)
# lapply(ww[2], unBoxExpression)
# lapply(ww, unBoxExpression)
# lapply(lapply(ww, unBoxExpression), deparse_to_one_line)


if (F){
  x_dr <- names(SIMS$drivers)
  x_cr <- unique(as.character(SIMS$cropLong$processName))
  # used <- c(x_dr, x_cr)
  modelId <- SIMS$modelId
  FUN_ODE <- get(modelId)
  FUN_PP <- get(paste0(modelId, "PostProc"))
  uniqueODE <- unique(all.vars(body(FUN_ODE)))
  uniquePP <- unique(all.vars(body(FUN_PP)))
  uniquePAR <- names(SIMS$usedParms)
  takeOut <- c("Time", "T", "F", "variable.factor", "res"
               , "temp.night", "maturityDegreeDays"
               , "toReturn", "dtw", "cropLong", "bygroup")
  takeOut <- union(takeOut
                   , grep("\\.", c(uniqueODE, uniquePP), value = TRUE))
  uniqueODE <- setdiff(uniqueODE, takeOut)
  uniquePP <- setdiff(uniquePP, takeOut)

  uniqueODE2 <- setdiff(uniqueODE, c(x_dr
                                     #, x_cr
  ))
  uniquePP2 <- setdiff(uniquePP, c(x_dr, x_cr))

  vvOrder <- union(uniqueODE2, uniquePP2)
  vvOrder <- unique(vvOrder)
  vvOrder
  defOrder <- union(intersect(x_dr, vvOrder)
                    , intersect(vvOrder, x_cr))
  defOrder
  mget(ls())
}
