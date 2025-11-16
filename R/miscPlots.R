# PCA
if (F)
{
  # DTcyc21 <- readObject(storeCustom, startDataPath)
  # startDataPath <- "1005/plot_clean/kbb_tuin3_1053/kbb_21_tuin_3.rds"
  # DTcyc21 <- readObject(storeCustom, startDataPath)
  DTw <- lastYield2(DTcyc)
  outpca <- aphPca(DTw
                   , foi = foip
                   , labelCol = "wk"
                   , mega = TRUE, psize = 12, legend = "none"
                   , geom = "pointline", doDebug = TRUE)
  # names(attributes(outpca))
  # dtmp <- attr(outpca, "dfg.inclscores")
  # ww <- ggplot_build(pppca)
  # xrange <- ww$layout$panel_params[[1]]$x.range
  # yrange <- ww$layout$panel_params[[1]]$y.range
  attr(outpca, "correlmatrix")
}


# zoom Plot  on last Yield (with weeknumbers and labels) ----
if(F)
{
  names(dtwClean)
  print(isoweek(Sys.Date()))
  dtwClean[, tail(.SD, 3), by = plot_syn]
  # casts, and selects vars
  dtLast <- lastYield2(aphMelt(dtwClean)[plot_syn == "k_21_m3"], n = 4)
  # ppggs(aphMelt(dtLast))
  tmp <- tail(dtLast, 14)
  tmp <- aphMelt(tmp)
  tmp[, wk := lubridate::isoweek(dateTime)]
  tmp[, label := round(value, 1)]
  yois <- "yield"
  p <- pggs(tmp[processName %in% yois]
            , psize = 7
            , fsize = 14
            , label = "wk", mega = TRUE
            , labelSize = 4
            , geom = "pointline"
            , chunkTitle = "last few months with weeknumbers and value labels")
  p <- addText(p = p
               , dfg = tmp[processName %in% yois]
               , color = "red"
               , size=4, fontface=3, angle=0, hjust=-0.9, vjust=0, labelRepel=0
  )
  vrmd("add", p = p, chunkId = "last few months with weeknumbers and value labels")
  p
}

# the global fit per month and week (pattern that prophet picks ups) ----
if(F)
{
  kk <- c("OUTEMP", "RAD", "yield", "setting", "harvest", "afw")
  kk <- c(bycols, doi, kk)
  dtwClean2 <- copy(dtwClean)
  
  pggs(aphMelt(dtwClean2[, ..kk]))
  dtwClean2[, wk := as.character(isoweek(dateTime))]
  dtwClean2[, mon := as.character(month(dateTime))]
  dtwClean2 <- dtwClean2[wk %in% 12:48]
  # dtwClean2 <- dtwClean2[!grepl("m[12]", plot_syn)]
  dtwClean2[, yield.mon := hmean(yield), by = mon]
  dtwClean2[, yield.wk := hmean(yield), by = wk]
  dtwClean2[, yield.mon := predict(lm(yield ~ mon, na.action = na.exclude)
                                   , na.action = na.exclude
                                   , newdata = .SD)]
  dtwClean2[, yield.wk := predict(lm(yield ~ wk, na.action = na.exclude)
                                  , na.action = na.exclude
                                  , newdata = .SD)]
  kk <- c("yield", "yield.mon", "yield.wk")
  kk <- c(bycols, doi, kk)
  dd <- aphMelt(dtwClean2[, ..kk])
  dd <- dd[!is.na(value)]
  ppggs(dd, facetcols = 1, fsize = 14, chunkTitle = "common pattern per month and week")
}



# after running the app once (or only it's 'global.R')
# g.DTcyc[, .N, by = .(cycle_syn, plot_syn)]
# # .DTcyc
# dd <- g.DTcyc[processName %in% yoisWeek]
# dd
# ppggs(dd)
# DTfocused <- cycleFocus(dd)
# ppggs(DTfocused)
# DTdiagn <- cycleDiagnostics(DTfocused)
# DTdiagn
# 
# monthsOK <- range2vec(selected$monthsOK)
# {
#   DTweek <- cycle2week(DT = NULL #DTcyc
#                        , thr = 100
#                        , DTfocused = DTfocused
#                        , DTdiagn = DTdiagn)
#   DTweek <- DTweek[, (c("field_syn", "plot_syn")) := NULL]
#   DTweek <- DTweek[, dateTime := toMonday(dateTime)]
#   # .DTweek
#   # table(DTweek[, wday(dateTime)])
#   # DTweek[, hsummary(dateTime), by = .(processName, cycle_syn)]
#   
#   yoisNice2Have
#   DTweek <- DTweek[processName %in% yoisNice2Have]
#   DTweek[, cycle_syn := sub("(.*)([0-9][0-9])(.*)", "cycle_\\2", cycle_syn)]
#   # monthsOK <- 1:10
#   DTweek <- DTweek[month(dateTime) %in% monthsOK]
#   DTweek
#   ppggs(DTweek) <- 
# }
# .DTweek <<- copy(DTweek)
# DTweek[, percentMonday(dateTime)]
