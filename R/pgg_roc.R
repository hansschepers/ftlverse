#' pgg.roc
#'
#' @author Hans.Schepers@@gmail.com
# @import ROCR
#' @export
pgg.roc <- function(df, yoi="yield", pred="predicted", sdsqueeze=1, sdshow=1, 
                    doCurve=T, usepgg=TRUE, ...){
  df <- as.data.frame(df)
  ytrue <- df[,yoi,  drop=TRUE]
  ypred <- df[,pred, drop=TRUE]
  thres <- mean(ytrue, na.rm=T)
  ysd   <- sd(ytrue, na.rm=T)
  ytruef <- ytrue > thres 
  ytruef <- factor(ytruef, levels=c(FALSE, TRUE), ordered=TRUE)
  list.rocr <- list(predictions = ypred, labels=ytruef)
  
  # library(ROCR)
  predic <- prediction( list.rocr$predictions, list.rocr$labels)
  perf <- performance(predic,"tpr","fpr")
  (pred.auc <- unlist(performance(predic, "auc")@y.values))
  # (pred.prbe <- unlist(performance(predic, "prbe")@y.values))
  # (pred.mxe <- unlist(performance(predic, "mxe")@y.values))
  p.roc <- "noCurve"
  dfg.rocr <- "noCurve"
  if (doCurve){
    # str(perf)
    dfg.rocr <- data.frame(x=perf@x.values, y=perf@y.values)
    dfg.rocr <- data.frame(x=perf@x.values, y=perf@y.values, thres=perf@alpha.values)
    names(dfg.rocr) <- c(perf@x.name, perf@y.name, perf@alpha.name)
    # dfg.rocr$status <- slideTitle
    xlab <- paste0(perf@x.name, ", (1 - Specificity)")
    ylab <- paste0(perf@y.name,      ", Sensitivity")
    roc.title <- paste0("AUC = ", round(pred.auc, 3))
    roc.title <- paste0("ROC curve, threshold used = ", round(thres, 0))
    thresXfun <- approxfun(dfg.rocr[,c(perf@alpha.name, perf@x.name)], rule = 2)
    thresYfun <- approxfun(dfg.rocr[,c(perf@alpha.name, perf@y.name)], rule = 2)
    cutoff.s <- thres + (ysd / sdsqueeze) * -sdshow:sdshow 
    xThres <- thresXfun(cutoff.s)
    yThres <- thresYfun(cutoff.s)
    dfg.cutoff <- data.frame(x=xThres, y=yThres, cutoff=cutoff.s)
    p.roc <- list("NB"="see attributes")
    dfg.rocr <<- dfg.rocr
    names(dfg.rocr) <- make.names2(names(dfg.rocr))
    if (usepgg & exists("pgg", mode="function")){
      p.roc <- pgg(dfg.rocr, geom="line", doplot=F, allowppt=F) # foi="status", 
      p.roc <- pgg(dfg=dfg.cutoff, p=p.roc, label="cutoff", mega=T
                  , xlab=xlab, ylab=ylab, title=roc.title, legend="none", ...)
    } else {
      # png("test.png")
      plot(dfg.rocr[,1], dfg.rocr[,2], type="l", xlab=xlab, ylab=ylab, main=roc.title)
      points(xThres, yThres, col="red", cex=3, pch=19)
      # dev.off()
      # vppt(ffpng="test.png")
    }
  } 
  return(
    structure(p.roc, "data" = dfg.rocr, "predic" = predic, "auc"=pred.auc)#, "prbe"=pred.prbe)
  )
}

