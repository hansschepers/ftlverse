#' fitRAD
#' 
#' @export
fitRAD <- function(dtw){
  dtw <- copy(dtw)
  dtw[, weekno := week(dateTime)]
  # require(e1071)
  radModel <- e1071::svm(light.sum.total.day ~ weekno
                         , na.action = na.exclude
                         , data = dtw)
  .radModel <<- radModel
  radModel
}

#' fitTEMP
#' 
#' @export
fitTEMP <- function(dtw, modelsGiven
                    , type = c("lm", "svm")[2]
                    , tryOUTEMP = FALSE
){
  dtw <- copy(dtw)
  # use modelsGiven$radModel to fill light/rad
  # dtw[is.na(light.sum.total.day)]
  dtw[is.na(light.sum.total.day)
      , light.sum.total.day := predict(modelsGiven$radModel
                                       , na.action = na.exclude
                                       , newdata = data.table(weekno = week(dateTime)))]
  # dtw[, weekno := week(dateTime)]
  formu = temp24hr ~ light.sum.total.day
  if (tryOUTEMP & "OUTEMP" %in% names(dtw)){
    formu = temp24hr ~ light.sum.total.day + OUTEMP
  }
  # print(formu)
  tempModel <- switch(type,
                      svm = e1071::svm(formu, na.action = na.exclude, data = dtw)
                      ,             lm(formu, na.action = na.exclude, data = dtw)
  )
  .tempModel <<- tempModel
  tempModel
}
