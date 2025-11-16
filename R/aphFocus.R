#' aphFocus
#' 
#' @export
aphFocus <- function(dt
                     , theme = c("all", "water", "load", "yield", "raw")
                     , omit = c("sunrise", "resetValue")[1]
                     ){
  data.table::setDT(dt)
  if (isWide(dt)){
    dtm <- aphMelt(dt)
  } else {
    dtm <- copy(dt)
  }
  if ("all" %in% theme){
    return(dtm[])
  }
  
  allYois <- aphVariableLevels(dtm)
  yois <- unlist(lapply(theme, function(x) {
    query <- switch(x
                    , water = "(slab)|(water)|(transpiration)|(irrigation)|(drain)|(WATLM2)"
                    , load = "(growthRate)|(weight)"
                    , raw = "(raw)"
                    , yield = "(yield)|(production)|(afw)"
                    , x
    )
    grep(query, allYois, ignore.case = TRUE, value = TRUE)
  }))
  yois <- unique(yois)
  yois <- setdiff(yois, grep(omit, allYois, ignore.case = TRUE, value = TRUE))
  yois
  dtm[processName %in% yois][]
}
