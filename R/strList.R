#' hstr1
#' @export
hstr <- function(obj, max.level = 1){ 
  str(obj, max.level = max.level)
}


#' strList
#' describe elements of a list
#' @examples \dontrun{
#'   # bad example
#'   #SIMS <- runFun()
#'   #sapply(SIMS, class)
#'   #ww <- strList(SIMS)
#' }
#' @export
strList <- function(obj, 
                    pf.s = list(class = class
                                # ,isS4 = isS4
                                , typeof = typeof
                                , length = length
                                , object.size2 = object.size2
                    )
                    , asNum = FALSE
){
  if (inherits(obj, "call")) {
    # if (inherits(as.list(obj$CALL)[[1]], "function")) 
    return(NULL)
  }
  objNames <- names(obj)
  if (is.null(objNames)) objNames <- seq_along(obj)
  if (asNum){
    object.size2 <- function(x) as.numeric(object.size(x))
  } else {
    object.size2 <- function(x) hprettyNum(as.numeric(object.size(x)))
  }
  res <- data.frame()
  sapply(obj, class)
  # el <- 3
  for (el in seq_along(obj)){
    elem <- obj[[el]]
    for (pf in seq_along(pf.s)){
      nm <- names(pf.s)[pf]
      # log_trace("nm {nm}")
      # length(elem)
      rr <- do.call(pf.s[[pf]], list(x = elem))
      if (inherits(elem, "symbol")) rr <- NA
      if (inherits(elem, "pairlist")) rr <- NA
      if (inherits(elem, "{")) rr <- NA
      if (inherits(elem, "call")) rr <- NA
      if (length(rr) > 1) {
        rr <- paste(rr , collapse="_")
      }
      res[objNames[el], names(pf.s)[pf]] <- rr
    }
  }
  resdt <- as.data.table(res, keep.rownames = TRUE)
  resdt
}


#' object.size2
#' 
#' as.numeric(object.size(x))
#' @export
object.size2 <- function(x) {
  # if (mode(x) == "S4"){
  as.numeric(object.size(x))
  # } else {
  #   as.numeric(object.size(as.list(x)))
  # }
}


#' lsFunEnv
#' ls() the Environment of a function
#' @export
lsFunEnv <- function(fun) ls(environment(fun), all.names = TRUE)


#' strFunEnv
#' 
#' Describe each object in the Environment of a function
#' @export
strFunEnv <- function(fun, focus="x"
                      , pf.s=c("class", "typeof", "length", "object.size2")) {
  object.size2 <- function(x) as.numeric(object.size(x))
  ll <- lsFunEnv(fun)
  if(focus %in% ll){
    focus <- get(focus, environment(fun))
    if(!is.function(focus)) return(focus)
    fun <- get(focus)
    ll <- lsFunEnv(fun)
  }
  # print(ll)
  res <- list()
  for (pf in pf.s){
    res[[pf]] <- sapply(ll, function(x) {
      # message(x)
      do.call(get(pf), list(get(x, environment(fun)) ) )
    })
    # print(res[[pf]])
  }
  res <- as.data.frame(res)
  res
}
