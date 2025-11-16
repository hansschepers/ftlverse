#' inideb
#'
#' @export
inideb <- function(posi="APHdebug", asfun=c("no", "both", "only")[1]){
  if (asfun %in% c("no", "both")) {
    while(posi %in% search()) detach(posi, character.only=TRUE)
    attach(NULL, name=posi)
  }
  if (asfun %in% c("both", "only")){
    posfun <- posi
    # posfun <- paste0("Shiny_", posi)
    while(posfun %in% search()) detach(posfun, character.only=TRUE)
    attach(NULL, name=posfun)
  }
}



#' stopdeb
#'
#' @export
stopdeb <- function(posi="APHdebug", asfun=c("no", "both", "only")[1]){
  if (asfun %in% c("no", "both")) {
    while(posi %in% search()) detach(posi, character.only=TRUE)
  }
  if (asfun %in% c("both", "only")){
    posfun <- posi
    # posfun <- paste0("Shiny_", posi)
    while(posfun %in% search()) detach(posfun, character.only=TRUE)
  }
}



#' todeb
#'
# @importFrom rlang enquo get_expr
#' @export
todeb <- function(obj
                  , lab=NULL
                  , pfx=NULL
                  , mask=TRUE
                  , pos="APHdebug"
                  , asfun=c("no", "both", "only")[1]
                  , ini=FALSE){
  # attachArgs("todeb")
  debname <- deparse(substitute(obj))
  if (asfun %in% c("no", "both")) {
    attr(obj, "added") <- Sys.time()
    if (!is.null(lab)) debname <- as.character(lab[1])
    if (!is.null(pfx)) debname <- paste0(pfx, debname)
    if (mask) debname <- paste0(pos, "_", debname)
    if (ini | (!pos %in% search()) ) inideb(posi=pos, asfun=asfun)
    assign(x=debname, value=obj,       pos=pos)
  }
  if (asfun %in% c("both", "only")) {
    # formals(todeb)
    message("making into function")
    # obf.asfun <- function(x) eval(substitute(obj))
    sss <- Sys.time()
    obj.asfun <- function(x) structure(obj, "added"= sss)
    if (!is.null(lab)) debname <- as.character(lab[1])
    if (!is.null(pfx)) debname <- paste0(pfx, debname)
    # if (mask) debname <- paste0(pos, "_", debname)
    posfun <- pos
    # posfun <- paste0("Shiny_", pos)
    if (ini | (!posfun %in% search()) ) inideb(posi=posfun, asfun=asfun)
    search()
    assign(x=debname, value=obj.asfun, pos=posfun)
  }
}



#' hreturn
#'
#' @export
hreturn <- function(obj, ...){
  todeb(obj)
  return(obj)
}



#' getdeb
#'
#' @export
getdeb <- function(what, pos="APHdebug", pfx=NULL, mask=TRUE){
  if (mask){
    base::get(paste0(pos,"_", pfx, what), pos=pos, inherits=FALSE)
  } else {
    base::get(paste0(         pfx, what), pos=pos, inherits=FALSE)
  }
}



#' lideb
#'
# @importFrom purrr map_dfr map_lgl
# @importFrom tibble lst
#' @export
lideb <- function(pos = "APHdebug"
                  , showAttr = FALSE
                  , pfx = NULL
                  , mask = TRUE
                  , df.only = TRUE) {
  li <- ls(pos=pos)
  if (showAttr) {
    # li <- purrr::map_dfr(li, \(x)
    #                      tibble::lst(what=x

    li <- lapply(li, \(x)
      list(what = x
           , added = attr(base::get(x, pos=pos, inherits=FALSE), "added")
           , elapsed = as.numeric(Sys.time() - added))
      )
  } else {
    if (df.only){
      # x <- li[1]
      li <- li[
        lapply(li, \(x) {"data.frame" %in% class(getdeb(x, pos=pos, pfx=pfx, mask=F))})
      ]
    } else {
      # li #<- gsub(paste0(pos, "_"), "", li)
    }
  }
  li
}
