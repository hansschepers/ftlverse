#' lsa
#' as ls() prints all objects as default
#' @param .only prints only the names starting with '.'
#' @export
lsa <- function(name, pos = 1, envir = as.environment(pos), all.names = TRUE, .only = TRUE) {
  res <- base::ls(name=name, pos=pos, envir=as.environment(pos), all.names = all.names)
  if(.only){
    res <- setdiff(res,
                   base::ls(name=name, pos=pos, envir=as.environment(pos), all.names = FALSE))
  }
  res
}
