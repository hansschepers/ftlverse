#' ftlview
#' @export
ftlview <- function(funnm = "pggs"
                    , dir = "ftl/src"
                    , base = "C:/Users/Lenovo/Documents/R"){
  if (!is.character(funnm)){
    funnm <- substitute(funnm)
    str(funnm)
  }
  kk <- grepCode(paste0(funnm, " <- function"), Rdir = file.path(base, dir), pathBlocker = F)
  file.edit(file.path(base, kk[1, file]))
}

