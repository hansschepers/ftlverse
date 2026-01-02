preAbbr.s <- list(aph = "C:/Users/Lenovo/OneDrive/Documents/APH/aphDHfromRPI/aphDH"
                  , r = "C:/Users/Lenovo/Documents/R"
                  , m = "C:/Users/Lenovo/Documents/R/DEBsens"
                  , amp = "C:/Users/Lenovo/Documents/R/DEBsens/inst/AmP"
                  , mdeb = "C:/User/Lenovo/AppData/Local/matlabPackages/DEBtool_M"
                  , mdeblib = "C:/User/Lenovo/AppData/Local/matlabPackages/DEBtool_M/lib"
                  , ftl = "C:/Users/Lenovo/Documents/R/ftl"
                  , ai = "C:/Users/Lenovo/Documents/R/AI_test")


#' grepCode2
#' @examples \dontrun{
#'  library(logger) ; library(data.table)
#'  grepCode2("anthropic")
#'  grepCode2("runFun")
#'  grepCode2("getPcaScenarioDT")
#'  grepCode2("wrangle")
#'  grepCode2("base")
#'  grepCode2("output_struct", Rdir = c("m"))
#'  grepCode2("std", Rdir = c("mdeb"))
#'  #alldeb <- sapply(.ffr.s, readLines)
#' }
#' @export
grepCode2 <- function(rege
                      , Rdir = c("r", "aph", "ftl", "ai", "m", "mdeb", "amp")[1]
                      , patt = "\\.[rR]$"
                      , ...
                      # , preAbbr.s = preAbbr.s
){
  if (any(tolower(Rdir) %in% c("m", "amp", "mdeb"))) {
    patt <- "\\.m"
  }
  RdirOrig <- Rdir
  for (ss in seq_along(Rdir)){
    if (tolower(Rdir[ss]) %in% names(preAbbr.s)){
      Rdir[ss] <- preAbbr.s[[tolower(Rdir[ss])]]
    }
  }
  res <- list()
  ii <- 0
  Rdirii <- Rdir[1]
  for (Rdirii in Rdir){
    ii <- ii + 1
    cat("searching ", paste(rege, collapse = " & \n  "), " in ", RdirOrig[ii], "(", Rdir[ii], ") patt =", patt, " ....\n")
    dd <- grepCode(rege = rege
                   , Rdir = Rdirii
                   , patt = patt
                   # , ...
    )
    dd[, from := RdirOrig[ii] ]
    res[[ii]] <- dd
  }
  require(data.table)
  res <- rbindlist(res, fill = TRUE)
  .gg <<- res
  res
}


#' fe2
#' @export
fe2 <- function (row
                 , gg = .gg
                 # , preAbbr.s = preAbbr.s
) {
  preAbbr <- "../"
  if ("from" %in% names(gg)){
    from <- gg[row, from ]
    preAbbr <- unname(unlist(preAbbr.s[ from ]))
    print(preAbbr)
  }
  ff <- paste0(preAbbr, gg[row, file])
  print(ff)
  file.edit(ff)
}


#' grepMatlab
#' @export
grepMatlab <- function(rege, Rdir = c("m", "mdeb", "amp"), patt = "\\.m$", ...){
  grepCode2(rege = rege, Rdir = Rdir, patt = patt, ...)
}

