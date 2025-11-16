#' autoAbbreviate
#' @examples \dontrun{
#'   autoAbbreviate("yield2brix")
#'   autoAbbreviate("yield2brix", keepExtention = FALSE)
#'   autoAbbreviate("yield2brix", sep = 4, keepExtention = FALSE)
#'   autoAbbreviate("yield4brix", sep = 4, keepExtention = FALSE)
#'   autoAbbreviate("yield2brix..cu")
#'   autoAbbreviate("yield2brix.cu", keepExtention = FALSE)
#'   autoAbbreviate("yield2brix.cu.ce")
#'   autoAbbreviate("yield2brix.cu.ce", lengthLeft = 2)
#'   autoAbbreviate("yield2brix.cu.ce", sep = ".")
#' }
#' @export
autoAbbreviate <- function(xx
                           , sep = "2"
                           , lengthLeft = 1
                           , lengthRight = lengthLeft
                           , keepExtention = TRUE
){
  # regex <- "(.{1})(.*)2(.{1})(.*)\\.(.*)", "\\1_\\3.\\5"
  
  pattern <- paste0("(.{",lengthLeft,"})(.*)",sep,"(.{",lengthRight,"})(.*)")
  if (keepExtention) pattern <- paste0(pattern, "\\.([a-zA-Z0-9]*)")
  pattern <- paste0(pattern, "$")
  pattern
  repl <- paste0("\\1",sep,"\\3")
  if (keepExtention) repl <- paste0(repl, ".\\5")
  sub(pattern, repl, xx)
}


#' autoSymbolize
#' @examples \dontrun{
#'   sub("(.{1})(.*)2(.{1})(.*)\\.(.*)", "\\1_\\3.\\5", "asd2fgh.sa")
#'   autoAbbreviate("asdfgh.sa")
#'   autoAbbreviate("asd2fgh.satu")
#'   autoSymbolize("fruit2plant.satu")
#'   autoSymbolize("sink.plant")
#'   autoSymbolize("fruit2plant.ww.satu")
#'   autoSymbolize("fruit2plant.ww.satu", regex = ".satu", repl = "_{\\mbox{init}}", abb = 2, lengthLeft = 2 )
#'   autoSymbolize("fruitplant.ww.satu", regex = ".satu", repl = "_{\\mbox{init}}", abb = 2, lengthLeft = 2 )
#' }
#' 
#' @export
autoSymbolize <- function(x = c("Radiation.satu", "CO2.satu", "nosatu")
                          , regex = ".satu$"
                          , repl
                          , treat2 = TRUE
                          , abb = 1
                          , fixed = FALSE
                          , ...
){
  if (missing(repl)){
    repl <- paste0("_{\\mbox{"
                   , sub("$", "", sub(".", "", regex), fixed = TRUE)
                   , "}}")
    # str(repl)
  }
  ff <- grepl(regex, x)
  res <- setNames(x, x)
  xx <- x[ff]
  # str(xx)
  if (treat2){
    wbase <- autoAbbreviate(xx, ...)
    # wbase <- sub("(.{1})(.*)2(.{1})(.*)\\.(.*)", "\\1_\\3.\\5", xx)
    # print(wbase)
  }
  wbase <- sub(regex, repl, wbase, fixed = fixed)
  res[ff] <- setNames(wbase, xx)
  res[ff]
  # res
}
