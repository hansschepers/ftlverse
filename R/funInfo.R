#' funInfo
#' @examples \dontrun{
#' # used to have unused synonym gitFun()
#'   funInfo("hmean")
#'   funInfo("hmean", nnn = 2)
#'   funInfo("DTexpr")
#'   funInfo("DTexpr", do = "readLines")
#'   funInfo("mapExternalProcessNames", do = "browseURL")
#' }
#' @export
funInfo <- function(fun = "DTexpr"
                   , nnn = 1
                   , do = c("all", "print", "browseURL", "readLines")[1]
                   ){
  
  # base <- "https://github.platforms.engineering/DigitalHorticulture/"
  base <- "https://github.com/bayer-int/"
  # require(pack, character.only = TRUE)
  
  includeFileContent <- any(c("all", "readLines") %in% do)
  
  res <- functionInfo(fun, includeFileContent  = includeFileContent , nnn = nnn)
  # res
  ffr <- basename(res$filenameEdited)
  # print(funInfo2(pack))
  pack <- sub("package:", "", res$found)
  gitPath <- paste0(base, pack, "/blob/master/R/", ffr)
  if ("print" %in% do) res <- gitPath
  if ("readLines" %in% do) res <- res$fileContent
  # if ("browseURL" %in% do) browseURL(gitPath)
  res
}
