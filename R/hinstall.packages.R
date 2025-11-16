#' hinstall.packages
#' @export
hinstall.packages <- function(...){
  print(getOption("repos"))
  pp <- file.choose()
  install.packages(pp, type = "source", repos = NULL)
}



#' zip_install.packages
#' @export
zip_install.packages <- hinstall.packages
