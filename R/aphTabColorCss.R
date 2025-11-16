#' aphTabColorCss
#' @examples \dontrun{
#'   cat(aphTabColorCss("blue"))
#' }
#' @export
aphTabColorCss <- function(col = "green"){
  paste0(
    '\n\n'
    # , '```{css, echo = FALSE}\n'
    , '<style>\n'
    , '  .nav-pills>li>a:hover, .nav-pills>li>a:focus, .nav-pills>li.active>a,     .nav-pills>li.active>a:hover, .nav-pills>li.active>a:focus{\n'
    , '     background-color: ', col,';\n'
    , '    }\n'
    # ,'```\n\n'
    , '</style>\n\n'
    )
}
