#' hstyle
#' 
#' @examples \dontrun{
#'   hstyle()
#'   shiny::wellPanel(style = hstyle(), "hello")
#' }
#' 
#' @export
hstyle <- function(background = "yellow" # "#ffffaa"
                   , color = "black"
                   , align = NULL # "center"
                   , margin_top = "0px"
                   , margin_bottom = "0px"
                   , margin_left = "0px"
                   , margin_right = "0px"
                   , pad = "0px"
                   , height = NULL # "300px"
                   , width = NULL # "300px"
){
  s <- character(0)
  s <- c(s, paste0("padding:", pad, " ", pad))
  if (!is.null(height)) s <- c(s, paste0("height:", height))
  if (!is.null(width)) s <- c(s, paste0("width:", width))
  if (!is.null(background)) s <- c(s, paste0("background:", background))
  s <- c(s, paste0("color:", color))
  if (!is.null(height)) s <- c(s, paste0("text-align:", align))
  s <- c(s, paste0("margin-top:", margin_top))
  s <- c(s, paste0("margin-bottom:", margin_bottom))
  s <- c(s, paste0("margin-left:", margin_left))
  s <- c(s, paste0("margin-right:", margin_right))
  
  style = paste0(paste(s, collapse = "; "), "; ")
  style
}

#' hdiv
#' @examples \dontrun{
#'   hdiv(plotOutput("p_plot"))
#' }
#' @export
hdiv <- function(...
                 , style = hstyle()
                 ){
  htmltools::div(style = style, ...)
}

#' hspan
#' 
#' @export
hspan <- function(...
                  , style = hstyle()
                  , .noWS = c("before", "after", "outside"
                                 , "after-begin", "before-end", "inside")[c(3, 6)]
                  ){
  htmltools::span(..., style = hstyle(), .noWS = .noWS)
}

#' hwellPanel
#' @examples \dontrun{
#'   hwellPanel("testid")
#'   hwellPanel("testid", width = "50%", height = "200px")
#' }
#' @export
hwellPanel <- function(...
                       , style = hstyle()
                       , .noWS = c("before", "after", "outside"
                                   , "after-begin", "before-end", "inside")[c(3, 6)]
){
  shiny::wellPanel(
    htmltools::span(..., style = style, .noWS = .noWS)
  )
}


#' hplotOutput
#' @examples \dontrun{
#'   hplotOutput("testid")
#'   hplotOutput("testid", inline = TRUE)
#'   hplotOutput("testid", width = "50%", height = "200px")
#' }
#' @export
hplotOutput <- function(  outputId,
                          width = "100%",
                          height = "400px",
                          click = NULL,
                          dblclick = NULL,
                          hover = NULL,
                          brush = NULL,
                          inline = FALSE
                          , ...){
  span(style = hstyle(...), plotOutput(outputId = outputId
                                       , width = width
                                       , height = height
                                       , inline = inline)
       )
}

#' htextOutput
#' 
#' @export
htextOutput <- function(...){
  textOutput(..., inline = TRUE)
}
