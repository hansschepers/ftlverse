# Helper function to create modal dialog with cross to close the window
#
# Author: mvarewyck
###############################################################################

#' modalDialogWithCross2
#' Modal dialog with cross to close the window
#' Extension of the shiny::modalDialog()
#' @examples \dontrun{
#'   library(shiny)
#'   modalDialogWithCross2()
#'   modalDialogWithCross2(size = "s")
#' }
#' @param ... ui main content of the dialog
#' @param addCross boolean, whether to add cross button at right top to close the window
#' the id of button is 'modal-cross'
#' @param title character, title for the dialog
#' @param footer ui, content for footer
#' @param size character, should be one of \code{c("m", "s", "l")}
#' @param easyClose boolean, whether the dialog can be closed by escape button
#' @param fade boolean, when closing the window using fade effect
#' @return modal dialog is being displayed
#'
#' @author mvarewyck
#' @export
modalDialogWithCross2 <- function(...
                                  , addCross = TRUE
                                  , title = NULL
                                  # , footer = modalButton(i18n$t("cancel"))
                                  # , i18n = i18n
                                  , cancelText = "cancel"
                                  , footer = modalButton(cancelText)
                                  , size = c("m", "s", "l")
                                  , easyClose = FALSE
                                  , fade = TRUE) {
  
  size <- match.arg(size)
  
  cls <- if (fade) "modal fade" else "modal"
  
  div(id = "shiny-modal"
      , style = "width:1250px;"
      , class = cls
      , tabindex = "-1"
      , `data-backdrop` = if (!easyClose) "static"
      , `data-keyboard` = if (!easyClose) "false"
      
      , div(
        class = "modal-dialog"
        , class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg")
        , div(class = "modal-content"
            
            , div(class = "modal-header",
                if (!is.null(title)) tags$h4(class = "modal-title", title),
                if (addCross) actionButton(inputId = "modal-cross", label = "\U2715")
            ),
            
            div(class = "modal-body", ...),
            
            if (!is.null(footer)) div(class = "modal-footer", footer)
        ),
        tags$script("$('#shiny-modal').modal().focus();")
      )
  )
}
