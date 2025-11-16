# aphBox 
# wrapper for shinydashboardPlus :: box with aph defaults.
# also adds shinyjqui :: jqui_resizable()
# NO TRANSLATION
# @examples \dontrun{
#    ui <- dashboardPage(
#    header = dashboardHeader()
#    , sidebar = dashboardSidebar()
#    , body = dashboardBody(
#      fluidRow(aphBox(6, title = "title"
#                      , p("par")
#                      , h3("h3")
#                      # , collapsed = TRUE
#                      # , footer = p("foot")
#      ))
#    )
#    )
#    ui
#    vhtml(ui)
#    ww <- shinyApp(ui = ui, server = function(input, output){})
#    ww
#    vhtml(aphBox())
# } 
# importFrom shinydashboardPlus box
# @export
# aphBox <- function(..., width = 12
#                    , title = "title"
#                    , solidHeader = TRUE
#                    , collapsible = TRUE
#                    , closable = FALSE
#                    , background = NULL # "white"
#                    , footer_padding = FALSE
#                    , footer = NULL
#                    , status = "success"
#                    , makeResizable = FALSE
#                    , box2skip = character(0)
#                    , newSection = character(0)
#                    # , ...
# ){
#   log_trace("aphBox {title}")
#   if (length(newSection)){
#     vrmd(section = setNames(list(x = newSection), title))
#   }
#   if (title %in% box2skip) {
#     log_trace("aphBox {title} ======================================== SKIPPED ")
#     htmlString <- shinydashboardPlus :: box(
#       width = width
#       , solidHeader = solidHeader
#       , collapsible = collapsible
#       , closable = closable
#       , footer_padding = footer_padding
#       , footer = footer
#       , background = background
#       , color = "red"
#       , status = status
#       , title = title)
#   } else {
#     # with ... !
#     htmlString <- shinydashboardPlus :: box(...
#                                           , width = width
#                                           , solidHeader = solidHeader
#                                           , collapsible = collapsible
#                                           , closable = closable
#                                           , footer_padding = footer_padding
#                                           , footer = footer
#                                           , background = background
#                                           , color = "red"
#                                           , status = status
#                                           , title = title)
#   }
#   
#   if (makeResizable) {
#     htmlString <- shinyjqui :: jqui_resizable(htmlString)
#   }
#   return(htmlString)
# }
# 
