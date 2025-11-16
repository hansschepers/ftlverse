#' candlesShiny
#' @examples \dontrun{
#'   vrmdQuick("test")
#'   candlesShiny(cpdt = cpdt, shinify = TRUE)
#'   candlesShiny(cpdt = cpdt, shinify = FALSE)
#'   vrmd("info")
#' }
#' @export
candlesShiny <- function(cpdt, shinify = FALSE){
  aphKey(cpdt)
  cpdt[processName == "RAD" & value > 1500, value := NA]
  cpdt[, doy := yday(dateTime)]
  cpdt[, hr := hour(dateTime)]
  cpdt[, wk := week(dateTime)]
  cpdt[, processName := gsub(" ", "_", processName)]
  
  keep <- c("dateTime", "wk", "doy", "hr", "processName", "value")
  voi <- "processName"
  takeOut <- list()
  
  factos <- c(aphFactors(cpdt), "wk", "doy")
  choices <- lapply(setNames(factos, factos), function(x) unique(as.list(cpdt)[[x]]))
  choices$xoi <- c("RAD", "temperature_gutter")
  # defaults  
  {
    def <- list()
    def$wk <- c(5, 10, 15, 20, 25)
    def$processName = grep("(temperature)|(growth)|(rad)|(water)|(transpiration)|(drain)"
                           , choices$processName, ignore.case = TRUE, value = TRUE)
    def$processName <- setdiff(def$processName
                               , grep("(JCM)|(reset)|(substrate)|(stem_load_cell)|(water_supply)"
                                      , choices$processName, value = TRUE))
    def$xoi <- c("RAD", "temperature_gutter")[1]
    def$cumul <- "yes"
    def$su <- 0
    def$titleId <- cpdt[1, plot_syn]
    input <- def
  }
  
  if (!shinify){
    output <- list()
    renderPrint <- srenderPlot <- function(x) eval(x)
    reactive <- function(x) function() {eval(x)}
  } else {
    # library(shiny)
    ui <- function(input, output, session){
      fluidPage(fluidRow(column(2, 
        selectInput("wk", "weeks"                , choices = choices$wk,          selected = def$wk, multiple = TRUE)
        , selectInput("processName", "variables" , choices = choices$processName, selected = def$processName, multiple = TRUE)
        , selectInput("xoi", "X-axis"            , choices = choices$xoi,         selected = def$xoi, multiple = FALSE)
        , radioButtons("cumul", "Cumulative"     , choices = c("yes", "no"), inline = TRUE)
        , sliderInput("su", "center", min=0, max=1, value=0, step = 0.05)
        , textInput("titleId", "titleId", def$titleId)
        , verbatimTextOutput("phover1")
      ), column(10, plotOutput("candles", height = "900px", hover = "plot_hover") ))) 
    }
  }
  
  server <- function(input, output, session) {
    dt1 <- reactive({
      dt <- cpdt[wk %in% input$wk]
      dt <- dt[processName %in% input$processName]
      if (input$cumul == "yes"){
        dt[, value := hcumsum(value-input$su*hmean(value))/24
           , by = .(processName, doy)]
      }
      dtw <- hdcast(dt[, ..keep])
      dtw[, dateTime := NULL]
      .dtw000 <<- dtw
      .input000 <<- input
      dt1 <- aphMelt(dtw, id.vars = c("wk", "doy", "hr", input$xoi))
      dt1
    })
    
    dt2 <- reactive({
      dt2 <- dt1()
      takeOut$doy <- dt2[, hdiffrange(get(input$xoi)), by = doy][V1 == 0][, doy]
      vv = names(takeOut)[1]
      for (vv in names(takeOut)){
        dt2 <- dt2[!get(vv) %in% takeOut[[vv]]]
      }
      dt2
    })
    
    p_candleR <- reactive({
      dt2 <- dt2()
      p01 <- pggs(dt2, xoi = input$xoi, group = "doy", foi = "wk", lwd = .1
                  , doplot = FALSE, chunkTitle = input$titleId)
      if (input$cumul == "yes"){
        p01 <- pggs(dt2[hr == 23], xoi = input$xoi, group = "doy", foi = "wk"
                  , p = p01
                  , label = "doy", mega = TRUE
                  # , psize = 9, labelSize = 7#, labelColor = "darkgrey"
                  , geom = "point", pointAlpha = .6
                  , doplot = FALSE, chunkTitle = input$titleId)
        p01 <- pggs(dt2[hr == 23], xoi = input$xoi, foi = voi
                  , p = p01
                  , geom = "point", pointAlpha = 0, fsize = 16
                  , ci.alpha = .05
                  , doplot = FALSE, chunkTitle = input$titleId)
      }
      p01
    })
    output$candles <- renderPlot({ p_candleR() })
      
    output$phover1 <- renderPrint({
      input$plot_hover[c("x", "y", "panelvar1")]})
  }
  
  if (!shinify){
    eval(body(server))
    # print(output$candles)
    return(p_candleR())
    # return(output$candles)
  } else {
    shinyApp(ui = ui, server = server)
  }
}
