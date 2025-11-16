#' shinifyFunction
#' @examples \dontrun{
#'   #library(shiny)
#'   #library(aphShiny)
#'   getShinyOption("shiny.launch.browser")
#'   prepShiny()
#'   getShinyOption("shiny.launch.browser")
#'   shinifyFunction("aphPca", DT)
#'   shinifyFunction("aphPca", DT, valueColumn = "senseValue")
#' }
#' @export
shinifyFunction <- function(DT
                            , func
                            # , VIS = "plotBenchmark"
                            , parameters = list(plotHeight = 600, maxLevels = 200)
                            , bycols = c("mon", "woy")
                            , colFilters = list(
                              pcaVariable  = 0
                              # , processName = 1:4
                              # , sensPar = 1:2
                              # , plot_syn = 1
                              # , field_syn = 0
                              # , mon = 3:9
                              # , woy = 0
                              # , account_id = 1
                            )
                            , positiveSelect = character(0) # "growth|temp|energy|vpd"
                            , negativeSelect = c(
                              "top$"
                              , "resetValue"
                              # , "2$", "vent"
                              # , "circuit", "outside"
                              , "sub temp"
                            )
                            # , i18n = list(t = function(x, ...) {x})
                            , valueColumn = "value"
                            , ...
) {
  prepShiny()
  dots <- list(...)
  
  # VIS = match.fun(VIS)
  # globals
  # dt.back <- aphMelt(dtwSim)
  dt.back <- copy(DT)
  if (!"plot_syn" %in% names(dt.back)) dt.back$plot_syn <- "dummy"
  
  if ("dateTime" %in% names(dt.back)){
    if (!"hod" %in% names(dt.back)) dt.back[, hod := hour(dateTime)]
    if (!"woy" %in% names(dt.back)) dt.back[, woy := week(dateTime)]
    if (!"mon" %in% names(dt.back)) dt.back[, mon := month(dateTime)]
  }
  
  # bycols = intersect(bycols, names(dt.back))
  # dt.back[, processName := i18n$t(processName)]
  
  (bycols <- aphFactors(dt.back))
  (bycols <- c(bycols, c("mon", "woy", "hod")))
  (bycols <- intersect(bycols, names(dt.back)))
  aphKey(dt.back)
  # message("keys:")
  # print(key(dt.back))
  # str(dt.back)
  # unique(dt.back$mon)
  # mask sensitive info
  # show/know decode table
  
  decoderDt <- dt.back[, .N, by = .(plot_syn)]
  decoderDt[, plot_synCoded := paste("plot", as.numeric(as.factor(plot_syn)))]
  decoderDt
  
  
  colFilters <- colFilters[names(colFilters) %in% names(dt.back)]
  
  with(parameters,{
    fois <- names(colFilters) #aphFactors(dt.back)
    # setkeyv(dt.back, c(fois, "woy", "hod"))
    sortunique <- function(x) sort(unique(x))
    choiceListN <- sapply(dt.back[, ..fois], uniqueN)
    
    choiceList <- lapply(dt.back[, ..fois], sortunique)
    {
      tmp <- choiceList$processName
      if (length(positiveSelect)){
        tmp <- grep(positiveSelect, tmp, value = TRUE)
      }
      tmp
      for (neg in negativeSelect){
        tmp <- tmp[!grepl(neg, tmp)]
      }
      tmp
      choiceList$processName <- tmp
    }
    
    choiceList <- choiceList[choiceListN < maxLevels]
    .choiceList <<- choiceList
    str(choiceList)
    
    # x <- 1
    defChoices <- lapply(seq_along(colFilters),
                         function(x) {
                           pref <- as.integer(colFilters[[x]])
                           choice <- choiceList[[names(colFilters)[[x]] ]]
                           # message("x ",x) ; str(pref) ; str(choice)
                           if (any(is.na(pref))){
                             message("not integer")
                             choice[1]
                           } else if (all(pref %in% seq(1, length(choice))) ){
                             choice[pref]
                           } else {
                             choice
                           }
                         })
    names(defChoices) <- paste0("default", names(choiceList))
    names(choiceList) <- paste0(names(choiceList), "Choices")
    # str(defChoices)
    
    makeControls <- function(colFilters, choiceList, defChoices){
      # cmd = "span("
      cmd = "fluidRow(\n"
      parii = names(colFilters)[1]
      colWidth <- setNames(rep(2, length(colFilters))
                           , names(colFilters))
      colWidth[names(colFilters) == "processName"] <- 6
      colWidth[names(colFilters) == "sensPar"] <- 6
      colWidth[names(colFilters) == "pcaVariable"] <- 6
      .colWidth <<- colWidth
      ii = 0
      for (parii in names(colFilters) ){ 
        ii = ii + 1
        cmdn  = paste0("column(", colWidth[parii]
                       , ", selectInput('",parii,"Chosen', "
                       , "label = '", parii, "', "
                       , "choices = ", parii,"Choices, "
                       , "selected = default",parii
                       , ", multiple = TRUE"
                       , ", selectize = TRUE"
                       , ", width = 1350"
                       , "))")
        if (ii == 1) { cmd = c(cmd, cmdn) } else { cmd = c(cmd, "\n,", cmdn ) }
      }
      cmd = c(cmd,")")
      cmd <- paste(cmd, collapse = "\n")
      .cmd <<- cmd
      # cat(.cmd)
      with(c(choiceList, defChoices), eval(parse(text=cmd)) )
    }
    # makeControls(colFilters)
    
    MainPage = function(){
      fluidPage(title="benchmark App", #responsive = FALSE,
                tabsetPanel(
                  tabPanel("Plot", 
                           makeControls(colFilters, choiceList, defChoices)
                           , fluidRow(
                             column(4, selectInput("sf_foi", "foi:"
                                                   , choices = bycols
                                                   , selected = NULL
                                                   , multiple = TRUE, selectize = TRUE))
                             , column(4, selectInput("accross", "Accross:"
                                                     , choices = bycols
                                                     , selected = setdiff(bycols
                                                                          , c("processName", "woy"))
                                                     , multiple = TRUE, selectize = TRUE))
                             , column(2, textInput("userTitle", "Title"
                                                   , value = "Filter: Strabena") )
                             , column(2, sliderInput('visheight', 'Chart Height'
                                                     , min=200, max=1000, value=700
                                                     , step=50, width=300)  ) 
                           )
                           
                           , fluidRow(
                             tabsetPanel(
                               tabPanel("timeseries"
                                        , fluidRow(plotOutput('visPlot', height=plotHeight) ) 
                               )
                               , tabPanel("ranges"
                                          , fluidRow(plotOutput('rangePlot', height=plotHeight) ) 
                               )
                             )
                           )
                  )
                  , tabPanel("visualization settings"
                             , parControl6("aphPca", 1)
                             , parControl6("aphPca", 2)
                             , parControl6("aphPca", 3)
                             # , parControl5("aphPca", "loadrad")
                  )
                  , tabPanel("Decoder"
                             , verbatimTextOutput("decoderTable")
                             , verbatimTextOutput("statsTable")
                  )
                )
      )
    }
    
    message("starting App...")
    .MainPage <<- MainPage
    shinyApp(
      #     onStart = function(input, output, session) {  # instead of Global.R
      #     },
      
      ui = MainPage()
      
      , server = function(input, output, session) {
        
        output$decoderTable <- renderPrint({
          decoderDt
        }, width = 900)
        
        output$statsTable <- renderPrint({
          statsDt <- dt.back[, .N, by = c(bycols)]
          # statsDt <- hdcast(statsDt, rhs = bycols[1])
          statsDt
        }, width = 900)
        
        last.input <<- observe(isolate(reactiveValuesToList(input)))
        viswidth <-  function(){ 1100 }
        visheight <- function(){ input$visheight }
        
        
        autoTitleR <- reactive({
          autoTitle <- paste0(input$userTitle, ", months: "
                              , paste(month.name[as.numeric(input$monChosen)], collapse = ", "))
          .autoTitle <<- autoTitle
          autoTitle
        })
        
        
        dt.frontR <- reactive({
          input$plot_synChosen
          input$monChosen
          input$processNameChosen
          # filter on
          message(189)
          dt.front <- copy(dt.back)
          # dt.front <- dt.back[mon %in% monChosen &
          #                       processName %in% processNameChosen]
          log_debug("start dim(dt.front): {dim(dt.front)}")
          for (filterii in setdiff(names(colFilters), "plot_syn")){
            log_trace("filter: {filterii}")
            ww <- input[[paste0(filterii, "Chosen")]]
            # print(ww)
            dt.front <- dt.front[as.character(get(filterii)) %in% input[[paste0(filterii, "Chosen")]]]
            log_debug("dim(dt.front): {dim(dt.front)}")
          }
          # print(dt.front[, .N, by = mon])
          log_debug("dim(dt.front): {dim(dt.front)}")
          
          # join to add Codings
          dt.front <- decoderDt[dt.front, on = "plot_syn"]
          # aggregate again
          (bycolsTmp <- c(union("plot_syn"
                                , setdiff(bycols, c(input$accross)))))
          bycolsTmp <- union(input$sf_foi, bycolsTmp)
          str(bycolsTmp)
          .bycolsTmp <<- bycolsTmp
          if (valueColumn %in% names(dt.front)){
            dt.front <- dt.front[, .(value = hmean(get(valueColumn)))
                                 , by = bycolsTmp]
          }
          # dt.front[plot_syn %in% input$plot_synChosen
          #          , plot_synCoded := plot_syn]
          message(211)
          .dt.front <<- dt.front
          dt.front
        })
        
        # unmask focus plot
        dt.frontfR <- reactive({
          message(217)
          dt.frontf <- copy(dt.frontR())
          if ("plot_synChosen" %in% names(input)){
            dt.frontf <- dt.front[plot_syn %in% input$plot_synChosen]
            log_debug("plot_synChosen: {plot_synChosen}")
          }
          # dt.frontf[, plot_synCoded := plot_syn]
          .dt.frontf <<- dt.frontf
          dt.frontf
        })
        
        
        # a.shiny <- list(dt.front = dt.frontR()
        #                 , dt.frontf = dt.frontfR()
        #                 , title = autoTitleR())
        # .a.shiny <<- a.shiny
        # message(231)
        # p = do.call(VIS, a.shiny)
        
        
        output$visPlot = renderPlot({
          req(autoTitleR())
          # .dt.frontf
          argList = list(dfg = dt.frontfR())
          argList$input = list(foi = input$sf_foi
                               , title = autoTitleR())
          argList <- mergeParameters(argList, dots)
          argList$doDebug <- TRUE
          
          # shinyInputNames <- parControl6(func, returnNamesOnly = FALSE)
          argList$input <- input#[shinyInputNames]
          
          .argList <<- argList
          if (func == "aphPca"){
            oo <- do.call(match.fun(func), argList)
            p <- attr(oo, "plot")
          } else {
            p <- do.call(match.fun(func), argList)
          }
          return(p)
        }
        # , width = viswidth
        , height = visheight
        )
        
        
        output$rangePlot = renderPlot({
          req(autoTitleR())
          dd <- dt.frontfR()
          p <- rangeBarPlot(dd
                            , foi4range = input$sf_foi
                            , title = autoTitleR())
          return(p)
        }
        # , width = viswidth
        , height = visheight
        )
        
      } # server
      
      , options = list(launch.browser = TRUE
                       , shiny.trace = TRUE)
      
    ) # shinyApp
  })  #with 
}

