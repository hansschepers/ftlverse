#' dev2png
#' 
#' takes a plot to convert to png file
#' @importFrom htmlwidgets saveWidget
#' @importFrom ggplot2 ggsave
#' @importFrom webshot2 webshot
# @importFrom plotly orca
#' @export
dev2png <- function(p = NULL
                    , ffpng.out = NULL
                    , ggwidth = 14
                    , ggheight = 8
                    , dpi = 150
                    # , relsize = c(1.4, 1) # unused
                    # , pxwidth = 560, pxheight = 380
                    # , aspect.ratio = "auto" # unused
                    , ffhtml = paste0(tempfile(), ".html")
){
  # if (aspect.ratio != "auto") {
  #   ggwidth <- ggheight / aspect.ratio
  # }
  
  if(is.null(p)) {p <- ggplot2::last_plot()}
  # hi-res     ggplot2::ggsave("last.png", plot=p, dpi=600, width=7, height=6, units="in")
  # .ffpng.out <<- ffpng.out
  ffpng.out <- ASCIIfy(ffpng.out)
  if (!is.null(ffpng.out)) {
    my_temp_file <- ffpng.out
  } else {
    my_temp_file <- tempfile(pattern = "d2v2png_", fileext = ".png")
  }
  log_trace("dev2png| my_temp_file: {my_temp_file}")
  
  
  ############################################################ base plot (with "recordPlot" / replayPlot)
  if ("recordedplot" %in% class(p)){
    log_info("recordedplot, reprinted to png device...")
    suppressWarnings(suppressMessages(
      png(my_temp_file
          , width = ggwidth
          , height = ggheight
          , units = "in"
          , res = dpi)
    ))
    print(p)
    dev.off()
    return(my_temp_file)
  }
  
  ############################################################ ggplot object
  if ("ggplot" %in% class(p) | "gg" %in% class(p)
      | "gtable" %in% class(p) | "arrangelist" %in% class(p) ){
    # log_info("ggplot, saved with ggsave...")
    .my_temp_file <<- my_temp_file
    .p0000 <<- p
    suppressWarnings(suppressMessages(
      ggplot2::ggsave(my_temp_file
                      , plot = p
                      , dpi = dpi
                      , width = ggwidth
                      , height = ggheight
                      , units = "in")
    ))
    return(my_temp_file)
  }
  
  ############################################################ ggplot object
  if (inherits(p, "plotly")){
    # message("not yet implemented")
    # p <- plotly::as_widget(p)
    suppressWarnings(suppressMessages(
      htmlwidgets::saveWidget(p
                              , file = ffhtml
                              , selfcontained = TRUE)
    ))
    # plotly::orca(p=p, file = my_temp_file)
    # aphViz::plotlyToPng(plotlyObject=p, filePaths = my_temp_file)
  }
  
  
  ############################################################ WIDGET: d3heatmap, leaflet map, ...
  # doWebshot <- all(c("htmlwidget") %in% class(p))
  doWebshot <- inherits(p, "htmlTable") | inherits(p, "htmlwidget")
  
  if (inherits(p, "htmlTable")){
    log_info("htmlTable, writing to ", ffhtml)
    writeLines(p, ffhtml)
  } else {
    log_info("htmlwidget, saving to ", ffhtml)
    suppressWarnings(suppressMessages(
      htmlwidgets::saveWidget(
        p
        , file = ffhtml
        , selfcontained = TRUE
        # , background = "#ccccff"
        # , title = "test"
        # , knitrOptions = list(fig.width = ggwidth
        #                       , fig.height = ggheight)
      )
    ))
  }
  
  editHtml <- TRUE
  if (editHtml){
    log_warn("editing html")
    html_content <- readLines(ffhtml)
    html_content <- gsub("background-color: rgba(204,204,255,"
                         , "background-color: rgba(255,255, 255,"
                         , html_content
                         , fixed = TRUE)
    html_content <- gsub("<title>.*</title>"
                         , ""
                         , html_content
                         , fixed = TRUE)
    ffhtml2 <- tempfile(fileext = ".html")
    # ffhtml2 <- paste0("test", htimestamp(),".html")
    log_info("Writing edited html to {ffhtml2} ")
    writeLines(htmltools::HTML(paste(html_content, collapse = "\n")), ffhtml2)
  } else {
    ffhtml2 <- ffhtml
  }
  
  
  # file.info(ffhtml)
  .ffhtml1 <<- ffhtml
  .ffhtml <<- ffhtml2
  if (doWebshot){
    log_info("capturing {ffhtml2} with webshot2::webshot... as png: {my_temp_file}")
    .my_temp_fileWS <<- my_temp_file
    suppressWarnings(suppressMessages(
      webshot2::webshot(ffhtml2
                        , file = my_temp_file
                        , vwidth = ggwidth
                        , vheight = ggheight
                        # , cliprect = "viewport"
      )
    ))
    file.remove(ffhtml2)
  }
  #   # jsGraphic2Pdf(ffhtml, c(2500, 2000, 50, 50))
  #   Sys.getenv("PATH")
  #   Sys.setenv(PATH = paste0(Sys.getenv("PATH"), 
  #                            "C:/Users/hhsche2/Documents/Software/R-3.6.2/bin/x64"))
  #   js2graphic::jsGraphic2Png("test.html"
  #                             , plotDims = c(3000, 2400, 50, 50)
  #                             , outFile = "test4.png" # my_temp_file
  #                             # , pathPhantomJS = "C:/Users/hhsche2/Documents/Software/R-3.6.2/bin/x64"
  #                             # , slow = TRUE
  #                             )
  # pdfPageExtract("JSgraphic.pdf", 1, 1, "JSgraphic2.pdf") # removing the last 2 empty pages from JSgraphic.pdf
  # pdfcrop("JSgraphic2.pdf", "JSgraphic3.pdf", c(-30, -30, -70, -30)) # cropping with adding a little extra white margin to the automatic crop; JSgraphic3.pdf contains the final cropped figure
  # file.remove(c("heatmap.html", "JSgraphic.pdf", "JSgraphic2.pdf", "JSgraphic3.pdf"))
  my_temp_file
}
