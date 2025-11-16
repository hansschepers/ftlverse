#' vmmd
#' @export
vmmd <- function(mmdlines
                 , fftmp = tempfile(fileext = ".mmd")){
  if (!file.exists(fftmp)){
    writeLines(mmdlines, fftmp)
  }
  DiagrammeR::mermaid(fftmp)
}

# dd <- data.table(x = 2:4, y = 4:3)
# graph<- ggplot(dd, aes(x = x, y=y))+ geom_bar(stat='identity')
# gg <- plotly::ggplotly(graph)
vhtml2 <- function(gg
                   , fftmp = tempfile(fileext = ".html")){
  str(fftmp)
  htmlwidgets::saveWidget(plotly::as_widget(gg)
                          , fftmp)
  # browseURL(fftmp)
}



#' save_tags
#' @export
save_tags <- function (tags
                       , file
                       , selfcontained = F
                       , libdir = "./lib"
){
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                    "_files", sep = "")
  }
  htmltools::save_html(tags, file = file, libdir = libdir)
  if (selfcontained) {
    if (!htmlwidgets:::pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
           "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    htmlwidgets:::pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  return(htmltools::tags$iframe(src = file
                                , height = "400px", width = "100%", style="border:0;"))
}


#' vhtml
#' 
#' @export
vhtml <- function(string, addHeader = TRUE){
  string <- as.character(string)
  if (addHeader){
    if (grepl("<body>", string)){
      string <- paste0("<html>\n", string, "\n</html>")
    } else {
      string <- paste0("<html><body>\n", string, "\n</body></html>")
    }
  }
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  writeLines(string, htmlFile)
  # browseURL(htmlFile)
  # rstudioapi::viewer(htmlFile)
}


# view_kable <- function(x, ...){
#   tab <- paste(capture.output(kable(x, ...)), collapse = '\n')
#   tf <- tempfile(fileext = ".html")
#   writeLines(tab, tf)
#   rstudioapi::viewer(tf)
# }
# view_kable(.dtw, format = 'html', table.attr = "class=nofluid")
# ?DT::renderDT
# 
# dd <- data.frame(
#   date = seq(as.Date("2015-01-01"), by = "day", length.out = 5), x = 1:5
# )
# datatable(dd)
# kable(.dtw) %>% kableExtra::kable_styling()

#' vdata
#' @importFrom DT datatable
#' @export
vdata <- function(dt, ...){
  DT::datatable(dt, ...)
}
