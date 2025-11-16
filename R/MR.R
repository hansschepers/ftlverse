#' paramSens2pca
#' 
#' @export
paramSens2pca <- function(ddsens
                          , kpiOUT_rgx.s = c("^v__")
                          , doplot = FALSE
                          , var.name = "kpi"
                          , doScale = TRUE
                          # , ...
){
  ddPca <- data.table::copy(ddsens)
  kpiOUT_rgx <- kpiOUT_rgx.s[1]
  for (kpiOUT_rgx in kpiOUT_rgx.s){
    ddPca <- ddPca[!grepl(kpiOUT_rgx, get(var.name))]
  }
  # ddPca[, kpi := paste0(kpi, "(", time, ")")]
  ddPca[, lab := paste0(kpi, "(", time, ")")]
  # kpi.s <- ddPca[, unique(get(var.name))]
  # TimeFocus = grepl("__0.6$", kpi)
  # ddPca <- ddPca[eval(TimeFocus)]
  
  out_pca <- aphPca(ddPca
               # , legend = "right"
               , labelCol = "lab"#var.name
               , color = var.name
               , legend = "none"
               , loadrad = 20
               , psize = 3
               , pointAlpha = 1
               , lsize = 4
               , loadLabelFun = ggrepel::geom_label_repel
               # , labelMod = function(x, ...) {paste0("Var = ", x)}
               , labelRepel = 2, labelSize = 1
               , doScale = doScale
               # , ...
               , doplot = FALSE)
  if (doplot) {
    print(out_pca$plot)
  }
  out_pca
}



#' parameterClustering
#' @export
parameterClustering <- function(out_pca
                                , SIMS = NULL
                                , nPC = 2
                                , kTree = 3
                                , pggsInputAdd = list()
){
  if (missing(out_pca)){
    stopifnot(length(SIMS) > 0)
    ddsens <- LSAjac(SIMS)
    out_pca <- paramSens2pca(ddsens)
  }
  pggsInput <- list()
  pggsInput <- mergeParameters(pggsInput, pggsInputAdd)
  
  {
    dtRot <- as.data.table(out_pca$rotation, keep.rownames = TRUE)
    setnames(dtRot, "rn", "parName")
    dd <- aphMelt(dtRot)
    # dd[, absValue := abs(value)]
    ddFocus <- dd[processName %in% paste0("PC", 1:nPC)]
    # ddFocuswide <- hdcast(ddFocus, value.var = "absValue")
    ddFocuswide <- hdcast(ddFocus, value.var = "value")
    {
      hc <- hclust(dist(ddFocuswide))
      clustering <- cutree(hc, k = kTree)
      # plot(hc, labels = ddFocuswide$parName, cex = .4)
      # plot(hc)
      paramOrder <- ddFocuswide$parName[hc$order]
      ddFocuswide[, clusterID := as.character(clustering)]
    }
    # , id.vars = c("parName", "clusterID")
    ddFocus2 <- aphMelt(ddFocuswide)
    ddFocus2[, parName := factor(parName, levels = paramOrder, ordered = T)]
    ddFocus2
    
    p_bars <- pggs(ddFocus2
                   # , xoi = "absValue"
                   , xoi = "value", xlab = "loading on axis"
                   , yoi = "parName", free_y = FALSE, ylab = NULL, ysize = 8
                   , facet_w = "processName"
                   , foi = "clusterID"
                   , geom = "col"
                   , legendTitle = "Cluster:"
                   , input = pggsInput
    )
    .p_bars <<- p_bars
    pggsPALETTE <- c("black", "darkred", "darkgreen", "darkblue"
                     , "magenta", "orange", "cyan", "violet"
                     , "red", "lightgreen", "lightblue", "gray")
    # if (Sys.getenv("R_CONFIG_ACTIVE") == "development"){
    # labelRepel <- 2 
    # } else {
    # labelRepel <- 0
    # }
    xoi <- "PC1"
    yoi <- "PC2"
    p_scatter <- pggs(ddFocuswide
                      , xoi = xoi
                      , yoi = yoi
                      , foi = "clusterID"
                      , label = "parName"
                      , labelRepel = 2#labelRepel
                      , geom = "point", psize = 8, pointAlpha = .4
                      , legend = "none"
                      , input = pggsInput
                      , palette.oi = pggsPALETTE[c(3, 2, 4)]
    )
    .p_scatter <<- p_scatter
    list(p_par_bars = p_bars
         , p_par_scatter = p_scatter)
  }
}
