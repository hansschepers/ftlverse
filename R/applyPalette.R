#' applyPalette
#' 
#' @export
applyPalette <- function(p, pal.oi = "bayer", useAs = "fillcolor"){
  # pal.std <- c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray")
  bayer = c(green = "#89D329", blue = "#00BCFF", red = "#FF3162", purple = "#624963")
  pal.std <- c("black", "red", "green", "blue"
               , "magenta", "orange", "cyan", "violet"
               , "darkred", "darkgreen", "darkblue", "gray")
  # if (pal.oi != "pal.std"){
  # log_trace(pal.oi)
  # cp <- ggthemes::canva_palettes
  # if (pal.oi[1] %in% names(cp)) {
  #   palette.oi <- cp[[pal.oi]]
  # } else {
  if (exists(base::get(pal.oi), mode="character")) {
    stop("palette not found")
  } else {
    palette.oi <- base::get(pal.oi)
  }
  # }
  palette.oi = rep(unname(palette.oi), 100)
  if (grepl("fill", useAs)){
    p = p + scale_fill_manual(values = palette.oi)
  }
  if (grepl("color", useAs)){
    p = p + scale_color_manual(values = palette.oi)
  }
  p
}
