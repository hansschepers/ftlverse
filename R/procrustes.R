#' procrustes
#' 
#' trim a numeric vector 'x' to a template 'bed'
#' 
#' @export
procrustes <- function(x
                       , bed
                       , n = length(bed)
                       , pos = c("left", "right")[2] #, "NA", "both"
                       , repl = c(NA, 0)[1]
                       ){
  nx <- length(x)
  res <- x
  if (n > nx){
    # elongate
    if (pos[1] == "left"){
      res <- c(rep(repl[1], n - nx), x)
    }
    if (pos[1] == "right"){
      res <- c(x, rep(repl[1], n - nx))
    }
  }
  # trim
  res <- res[seq_along(bed)]
  res
}
