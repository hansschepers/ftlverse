# old APHprototype/inst/tools/, now in aphTools/R

#' to8601
#' 
#' @export
to8601 <- function(x = Sys.time(), 
                   format = "%Y-%m-%dT%H:%M:%S%z", 
                   tz = "Europe/Amsterdam", 
                   clean = c("", "[-:+]")[1]
) 
  gsub(clean, "", format(x, format = format, tz = tz))
# to8601()
