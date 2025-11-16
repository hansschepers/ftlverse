#' assessBreaks
#' 
#' @examples \dontrun{
#'   dx <-  c(0, .01, .7, 0, -1, 11, 0)
#'   assessBreaks(dx)
#'   x <- 100*1:9
#'   res <- assessBreaks(x, lb = 333)
#'   assessBreaks(x, ub = 777)
#'   assessBreaks(x, lb = 333, ub = 777)
#'   assessBreaks(x, lb = 333, ub = 1e11)
#'   assessBreaks(x, lb = 50, type = "diff")
#'   res <- assessBreaks(x, lb = 150, type = "diff")
#'   assessBreaks(x, lb = 1, type = "relchange")  #TODO
#'   res <- assessBreaks(x, lb = .4, type = "relchange")  #TODO
#'   length(res)
#'   assessBreaks(c(200, NA, 500, 2000, 0, 100)
#'                , type = "diff", lb = 200#, ub = 1e11
#'                , naNormal = FALSE)
#'   assessBreaks(c(200, NA, 500, 2000, 0, 100)
#'                , type = "relChange", lb = -0.20, ub = 1e11
#'                , naNormal = TRUE)
#'   )
#' }
#' @export
assessBreaks <- function(
    x
    , pn = ""
    , type = c("value", "diff", "relChange")[1]
    , lb = "auto"
    , ub = "auto"
    , normal = "normal"
    , Tolerances = list(
      diff = list(growthRate = c(low = -0.2, normal = 0.5, high = 5))
      , relchange = list(growthPercentage = c(low = -.012, high = .02)))
    , naNormal = TRUE
){
  x
  if (tolower(type[1]) == "diff") x <- diff0(x)
  if (tolower(type[1]) == "relchange") {
    x <- x / shift(x, n = 1
                    # , fill = x[1]
                    ) - 1
  }
  Tols <- Tolerances[[tolower(type[1])]][[pn]]
  if(is.null(Tols)){
    if (tolower(type[1]) == "value") Tols <- c(low = 0, normal = 1)
    if (tolower(type[1]) == "diff") Tols <- c(low = -1, normal = 1)
    if (tolower(type[1]) == "relchange") Tols <- c(low = -0.2, normal = 0.5, toohigh = 5)
    Tols <- numeric()
  }
  Tols <- c(unused = -Inf, Tols, extreme = Inf)
  if (lb != "auto") Tols <- c(c(low = lb), Tols)
  if (ub != "auto") Tols <- c(Tols, c(normal = ub))
  if(("extreme" %in% names(Tols) &
     !"high" %in% names(Tols)) | Tols["extreme"] != Inf ) names(Tols)[names(Tols) == "extreme"] <- "high"
  Tols <- Tols[order(Tols)]
  
  if (length(Tols) <= 2){
    # log_warn("no Tolerances found")
    res <- rep(normal, times = length(x))
    return(res)
  }
  if (log_threshold() == 600) print(Tols)
  if (log_threshold() == 600) print(x)
  ind <- findInterval(x, Tols)
  ind
  res <- names(Tols)[ind+1]
  res
  setNames(x, res)
  if (naNormal) res[is.na(res)] <- normal
  res
}
