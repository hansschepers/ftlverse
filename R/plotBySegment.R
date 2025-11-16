#' plotBySegment
#' @examples \dontrun{
#'   plotBySegment(.SIMS$out)
#'   plotBySegment(.SIMS$out, layerIN = "skin", pggsInputAdd = list(legend = "right"))
#'   plotBySegment(.SIMS$out, layerIN = "muscle", pggsInputAdd = list(legend = "right"))
#'   yoisOUT <- c("speed", "road_distance", "v", "x")
#'   ddm[!processName %in% yoisOUT][value == hmax(value)]
#'   plotBySegment(out, yoisOUT = yoisOUT, geom = "line", dois = "T_air")
#'   aphVariableLevels(.ddww)
#' }
#' 
#' @export
plotBySegment <- function(out = SIMS$out
                          , SIMS = list()
                          , yoisIN = "all"
                          , yoisOUT = c("v")
                          , segmentOUT = character()
                          , layerIN = "all"
                          , layerOUT = c("page2", "page3")
                          , dois = "time"
                          , myois = c("emax_tr"
                                      , "lungCooling"
                                      , "Altitude"
                                      , "Slope"
                                      # , "speed"
                                      # , "co"
                                      # , "chill"
                          )
                          , poolOther = TRUE
                          , pggsInputAdd = list()
                          , doplot = TRUE
){
  {
    pggsInput = list(facet_w = "layer"
                     , group = "processName"
                     , psize = 3, pointAlpha = .4
                     , lwd = 1.5, lineAlpha = .4
                     , legend = "none"
                     , subtitle = SIMS$context
                     , allowppt = FALSE
                     , free_y = TRUE)
    pggsInput[names(pggsInputAdd)] <- pggsInputAdd
    
    dd <- as.data.table(out)
    ddm <- aphMelt(dd, dois = dois)
    aphVariableLevels(ddm)
    if (!"time" %in% dois) yoisOUT <- union(yoisOUT, "time")
    if (!"all" %in% yoisIN) ddm <- ddm[processName %in% yoisIN]
    ddm <- ddm[!processName %in% yoisOUT]
    
    ddm[, (c("segment", "layer")) := "page2"]
    ddm[processName %in% c("v"), layer := "page3"]
    ddm[grepl("^T", processName)
        , (c("segment", "layer")) := tstrsplit(processName, "_")]
    
    ddm[grepl("^T", processName), layer := paste("Temperature in ", layer)]
    ddm[, segment := sub("^T", "", segment)]
    ddm[segment == "brain", layer := "Blood & Brain"]
    ddm[segment == "brain", segment := "hd"]
    ddm[segment == "blood", layer := "Blood & Brain"]
    
    #group
    {
      layer.s <- c(core = "core"
                   , muscle = "muscle"
                   , fat = "fat"
                   , skin = "skin")
      
      for (layer.oi in layer.s){
        ddm[processName %in% yoisList[[layer.oi]], layer := layer.oi]
      }
      ddm[processName %in% c("tcore", "tskin", "tmuscle", "tfat"), layer := "bodyTemp"]
      ddm[processName %in% c("dilat", "stric"), layer := "blood flow"]
      ddm[processName %in% c("chill", "sweat"), layer := "Chill Sweat"]
      ddm[processName %in% c("road_distance", "speed"), layer := "Speed, X"]
      ddm[processName %in% c("sum_Pext", "sum_Watt", "heat_production_Watt"), layer := "Power"]
      ddm[processName %in% c("co"), layer := "Cardiac Output"]
      ddm[processName %in% c("Pacc", "Pext", "Pair", "Prolling", "Pgrade"), layer := "PowerMovements"]
      
      for (myoi in myois){
        ddm[processName == myoi, layer := myoi]
        ddm[processName == myoi, segment := myoi]
      }
    }
    
    # order
    # if(F){
    #   segmentLevels <- c("hd", "tr", "arm", "hand", "leg", "feet"
    #                      , "blood", "mean", myois)
    #   ddm[, segment := factor(segment, levels = segmentLevels, ordered = TRUE)]
    # }
    
    .ddm00 <<- copy(ddm)
    ddm[is.na(layer), layer := "other"]
    if (poolOther) {
      ddm[segment == "page2", segment := "other"]
    } else {
      ddm[segment == "page2", segment := processName]
    }
    .ddm0 <<- copy(ddm)
    .ddm0[, unique(layer)]
    if (!"all" %in% layerIN) ddm <- ddm[layer %in% layerIN]
    .ddm <<- copy(ddm)
  }
  {
    # ddww <- copy(.ddm)
    ddww <- copy(ddm)
    ddww <- ddm[!segment %in% segmentOUT]
    ddww <- ddm[!layer %in% layerOUT]
    .ddww <<- copy(ddww)
    
    
    # add labels
    {
      ddww[, ind := seq(.N), by = processName]
      ddww[, llaabb := NA_character_]
      ddww[, llaabb := ifelse(sample(ind, 1) == ind, processName, llaabb)
           , by = processName]
      # ddww[!is.na(llaabb)]
      pggsInput$label = "llaabb"
     
      # if (Sys.getenv("R_CONFIG_ACTIVE") == "development"){
        pggsInput$labelRepel = 2
      # }
    }
    .pggsInput <<- pggsInput
    
    
    pggsInput$xoi <- dois[1]
    pggs(ddww
         , input = pggsInput
    )
  }
}
