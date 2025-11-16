################################################################################
{
  if (!exists("y_init")) y_init <- unlist(cyclist03States())
  namesOutputVariables <- names(y_init)
  yoisList <- list(all_temps = setdiff(names(y_init), c("x", "v")))
  
  segment.s <- c(head = "hd"
                 , trunk = "tr"
                 , arm = "arm"
                 , hand = "hand"
                 , leg = "leg"
                 , feet = "feet")
  yoisList <- c(yoisList, sapply(unname(segment.s)
                                 , \(regex) grep(regex, yoisList$all_temps, value = T)
                                 , simplify = F)
  )
  yoisList
  layer.s <- c(core = "core"
                       , muscle = "muscle"
                       , fat = "fat"
                       , skin = "skin")
  yoisList <- c(yoisList, sapply(layer.s
                                 , \(regex) grep(regex, yoisList$all_temps, value = T)
                                 , simplify = F)
  )
  yoisList$hd <- c("Tbrain", yoisList$hd)
  yoisList$core <- c("Tbrain", yoisList$core)
  yoisList
}
