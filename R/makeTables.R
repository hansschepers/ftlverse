#' makeTables
#' @export
makeTables <- function(CONSTANTS = cyclist03Constants()
                       , segment.s){
  # not yet done P (pressure), CCbody_tot, 
  dcPars25 <- with(CONSTANTS
                  , data.table(segment_layer = names(TSET)
                               , TSET = TSET
                               , CC = CC
                               , QB = c(QB, 0)
                               , EB = c(EB, 0)
                               , BFB = c(BFB, 0)
                               , TC = c(TC, 0)
                               # , RATE = RATE
                  ))
  
  # write.csv(dcPars25, "inst/digital_cyclist/dcPars25.csv")
  
  dcPars6 <- with(CONSTANTS
                 , data.table(segment = segment.s
                              , S = S
                              , HR = HR
                              , HC = HC
                              , SKINR = SKINR
                              , SKINS = SKINS
                              , SKINV = SKINV
                              , SKINC = SKINC
                              , WORKM = WORKM
                              , CHILM = CHILM
                 ))
  list(dcPars25 = dcPars25
       , dcPars6 = dcPars6)
}

