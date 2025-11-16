#' makeYoisList
#' @examples
#' \dontrun{
#'   makeYoisList()
#'   dttest <- as.data.table(list(RAD = 3, afw = 1, growthRate = 9))
#'   makeYoisList(DT = dttest)
#'   makeYoisList(DT = dttest, crossWithDT = TRUE)
#' }
#' @export
makeYoisList <- function(DT = NULL
                         , domains = c("setting", "afw", "maturity")
                         , ignore.case = TRUE
                         , yoisList = list()
                         , crossWithDT = FALSE){
  
  yoisList$PROCESSNAMES_W <- union(PROCESSNAMES_W
                          , c("stem.density.setting"
                              , "pruning"
                              , "setting.fruits.m2.wk"
                              , "setting.fruits.m2.cu"
                              , "harvested.fruits.m2.wk"
                              , "harvested.fruits.m2.cu"
                              , "harvested.truss.stem.wk"
                              , "harvested.truss.stem.cu"
                              , "harvestMaturity"
                              , "yield"
                              , "yield.cu"
                              , "plantload.fruits.m2.calc"
                              , "afw"
                              , "brix"
                          ))
  
  yoisList$PROCESSNAMES_LG <- union(PROCESSNAMES_LG
                           , c("RADJCM", "GHTEMP", "GHTEMPdiff"
                               , "RAD"
                               # , "PARMOL"
                               , "totalPAR"
                               , "GHHUMI", "GHCO2C"
                               , "SGLCO2"
                               , "OUTEMP", "OUHUMI"
                               , "VEWSNI", "VELSNI"
                               , "ECDRAI", "PHIRRI", "ECIRRI"))
  
  yoisList$PROCESSNAMES_IOT <- union(PROCESSNAMES_IOT
                            , c("growthRate", "plant weight m2"
                                , "drain", "drainLG"
                                , "waterSupply", "waterUptake", "slabweight"
                                , "ccJ"))
  
  if (length(DT)){
    yoisDT <- aphVariableLevels(DT)
    yoisList <- c(yoisList
                  , sapply(domains, \(patt) grep(patt
                                               , x = yoisDT
                                               , ignore.case = ignore.case
                                               , value = T)
                           , simplify = F))
  }
  if (crossWithDT){
    if (length(DT)){
      yoisList$PROCESSNAMES_W <- intersect(yoisList$PROCESSNAMES_W, yoisDT)
      yoisList$PROCESSNAMES_LG <- intersect(yoisList$PROCESSNAMES_LG, yoisDT)
      yoisList$PROCESSNAMES_IOT <- intersect(yoisList$PROCESSNAMES_IOT, yoisDT)
    }
  }
      
  yoisList
}
