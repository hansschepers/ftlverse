# Project: aph-aphLite
# 
# Author: ltuijnder
###############################################################################

#' PROCESSNAMES_W
#' @export
PROCESSNAMES_W <- c("stem.density.setting"
  , "setting.fruits.m2.wk"
  , "setting.fruits.m2.cu"
  , "harvested.fruits.m2.wk"
  , "harvested.fruits.m2.cu"
  , "harvested.truss.stem.wk"
  , "harvested.truss.stem.cu"
  , "harvestMaturity"
  , "yield", "afw", "brix"
)


#' PROCESSNAMES_LG
#' @export
PROCESSNAMES_LG <- c("RADJCM", "GHTEMP", "GHTEMPdiff"
                     , "RAD"
  # , "PARMOL"
  , "totalPAR"
  , "GHHUMI", "GHCO2C"
  , "OUTEMP", "OUHUMI"
  , "VEWSNI", "VELSNI"
  , "ECDRAI", "PHIRRI", "ECIRRI")


#' PROCESSNAMES_IOT
#' @export
PROCESSNAMES_IOT <- c("growthRate", "plant weight m2"
  , "drain", "drainLG"
  , "waterSupply", "waterUptake", "slabweight"
  , "ccJ")