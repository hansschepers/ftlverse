#' cyclist04
#' driv_funs and elements of CONSTANTS are global!
#' @export
cyclist04 <- function(time = 0
                      , y
                      , pa) {
  # drivers ************************************************ ##################
  {
    for (poi in names(driv_funs$distance)){
      pa[poi] <- driv_funs$distance[[poi]](v = y[26])
    }
    for (poi in names(driv_funs$time)){
      pa[poi] <- driv_funs$time[[poi]](v = time)
    }
  }
  # .pa <<- pa
  # pa <- .pa
  # pa["T_air"]
  stopifnot(all(c("T_air", "hAH") %in% names(pa)))
  # par2par rewritings
  {
    # basal metabolic rate is taken proportional to body weight
    pa["BASAL_METABOLIC_RATE"] <- pa["BASAL_METABOLIC_RATE"] * pa["Ma"]/74.1
    
    # body surface area according to Dubois formula, Eq. 5, Olds et al., 1993
    pa["SurfaceArea"] <- pa["Ma"]^0.425 * pa["Ht"]^0.725*0.007184
    # the total surface area according to Stolwijk's model for the 74.1 kg man is almost equal to the
    # surface area calculated according to the Dubois formula.
    # surface areas of the segments are therefore not adjusted
    # CF_Swain, correction factor
    pa["RedAir"] <- (23.06 - 2.46*pa["SurfaceArea"])/18.7
  }
  
  # more aux not needing states...
  {
    # calculate the barometric pressure at that height
    Pbvar <- pa["PB"] * (1 - 0.0000225577 * pa["hAH"])^5.25588
    
    # next we calculate the muscular power (work per unit time)
    # available from the muscles of the athlete to sustain velocity against gravity and friction or to accelerate
    # it is assumed that 1 ml of O2 (STPD) corresponds to 20.9 J/ml, corresponding to a respiratory quotient
    # of 0.96 (Di Prampero, 1979)
    # 1 kcal (th) = 4184 Joule
    # 1000 ml/min = 20,900 J/min = 4.99522 kcal/min = 299.713 kcal/hr
    
    # at t<0 WORK is calculated from VO2init i.e. the initial rate of oxygen consumption
    # the amount of WORK above basal metabolic rate of 74.4 kcal/hr may be due to muscle tension,
    # increased physiological activity of digestive system etc.
    # of this internal work nothing is used for external work Pext (Watt)
    # Pext equals WR in the Olds, 1993 paper
    
    # if (phase == 0) {
    #   # if (t <= 0.) 
    #   # converting VO2init in litre O2 / min to WORK in kcal/hr
    pa["L2kcal"] <- 4.99522 #kcal/L
    
    # VO2_basal <- unname(pa["BASAL_METABOLIC_RATE"] / pa["L2kcal"] / 60)
    
    
    WORK0 <- pa["VO2init"] * pa["L2kcal"] * 60
    # pa$VO2init 
    WORK0 # kcal/hr
    #   # calculating WORKI from oxygen uptake measurements before starting to cycle
    WORKI0 <- WORK0 - pa["BASAL_METABOLIC_RATE"]
    #   Pext <- 0.
    # }
    
    kcal_per_W <- 3600/4184
    # print(kcal_per_W, digits = 6)
    
    # if (phase == 1) 
    {
      # if (t > 0.) 
      # nVO2 (normalized VO2max) =  0.4397485 + 0.0007373 * barometric pressure (mmHg)
      # here the barometric pressure calculated for that altitude is used : pa["Pbvar"]
      nVO2 <-  0.4397485 + 0.0007373 * Pbvar
      
      # what is the maximum part of VO2max that can be used for a certain period of time
      partVO2 <- nVO2 * pa["brainpush"] * pa["VO2max"]
      
      # what is the energy turnover in kcal/hr that is equivalent to partVO2
      WORKEQ <- max(pa["BASAL_METABOLIC_RATE"], partVO2 * pa["L2kcal"] * 60)
      
      
      #TODO reduce WORKEQ from shivering, heat production..?
      #TODO change WORKEQ by temperature? 
      
      # WORKEQ <- stress_cost_kcal
      WORK <- WORKEQ
      # increase in energy turnover over basal metabolic rate in kcal/hr
      WORKEQI <- WORKEQ - pa["BASAL_METABOLIC_RATE"]
      # the part of increase energy turnover available for mechanical work done by the athlete
      # Pext <- 0
      Pext <- WORKEQI * pa["DeltaMechEff"]/kcal_per_W # in Watt, calculated from Delta mechanical efficiency
      # WORKI is the heat generation above baseline level associated with the muscle work performed
      WORKI <-  WORKEQI - Pext* kcal_per_W # converting 1 Watt to kcal_per_W kcal/hr
    }
  }
  
  #################################################################### TEMPERATURE for each compartment ############
  ydot <- matrix(0, nrow=27, ncol=1)
  
  # v_cycle in m/s is calculated from y[27] in m / hr ; pa["v_wind"], frontal wind speed in m/s
  v_cycle <- y[27] / 3600
  # pa["v_wind"] <- pa["V"]
  
  # combined heat transfer coefficient radiative (HR) plus convective (HC)
  # length = 6
  H <- (HR + HC * ((pa["v_wind"] + v_cycle) / 0.1)^0.5) * S
  
  P_air <- get.vpd(pa["RH"], pa["T_air"])
  
  # construct vectors which are going to be filled in iterative loops
  error <- numeric(length = 25)
  cold <- numeric(length = 25)
  warm <- numeric(length = 25)
  qq <- numeric(length = 24)
  bf <- numeric(length = 24)
  e <- numeric(length = 24)
  pskin <- numeric(length = 6)
  emax <- numeric(length = 6)
  # bc <- numeric(length = 24)
  td <- numeric(length = 24)
  hf <- numeric(length = 25)
  
  
  # error etc is calculated in all layers
  for (n in 1:25) {
    # if (FF[n] >= 0.) FF[n] <<- 0.
    # note that RATE is always zero in the present model
    error[n] <- y[n] - TSET[n] # RATE[n]*FF[n]
    # in contrast to the Stolwijk report (1971) cold[n] is set to -error[n]
    # this seems in line with the use of cold[n] in the control equations for sweat, dilat, stric, chill
    if(error[n] <= 0.) { cold[n] <- -error[n]; warm[n] <- 0. }
    if(error[n] >  0.) { warm[n] <-  error[n]; cold[n] <- 0. }
    
    # if(error[n]==0.) {cold[n]<- 0.; warm[n] <- 0.}
  }
  
  # error etc was calculated in all layers, but used only for skin?    
  warms <- 0.
  colds <- 0.
  for (i in 1:6) {
    k <- 4*i
    warms <- warms + warm[k]*SKINR[i]
    colds <- colds + cold[k]*SKINR[i]
  }
  
  sweat <-  pa["CSW"]  *error[1] + pa["SSW"]  *(warms - colds) + pa["PSW"]  *warm[1]*warms
  dilat <-  pa["CDIL"] *error[1] + pa["SDIL"] *(warms - colds) + pa["PDIL"] *warm[1]*warms
  stric <- -pa["CCON"] *error[1] - pa["SCON"] *(warms - colds) + pa["PCON"] *cold[1]*colds
  chill <- -pa["CCHIL"]*error[1] - pa["SCHIL"]*(warms - colds) + pa["PCHIL"]*cold[1]*colds
  
  if (sweat < 0.) sweat <- 0.
  if (dilat < 0.) dilat <- 0.
  if (stric < 0.) stric <- 0.
  if (chill < 0.) chill <- 0.
  
  for (i in 1:6) {
    n <- 4*i - 3
    
    # heat production qq : we use qq instead of q because the latter may have a special meaning in R
    qq[n] <- QB[n]
    # blood flow bf (NO REDUCTION WHEN DOING EXERCISE ? e.g. IN TRUNK)
    bf[n] <- BFB[n] # basal effective blood flow in litre / hour
    # evaporation e (only non-zero for lungs when i == 2)
    e[n] <- EB[n]
    
    ############################## muscle
    # heat production associated with muscle work and with shivering (chill) are added to the basal heat rate
    exercise <- WORKM[i]*WORKI
    shiver <- CHILM[i]*chill
    # exercise_shiver <- WORKM[i]*WORKI + CHILM[i]*chill
    qq[n+1] <- QB[n+1] + exercise + shiver
    e[n+1] <- 0.0
    bf[n+1] <- BFB[n+1] + exercise + shiver # was: qq[n+1] - QB[n+1]
    
    ############################## fat
    qq[n+2] <- QB[n+2]
    e[n+2] <- 0.
    bf[n+2] <- BFB[n+2]
    
    ############################## skin
    qq[n+3] <- QB[n+3]
    e[n+3] <- (EB[n+3] + SKINS[i]*sweat)*2.^(error[n+3]/pa["BULL"])
    bf[n+3] <- ((BFB[n+3]+SKINV[i]*dilat)/(1.0+SKINC[i]*stric))*2.0^(error[n+3]/10.0)
    
    
    pskin[i] <- get.vpd(rh = pa["RH"], temp = y[n+3])
    # pskin[i] <- P[k] + (P[k+1] - P[k])*(y[n+3]-5*k)/5.
    emax[i] <-           (pskin[i] - P_air) * 2.2 * (H[i] - HR[i] * S[i])
    # emax[i] <-  -max(0, pskin[i] - P_air) * 2.2 * (H[i] - HR[i] * S[i])
    if (e[n+3] > emax[i]) {
      e[n+3] <- emax[i]
    }
    # e[n+3] <- pmin(e[n+3], emax[i])
  }
  # .pskin <<- pskin
  # .P_air <<- P_air
  
  # note Stolwijk's choice for 44.0; this might be P_H2O based on central blood temperature
  # or trunk core temperature
  # lung cooling energy
  e[5] = (pa["BASAL_METABOLIC_RATE"] + WORKI) * 0.0023 * max(0, 44.0 - P_air)
  
  # energy to the blood (kcal/hr?)
  bc <- bf * (y[1:24] - y[25])
  
  # heat diffusion from inner layer to outer layer (td)
  # same but slower??!!
  # td <- TC * rev(-diff0(rev(y)))[1:24]
  for (k in 1:24) {
    td[k] = TC[k] * (y[k] - y[k+1])
  }
  
  # qq: heat production (metabolism)
  # bc: from organ to blood
  # td: heat diffusion from inner layer to outer layer
  # e:  energy loss from evaporation (skin & lungs)
  #
  for (i in 1:6) {
    k <- 4*i - 3
    hf[k]   <- qq[k]   - bc[k]   - td[k]            - e[k]
    hf[k+1] <- qq[k+1] - bc[k+1] + td[k]   - td[k+1]
    hf[k+2] <- qq[k+2] - bc[k+2] + td[k+1] - td[k+2]
    hf[k+3] <- qq[k+3] - bc[k+3] + td[k+2]          - e[k+3] - H[i]*(y[k+3] - pa["T_air"])
  }
  # blood heatup from rest
  hf[25] <- sum(bc[1:24])
  
  # the rate of change of temperature is the total local heat flux divided by the thermal capacity
  ydot[1:24] <- hf[1:24] / CC[1:24]
  # rate of change of central blood temperature
  ydot[25] <- hf[25] / CC[25]
  
  # e/emax, weighted by surface
  ewet <- sum(e[4* 1:6] / emax[1:6] * S[1:6], na.rm = TRUE) / SA
  # the fractional wetted area of the skin, pwet, is calculated in %
  pwet <- 100 * ewet
  
  co <- sum(bf)/60
  hp <- sum(qq)
  ev <- sum(e)
  
  sbf     <- sum(bf[4*1:6])/60
  #   net rate of heat storage for the whole body
  hflow   <- sum(hf[1:25])
  #   weighted-average body temperature
  tbody   <- sum(y[1:25] * CC) / CCbody_tot
  tskin   <- sum(y[4* 1:6   ] * CC[4 * 1:6   ]) / CCskin_tot
  tfat    <- sum(y[4* 1:6-1 ] * CC[4 * 1:6-1 ]) / CCfat_tot
  tmuscle <- sum(y[4* 1:6-2 ] * CC[4 * 1:6-2 ]) / CCmuscle_tot
  tcore   <- sum(y[4* 1:6-3 ] * CC[4 * 1:6-3 ]) / CCcore_tot
  
  ev <- ev/SA   # total evaporation per surface area
  hp <- hp/SA
  hflow <- hflow/SA
  cond <- (hp - e[5] / SA - hflow) / (y[25] - tskin)  # lung
  
  
  ####################################################### DISTANCE CHANGE ( = VELOCITY) #######################
  # here the non-thermal model variables are calculated
  # calculate distance y[26] covered by athlete given in meters
  ydot[26] <- y[27]
  
  ####################################################### VELOCITY CHANGE (ACCELLERATION) #######################
  # air resistance:
  #
  # pa["RedAir"] takes into account reduction of air resistance above situation taken in Olds (1993) paper
  # coming from Di Prampero.
  # Pair is the power dissipated against air resistance increased by head wind
  Pair <- pa["RedAir"] * 0.19*(288/(pa["T_air"]+273.15)) * 
    (pa["SurfaceArea"]/1.77) * (v_cycle + pa["v_wind"])^2.* v_cycle
  # if (pa["Air_res_red_by_alt"] == 1)  {
  Pair <- Pair * (Pbvar/755)
  # }
  
  if ( (v_cycle + pa["v_wind"]) < 0.) {
    #   Pair <- Pair
    # } else { 
    Pair <- -Pair 
  } # assuming that with net wind velocity blowing from behind extra power becomes available
  
  
  # if (phase == 1) 
  {
    # the force to overcome rolling resistance (Olds, 1993) is multiplied by the ground speed
    atanSlope <- atan(pa["Slope"])
    
    # total mass of athlete plus bicycle
    # in function because the mass of the athlete can be made time dependent because of sweat loss
    totalM <- pa["Ma"] + pa["Mb"]
    
    Prolling <- pa["Crr"] * cos(atanSlope)* totalM * v_cycle
    # the force to climb against gravity is multiplied by ground speed
    Fgrade <- totalM * pa["g"] * sin(atanSlope)
    Pgrade <- Fgrade * v_cycle
    
    # ACCELERATION
    # the available power for acceleration, Pacc in Watt, is calculated
    Pacc <- Pext - Pair - Prolling - Pgrade
    # Pacc can be constrained to provide forward thrust
    if(Pacc < 0) {
      Pacc <- 0
      if (!exists("ii_negative_time")) ii_negative_time <<- 0
      if (ii_negative_time %% 50 == 0){
        log_warn(" *********************** Warning: Pacc was less than zero at time = {time}")
        timeOfLastNegativeAccelleration <<- time
        ii_negative_time <- ii_negative_time + 1
        ii_negative_time <<- ii_negative_time
      }
    }
    # count all calls to this function... ############################
    if (!exists("iiggtime") | !exists("iiggdata")) {
      iiggtime <<- 1
      iiggdata <<- data.table(ind = 1:1e5, timeCalc = NA_real_)
    } else {
      iiggtime <- iiggtime + 1
      iiggdata[iiggtime, timeCalc := time]
      iiggtime <<- iiggtime
      iiggdata <<- iiggdata
    }
    
    # the force for acceleration, Facc, is calculated
    # to avoid division by zero, giving infinite force, Facc is limited to 800 N, about 80 kg per leg,
    # at startup, v = 0 to 1 m/sec
    # if v gets higher, higher forces are possible reaching 1200 N at v = 5 m/s
    # THE FOLLOWING GROUP OF STATEMENTS MAY REQUIRE FUTURE IMPROVEMENT
    # warning message if speed becomes negative: a real cyclist would presumably dismount in that case
    # unless (s)he recovers very quickly
    if (v_cycle < 0.) {
      log_warn("Negative speed, equals ", v_cycle," m/sec")
    }
    # from the available power the force available for acceleration is calculated. This is the total force
    # minus the force for rolling resistance, air resistance and gravity
    # this force may be negative and lead to deceleration
    # the negative force leads to negative work (on the muscle) which is accompanied by increased
    # metabolism (eccentric work)
    # whether the latter effect on metabolism exists under these circumstances is unresolved
    # it has not yet been incorporated in the model
    
    # for all speeds different from zero, the available force for acceleration is calculated
    Facc <- 800 - Fgrade
    if (abs(v_cycle) <= 0.001) {
      Facc <- 800 - Fgrade
    } else {
      Facc <- Pacc / v_cycle
    }
    # the force for acceleration is limited by maximal values for the force which depend on the speed of the cyclist
    # this should perhaps be the rotation speed of the legs in a more accurate implementation
    if (v_cycle <= 1.) {
      Facc_max <- 800 - Fgrade
    }
    if (v_cycle >= 1. && v_cycle < 5.) {
      Facc_max <- 800 + (1200 - 800) * (v_cycle - 1.)/4. - Fgrade
    }
    if (v_cycle >= 5) {
      Facc_max <- 1200 - Fgrade
    }
    Facc <- pmin(Facc, Facc_max)
    
    if (Pacc < 0. && v_cycle < 0.) {
      Facc <- -Pacc / v_cycle
    }
    # the acceleration, acc, is calculated; it is taken into account that the model integrates in hours, while
    # the calculation in the power balance part gives acceleration in m / sec^2
    # y[27] is given in m / hr
    acc <- Facc / totalM
    # the acceleration in m/sec^2 is converted to m/hr^2 by multiplying by 3600^2 = 12960000  
    ydot[27] <- acc * 12960000
  }
  # ydot <- pmax(ydot, -10*y)
  
  # assemble a vector of function values and transport rates at this time point (=t) for output
  functionValuesOut <- c(     #first variables from the Stolwijk model are added
    sweat = unname(sweat)
    , lungCooling = unname(e[5])
    , emax_tr = unname(emax[2])
    , chill = unname(chill)
    , co = unname(co)
    , Facc_max = unname(Facc_max)
    , Fgrade = unname(Fgrade)
    , hp = unname(hp)
    , ev = unname(ev)
    , tskin = unname(tskin)
    , tfat = unname(tfat)
    , tmuscle = unname(tmuscle)
    , tcore = unname(tcore)
    , tbody = unname(tbody)
    , hflow = unname(hflow)
    , cond = unname(cond)
    , pwet = unname(pwet)
    , dilat = unname(dilat)
    , stric = unname(stric)
    , skinbf = unname(sbf)
    
    # speed, distance, altitude, slope grouped together
    , road_distance = unname(y[26]/1000)
    , speed = unname(y[27])/1000
    , Slope = unname(pa["Slope"])
    
    # other variables
    , p_barometric = unname(Pbvar)
    , WORKI = unname(WORKI)
    , Pext = unname(Pext)
    , Pair = unname(Pair)
    , Prolling = unname(Prolling)
    , Pgrade = unname(Pgrade)
    , Pacc = unname(Pacc)
    , Facc = unname(Facc)
    , acc = unname(acc)
    #
    # variables which are calculated (postprocessing)
    , sum_Pext = unname(Pair + Prolling + Pgrade + Pacc)
    , heat_production_Watt = unname(hp * 1.1622 * SA)
    , sum_Watt = unname(Pair + Prolling + Pgrade + Pacc + hp * 1.1622 * SA)
    , P_air = unname(P_air)
    , T_air = unname(pa["T_air"])
    , Altitude = unname(pa["hAH"]/1000)
    , brainpush = unname(pa["brainpush"])
  )
  
  # gg <- c(gg, list(a = as.data.table(c(list(time = t)
  #                                      , as.list(y)
  #                                      , as.list(setNames(as.vector(ydot), paste0("d_", namesOutputVariables)))
  #                                      ))))
  # gg <<- gg
  
  list(ydot, functionValuesOut)
}
