#' cyclist03Parms
#' this file contains parameters for a specific run used to overwrite default parameters values
#' data for Lance Armstrong from Coyle paper
#' @examples \dontrun{
#'   cyclist03Parms()
#' }
#' 
#' @export
cyclist03Parms <- function(pa = list()
){
  {
    pa_default <- c(
      # here parameters are named and given a default value
      # the values below come from p. 51 of the Stolwijk report (1971)
      CSW = 320,
      SSW = 29,
      PSW = 0.,
      BULL = 10,
      # the following values come from p. 54 of the mentioned report
      CDIL = 117,
      SDIL = 7.5,
      PDIL = 0.,
      # the following values come from p. 58 of the mentioned report
      CCHIL = 0,
      SCHIL = 0,
      PCHIL = 21,
      # the following values are inferred from p. 58, the expression for STRIC at the bottom, in conjunction
      # with p. 41
      CCON = 5,
      SCON = 5,
      PCON = 0,
      #
      treadmill = 0,
      T_air=20.,
      # V, air velocity (m/s) across body, used for convective transport in Stolwijk model
      # for the moment taken to blow from ahead and also determining wind contribution to air resistance
      # V
      v_wind = 0.1,
      # relative humidity
      RH = 0.5,
      BASAL_METABOLIC_RATE = 74.4,
      # WORK=74.4,
      
      # up till this point parameters come from Stolwijk equations and pertain to energy usage, blood flow,
      # heat transport, sweating, temperature control
      
      # energy supply from Olds model.
      # brainpush: exertion level the athlete reaches under voluntary control given fatigue and
      # exhaustion which (s)he experiences, given as a fraction of VO2max
      brainpush = 0.752, # from Olds (1995) paper
      # DeltaMechEff, Delta Efficiency as in Coyle's paper - here the default value is given from Stolwijk
      DeltaMechEff = 0.22,
      # VO2max in litre/min, STPD
      VO2max = 5.143,
      # VO2init, initial oxygen consumption, before start in litre/min, STPD
      VO2init = 0.681,
      # Mb = mass bicycle plus clothing (kg)
      Mb = 7.3, # minimum weight according to UCI rules (6.8 kg) + weight clothing (0.5 kg)
      # Ma, mass athlete (kg)
      # the heat capacities above can be made to depend on the body size of the athlete
      Ma = 75.29,
      # Ht, height athlete (cm)
      Ht = 179.3,
      
      # bicycle resistance parameters
      # rolling resistance parameter, Crr
      Crr = 0.0457,
      # air resistance reduction gain relative to assumed Cair, Olds (1993), di Prampero (1979)
      RedAir = 1,
      # slope of the road in meter/meter, i.e. rise/run
      S = 0.,
      # AreaRatio, projected frontal area/total body area, important to determine air resistance
      # AreaRatio = 0.25,
      # PB, barometric pressure (mmHg) taken at sea level
      PB = 760,
      # if reduction of barometric pressure should be calculated, set pa["VO2_red_by_alt"] = 1
      # pb_red_by_alt = 1,
      # if effect of reduction by altitude of barometric pressure must be calculated set next parameter to 1
      # VO2_red_by_alt = 1,
      # if reduction of air resistance by altitude must be calculated set next parameter 1
      # Air_res_red_by_alt = 1,
      # acceleration due to gravity, m / sec^2
      g = 9.807
    )
  }
  # ADD TIMING AND PLOT PARAMETERS TO THE PARAMETER ARRAY 'PA'
  # hsource1("Timing_and_Plot_Parameters.R")
  # {
  #   pa_default <- c( pa_default, #
  #                    # plot parameters
  #                    y_axis_min = 34.,
  #                    y_axis_max = 40.,
  #                    # timing parameters
  #                    tstep = 0.05, # gives time steps in hours for which integration results are desired during runin period,
  #                    tstep2 = 0.01, # gives time steps for which integration results are desired during simulation period,
  #                    tbegin = -0.1, # determines begin simulation and 'out' file - the cyclist starts cycling at t=0
  #                    tend = 0.67, # slightly more time than needed to reach finish of mountain time trial
  #                    trunin = 0.5, # determines runin time to obtain initial steady state
  #                    # the plotinterval determines the time points on the plot and writing results on computer screen
  #                    # works as a side effect of model function calls, does not interfere with tstep and tstep2 integration times
  #                    plotinterval = 10.005, #0.0002, #0.001,
  #                    # integration parameter hmax, see documentation for lsoda integration routine
  #                    hmax_nonPulsatile = 0.01,
  #                    hmax_Pulsatile = 0.0003,
  #                    Pulsatility = 1 # if there are no quick changes in simulation parameters or conditions: Pulsatility = 0,
  #                    # if there are quick changes (bursts) Pulsatility = 1
  #   )
  # }
  
  # LA 2024 Huez
  pa <- pa_default
  {
    # pa["WORK"]<- 74.4
    # guess for relative humidity during mountain time trial 2004 ########### TOO LOW !!!!!!!!!!!!!!!!!!!!!!
    # pa["RH"]<- 0.2
    # VO2max slightly higher than max measured in paper Coyle
    pa["VO2max"] <- 6.74
    # lowest estimate of body mass from Coyle paper
    pa["Ma"]<- 72.
    # height of athlete
    pa["Ht"]<- 178
    # pa["f"] <- 1.00
    # brainpush very high to obtain reasonable time
    pa["brainpush"]<- 0.95
    pa["RedAir"]<-0.95
    pa["DeltaMechEff"] <- 0.2312
    pa["PB"] <- 760 # barometric pressure
    # pa["pb_red_by_alt"] <- 1
    # pa["VO2_red_by_alt"] <- 1
    # pa["Air_res_red_by_alt"] <- 1
    
    # y-axis limits on plot
    # pa["y_axis_min"]<- 36.5
    # pa["y_axis_max"]<- 39.5
    
    # modified model parameters
    
    # small value for hmax to take changes in slope into account
    # pa["hmax_Pulsatile"] <- 0.0001
    
    # pa["T_air"]<- 25
    # pa["V"] <- 0.05
    pa["v_wind"] <- 0.05
    pa["hAH"] <- 0
    pa["Slope"] <- 0
    # pa["S"] <- 0
  }
  pa
}

#' cyclist03Constants
#' @export
cyclist03Constants <- function(){
  # if(F){
  #   stopifnot(dir.exists("src/ftlHES"))
  #   stopifnot(file.exists("src/ftlHES/Thermal_Parameters_Digital_Cyclist.R"))
  #   env_pars <- new.env()
  #   source("src/ftlHES/Thermal_Parameters_Digital_Cyclist.R", local = env_pars)
  #   env_pars <- as.list(env_pars)
  #   env_pars$i <- NULL
  #   names(env_pars)
  #   # env_pars$TSET
  #   env_pars
  # }
  
  CONSTANTS <- list()
  {
    # SETTING PARAMETERS - READ CONSTANTS CONTROLLED SYSTEM
    # In the following section default parameters and initial conditions are set
    # The values for node 1 through 25 are given in vectors
    # array of heat capacities in kcal / degree Celsius
    CONSTANTS$CC <- c(
      2.22,
      0.33,
      0.22,
      0.24,
      9.82, # with heat capacity of central blood subtracted from value Table 5 (Stolwijk), yielding value in Table 8
      16.15,
      4.25,
      1.21,
      1.41,
      3.04,
      0.58,
      0.43,
      0.14,
      0.06,
      0.09,
      0.17,
      4.24,
      9.17,
      1.43,
      1.08,
      0.23,
      0.06,
      0.13,
      0.22,
      2.25
    )
    
    # total body heat capacity
    CONSTANTS$CCbody_tot   <- sum(CONSTANTS$CC[1:25])
    # total skin heat capacity
    CONSTANTS$CCcore_tot   <- sum(CONSTANTS$CC[4 * 1:6 -3])
    CONSTANTS$CCmuscle_tot <- sum(CONSTANTS$CC[4 * 1:6 -2])
    CONSTANTS$CCfat_tot    <- sum(CONSTANTS$CC[4 * 1:6 -1])
    CONSTANTS$CCskin_tot   <- sum(CONSTANTS$CC[4 * 1:6   ])
    
    
    # basal metabolic heat production - in kcal / hour
    CONSTANTS$QB <- c(
      12.84,
      0.10,
      0.11,
      0.08,
      45.38,
      5.00,
      2.13,
      0.40,
      0.70,
      0.95,
      0.17,
      0.13,
      0.08,
      0.20,
      0.03,
      0.05,
      2.23,
      2.86,
      0.43,
      0.32,
      0.13,
      0.02,
      0.04,
      0.07
    )
    # has 24 element, no blood: blood has no metabolic heat production
    
    # basal evaporative heat loss in kcal / hour
    CONSTANTS$EB <- c(
      0.,
      0.,
      0.,
      0.63,
      9.00,  # lungs, trunk-core
      0.,
      0.,
      3.25,
      0.,
      0.,
      0.,
      1.20,
      0.,
      0.,
      0.,
      0.45,
      0.,
      0.,
      0.,
      2.85,
      0.,
      0.,
      0.,
      0.62  #HS bare feet??
    )
    # has 24 elements, no blood
    
    # basal effective blood flow in litre / hour
    CONSTANTS$BFB <- c(
      45.00,
      0.12,
      0.13,
      1.44,
      210.00,
      6.00,
      2.56,
      2.10,
      0.84,
      1.14,
      0.20,
      0.50,
      0.10,
      0.24,
      0.04,
      2.00,
      2.69,
      3.43,
      0.52,
      2.85,
      0.16,
      0.02,
      0.05,
      3.00
    )
    # has 24 elements, no blood
    
    # thermal conductance between compartment N and N+1 in kcal/hour/degree Celsius
    CONSTANTS$TC <- c(
      1.38,
      11.4,
      13.8,
      0.,
      1.37,
      4.75,
      19.80,
      0.,
      1.20,
      8.90,
      26.20,
      0.,
      5.50,
      9.65,
      9.90,
      0.,
      9.,
      12.4,
      64.,
      0.,
      14.0,
      17.7,
      14.1,
      0.
    )
    # has 24 elements, no blood
    
    # Surface area of skin chosen for men, not women. Unit m^2
    CONSTANTS$S <- c(
      0.1326,
      0.6804,
      0.2536,
      0.0946,
      0.5966,
      0.1299
    )
    
    # total surface area
    CONSTANTS$SA <- sum(CONSTANTS$S)
    
    # radiant heat transfer coefficient from skin, segment 1 through 6, in kcal / m^2 / hour / degree Celsius
    CONSTANTS$HR <- c(
      5.5,
      4.5,
      4.3,
      3.0,
      4.0,
      4.0
    )
    
    # convective and conductive heat transfer coefficient from skin, segment 1 through 6, in kcal / m^2 / hour / degree Celsius
    CONSTANTS$HC <- c(
      2.75,
      2.15,
      3.0,
      3.35,
      2.75,
      3.0
    )
    
    # vapour pressure table from 5 to 50 degrees Celsius (steps of 5 degrees), in mmHg
    # P0 <- exp(1.75+seq(5, 50, 5)*.055)
    # P <- c(
    #   6.541,
    #   9.205,
    #   12.78,
    #   17.51,
    #   23.69,
    #   31.71,
    #   42.02,
    #   55.13,
    #   71.66,
    #   92.30
    # )
    # 100*P0 / P -100
    
    # reference temperatures calculated for thermoneutral conditions in degree Celsius
    CONSTANTS$TSET <- c(
      Tbrain =      36.96,
      Thd_muscle =  35.07,
      Thd_fat =     34.81,
      Thd_skin =    34.58,
      Ttr_core =    36.89,
      Ttr_muscle =  36.28,
      Ttr_fat = 34.53,
      Ttr_skin = 33.62,
      Tarm_core = 35.53,
      Tarm_muscle = 34.12,
      Tarm_fat = 33.59,
      Tarm_skin = 33.25,
      Thand_core = 35.41,
      Thand_muscle = 35.38,
      Thand_fat = 35.30,
      Thand_skin = 35.22,
      Tleg_core = 35.81,
      Tleg_muscle = 35.30,
      Tleg_fat = 35.31,
      Tleg_skin = 34.10,
      Tfeet_core = 35.14,
      Tfeet_muscle = 35.03,
      Tfeet_fat = 35.11,
      Tfeet_skin = 35.04,
      Tblood = 36.71
    )
    
    # fraction of total skin receptors sending signals to central controller
    CONSTANTS$SKINR <- c(
      0.0695,
      0.4935,
      0.0686,
      0.1845,
      0.1505,
      0.0334
    )
    
    # fraction of sweating command applicable to skin of segment I
    CONSTANTS$SKINS <- c(
      0.081,
      0.481,
      0.154,
      0.031,
      0.218,
      0.035
    )
    
    # fraction of vasodilation command applicable to skin of segment I
    CONSTANTS$SKINV <- c(
      0.132,
      0.322,
      0.095,
      0.121,
      0.230,
      0.10
    )
    
    # fraction of vasoconstriction command applicable to skin of segment I
    CONSTANTS$SKINC <- c(
      0.05,
      0.15,
      0.05,
      0.35,
      0.05,
      0.35
    )
    
    # fraction of total work done by muscles in segment I. Present numbers apply to bicycle work
    CONSTANTS$WORKM <- c(
      0.,
      0.30,
      0.08,
      0.01,
      0.60,
      0.01
    )
    
    # fraction of total shivering occurring in muscles of segment I
    CONSTANTS$CHILM <- c(
      0.02,
      0.85,
      0.05,
      0.00,
      0.07,
      0.00
    )
  }
  CONSTANTS
}



#' get.vpd
#' @examples \dontrun{
#'   get.vpd(0, 20)
#'   get.vpd(50, 20)
#'   get.vpd(100, 20)
#' }
#' 
#' @export
get.vpd <- function(rh, temp){
  rh <- unname(rh)
  temp <- unname(temp)
  ## calculate saturation vapor pressure
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp)))
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}
