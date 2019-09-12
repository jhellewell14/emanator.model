#' Model run
#' This function performs model runs with specified parameters. Some default parameters are specified to save repeating code. Some must
#' be specified each time to guarantee that parameter values aren't incorrectly passed.
#' @importFrom ICDMM create_r_model
#' @return A list of all of the different outputs from the model
#' @author Joel Hellewell
#' @keywords internal
mr <- function(em_cov,itn_cov,bites_Emanator,bites_Indoors,bites_Bed,
               r_EM0=0.6053263,em_loss=0.001954954,d_EM0=0,
               surv_bioassay=0,init_EIR,...){
  wh <- ICDMM:::create_r_model(odin_model_path = system.file("extdata/old_models/odin_model_emanators.R",package = "ICDMM"),
                                  het_brackets = 5,
                                  age = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,3.5,5,7.5,10,15,20,30,40,50,60,70,80),
                                  num_int = 4,
                                  init_EIR = init_EIR,
                                  init_ft = 0.8,
                                  country = NULL,
                                  admin2 = NULL,
                                  bites_Emanator = bites_Emanator,
                                  bites_Indoors = bites_Indoors,
                                  bites_Bed = bites_Bed,
                                  d_EM0 = d_EM0,
                                  r_EM0 = r_EM0,
                                  irs_cov = em_cov,
                                  em_cov = em_cov, # this is quick and dirty way of making emanator coverages work with model initialisation code
                                  itn_cov = itn_cov,
                                  EM_interval = 365,
                                  ITN_interval = 365*3,
                                  em_loss = em_loss,
                                  EM_on = 365*5,
                                  ITN_on = 365*5,
                                  surv_bioassay = surv_bioassay,...)

  mod <- wh$generator(user = wh$state,use_dde = TRUE)
  mod_run <- mod$run(t = 1:(365*14))
  out <- mod$transform_variables(mod_run)
}

#' Find EIR boundaries
#' This function finds the maximum pre-intervention EIR at which elimination can still occur for ITN or ITN+EM interventions. It systematically
#' searches until it has found the EIR to 3 decimal places.
#' @return The minimum EIR value
#' @author Joel Hellewell
#' @keywords internal
find_all_boundary <- function(r_EM0,em_loss,surv_bioassay,
                              bites_Emanator,bites_Indoors,bites_Bed,
                              em_cov,itn_cov){

  EIR_min <- find_EIR_boundary(step=1,EIR_min = 0,
                               r_EM0,em_loss,
                               surv_bioassay,
                               bites_Emanator,bites_Indoors,bites_Bed,
                               em_cov,itn_cov)

  # 2nd digit
  EIR_min <- find_EIR_boundary(step=0.1,EIR_min = EIR_min,
                               r_EM0,em_loss,
                               surv_bioassay=0,
                               bites_Emanator,bites_Indoors,bites_Bed,
                               em_cov,itn_cov)
  # 3rd digit
  EIR_min <- find_EIR_boundary(step=0.01,EIR_min = EIR_min,
                               r_EM0,em_loss,
                               surv_bioassay,
                               bites_Emanator,bites_Indoors,bites_Bed,
                               em_cov,itn_cov)
  # 4th digit
  EIR_min <- find_EIR_boundary(step=0.001,EIR_min = EIR_min,
                               r_EM0,em_loss,
                               surv_bioassay,
                               bites_Emanator,bites_Indoors,bites_Bed,
                               em_cov,itn_cov)
  return(EIR_min)
}

#' Finds the EIR boundary systematically for a given step size
#' @author Joel Hellewell
#' @return Minimum EIR value
#' @keywords internal
find_EIR_boundary <- function(step,EIR_min,r_EM0,em_loss,surv_bioassay,
                              bites_Emanator,bites_Indoors,bites_Bed,
                              em_cov,itn_cov){
  EIR_min <- ifelse(EIR_min==0,0.0001,EIR_min)
  EIR_max <- EIR_min + step
  elim <- FALSE
  while(elim==FALSE){
    print(paste("Trying min:",EIR_min))
    temp <- compare_elim(EIR_min,EIR_max,
                         r_EM0,em_loss,
                         surv_bioassay,
                         bites_Emanator,
                         bites_Indoors,
                         bites_Bed,
                         em_cov,
                         itn_cov)

    if(temp$min==TRUE & temp$max==FALSE){ # when EIR_min causes elimination but EIR_max does not
      elim<-TRUE
    }else{ # otherwise increase to next value
      EIR_min <- EIR_min + step
      EIR_max <- EIR_min + step
    }
  }
  return(round(EIR_min,3)) # return minimum (rounded to get rid of 0.0001 if you started at 0)
}

#' Performs model runs for two EIR values and returns whether they achieve elimination
#' @author Joel Hellewell
#' @keywords internal
compare_elim <- function(EIR_min,EIR_max,r_EM0,em_loss,surv_bioassay,
                         bites_Emanator,bites_Indoors,bites_Bed,
                         em_cov,itn_cov){

  # Model runs
  run_min <- mr(em_cov=em_cov,itn_cov=itn_cov,
                bites_Emanator=bites_Emanator,
                bites_Indoors=bites_Indoors,
                bites_Bed=bites_Bed,
                init_EIR=EIR_min,# EIR min
                r_EM0=r_EM0,em_loss=em_loss,
                surv_bioassay = surv_bioassay)

  run_max <- mr(em_cov=em_cov,itn_cov=itn_cov,
                bites_Emanator=bites_Emanator,
                bites_Indoors=bites_Indoors,
                bites_Bed=bites_Bed,
                init_EIR=EIR_max, # EIR max
                r_EM0=r_EM0,em_loss=em_loss,
                surv_bioassay = surv_bioassay)

  return(list(min=elim(run_min),max=elim(run_max))) # Returns TRUE/FALSE for each run if it eliminated
}

#' Returns whether a run has achieved elimination
#' @author Joel Hellewell
#' @return TRUE or FALSE depending on whether elimination is achieved
#' @keywords internal
elim <- function(mod_run){
  out <- FALSE
  for(j in 1:3234){
    # Checks for a period of 50 consecutive days when all-age prev is less than 0.0149%
    if(all(mod_run$allprev[((365*5)+j):((365*5)+j+50)] < 0.000149)==TRUE){out<-TRUE}
  }
  return(out)
}
