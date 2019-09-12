
 mr <- function(em_cov,itn_cov,bites_Emanator,bites_Indoors,bites_Bed,
                r_EM0=0.6053263,em_loss=0.001954954,d_EM0=0,
                surv_bioassay=0,init_EIR,...){
  wh <- hanojoel:::create_r_model(odin_model_path = system.file("extdata/old_models/odin_model_emanators.R",package = "hanojoel"),
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
                                  em_cov = em_cov, # this is a hack to make my old code work with the new model framework
                                  itn_cov = itn_cov,
                                  EM_interval = 365,
                                  ITN_interval = 365*3,
                                  em_loss = em_loss,
                                  EM_on = 365*5,
                                  ITN_on = 365*5,
                                  surv_bioassay = surv_bioassay,...)

  mod <- wh$generator(user = wh$state,use_dde = TRUE)
  mod_run <- mod$run(t = 1:(365*16))
  out <- mod$transform_variables(mod_run)
 }

