# On load functions. Sets up global parameters for emanators from (summarised) Tanzanian trial data

# source("~/Dropbox/PhD/emanators/phiB reanalysi.R")
.onLoad <- function(libname,pkgname){
 # Enanator effectiveness at different distances
 # tan_stdy <- data.frame(dist=c(0,2,5,10,20),averted=c(83,33,40,19,2),lb=c(70,0.61,10,-23,-48),ub=c(91,56,60,47,35))
 #
 # # Simple regression
 # x1 <- tan_stdy$dist
 # em <- lm(log(tan_stdy$averted)~x1)
 # my_D <- function(x){exp(predict(em,list("x1"=x)))/100}
 #
 # # Distribution of population distance
 # half_within_m <- 1
 # my_C <- function(x){dexp(x,rate=log(2)/half_within_m)}
 #
 # my_fun <- function(x){
 #  my_D(x)*my_C(x)
 # }
 #
 # r_EM0 <- integrate(f=my_fun,upper=Inf,lower=0)$value[[1]]
 # d_EM0 <- 0
 # em_loss <- 0.001954954
 #
 # # Parameters that will not change across runs
 # init_age <- c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,3.5,5,7.5,10,15,20,30,40,50,60,70,80)
 # ft <- 0.8
 # ITN_on <- 365*5
 # EM_on <- 365*5
 # num_int <- 4
 # bed_to_indoors_ratio <- 0.7851768/0.8908540
 # country <- NULL
 # admin2 <- NULL
 #
 # # function for running the deterministic malaria model with varying parameters
 #
 # mr <- function(em_cov,itn_cov,...){
 #  wh <- hanojoel:::create_r_model(odin_model_path = system.file("extdata/old_models/odin_model_emanators.R",package = "hanojoel"),
 #                                  het_brackets = 5,
 #                                  age = init_age,
 #                                  num_int = num_int,
 #                                  init_EIR = init_EIR,
 #                                  init_ft = init_ft,
 #                                  country = NULL,
 #                                  admin2 = NULL,
 #                                  bites_Emanator = bites_Emanator,
 #                                  bites_Indoors = bites_Indoors,
 #                                  bites_Bed = bites_Bed,
 #                                  d_EM0 = 0,
 #                                  r_EM0 = r_EM0,
 #                                  irs_cov = em_cov,
 #                                  em_cov = em_cov, # this is a hack to make my old code work with the new model framework
 #                                  itn_cov = itn_cov,
 #                                  EM_interval = EM_interval,
 #                                  em_loss = em_loss,
 #                                  EM_on = EM_on,
 #                                  ITN_on = ITN_on,
 #                                  surv_bioassay = surv_bioassay,...)
 #
 #  mod <- wh$generator(user = wh$state,use_dde = TRUE)
 #  mod_run <- mod$run(t = 1:(365*16))
 #  out <- mod$transform_variables(mod_run)
 # }
 #
 # print("hello")
}
