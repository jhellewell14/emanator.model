#' Plot Figure 4a
#'
#' This performs 9 model runs in total
#' @importFrom ggplot2 ggplot geom_line xlab ylab scale_x_continuous theme_bw scale_x_continuous scale_y_continuous geom_abline theme
#' @importFrom wesanderson wes_palette
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @return plot
#' @author Joel Hellewell
#' @export
figure4a <- function(){

  bed_to_indoors_ratio <- 0.7851768/0.8908540
  init_EIR <- 30
  outdoor_exp_low <- c()
  outdoor_exp_high <- c()
  k <- 1


  # Model runs

  for(i in 1:9){
    print(paste("Run",k,"of 9"))
    bites_Emanator <- seq(0,0.4,0.05)[i]
    bites_Indoors <- 1 - bites_Emanator
    bites_Bed <- bites_Indoors*bed_to_indoors_ratio
    init_EIR <- 30

    itnonly <- mr(itn_cov=0.5,em_cov=0,
                  bites_Emanator=bites_Emanator,
                  bites_Indoors=bites_Indoors,
                  bites_Bed=bites_Bed,
                  init_EIR=init_EIR,Q0=0.16)
    itnonly2 <- mr(itn_cov=0.8,em_cov=0,
                   bites_Emanator=bites_Emanator,
                   bites_Indoors=bites_Indoors,
                   bites_Bed=bites_Bed,
                   init_EIR=init_EIR,Q0=0.16)

    r_EM0 <- 1
    em_loss <- 0
    no_outdoor <- mr(itn_cov=0.5,em_cov=1,
                     bites_Emanator=bites_Emanator,
                     bites_Indoors=bites_Indoors,
                     bites_Bed=bites_Bed,
                     init_EIR=init_EIR,
                     r_EM0 = 1,em_loss=0,Q0=0.16)
    no_outdoor2 <- mr(itn_cov=0.8,em_cov=1,
                      bites_Emanator=bites_Emanator,
                      bites_Indoors=bites_Indoors,
                      bites_Bed=bites_Bed,
                      init_EIR=init_EIR,
                      r_EM0 = 1,em_loss=0,Q0=0.16)

    outdoor_exp_low[i] <- sum(itnonly$inc05[(365*5):(365*6)]-no_outdoor$inc05[(365*5):(365*6)])/sum(itnonly$inc05[(365*5):(365*6)])
    outdoor_exp_high[i] <- sum(itnonly2$inc05[(365*5):(365*6)]-no_outdoor2$inc05[(365*5):(365*6)])/sum(itnonly2$inc05[(365*5):(365*6)])
    k <- k + 1
  }

  # Output for gambiae-like mosquito
  gamb_df <- data.frame(
    x = seq(0,0.4,0.05),
    gamb_out_low = c(0.00000000,0.09174768,0.17582614,0.25186418,0.32233376,0.38625619,0.44475916,0.49877595,0.54799187),
    gamb_out_high = c(0.0000000,0.1113744,0.2150535,0.3121984,0.4024373,0.4815407,0.5515633,0.6123888,0.6654937))

  # Plot output
  yo <- wesanderson::wes_palette(name="IsleofDogs1",n=2)

  p <- data.frame(x=seq(0,0.4,0.05),outdoor_exp_low,outdoor_exp_high) %>% ggplot() +
    geom_line(aes(x,outdoor_exp_low),size=1.2,col=yo[[1]],lty=2) +
    geom_line(aes(x,outdoor_exp_high),size=1.2,col=yo[[2]],lty=2) +
    geom_line(data=gamb_df,aes(x=x,y=gamb_out_low),size=1.2,col=yo[[1]]) +
    geom_line(data=gamb_df,aes(x=x,y=gamb_out_high),size=1.2,col=yo[[2]]) +
    theme_bw() +
    scale_x_continuous(breaks=seq(0,1,0.1),labels=paste(seq(0,100,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,1,0.1),labels=paste(seq(0,100,10),"%",sep="")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size=18),
          axis.title = element_text(size=20)) +
    theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18,axis_title_just = "centre") +
    xlab("Proportion of feeding attempts that happen outdoors") +
    ylab("Percentage of cases due to bites happening outdoors") +
    geom_abline(slope = 1,intercept = 0,lty=2)

  return(p)
}

#' Plot Figure 4b
#'
#' This performs 4 model runs in total
#' @importFrom ggplot2 ggplot geom_line geom_hline coord_cartesian scale_y_continuous scale_x_continuous geom_vline xlab ylab
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @return plot
#' @author Joel Hellewell
#' @export
figure4b <- function(){

  bites_Indoors <- 0.8
  bites_Emanator <- 1 - bites_Indoors
  bites_Bed <- 0.8813754*bites_Indoors # this is bed to indoors ratio
  init_EIR <- 4.3

  nr1 <- mr(em_cov=0,itn_cov=0.8,
            bites_Emanator=bites_Emanator,
            bites_Indoors=bites_Indoors,
            bites_Bed=bites_Bed,
            init_EIR=init_EIR) # ITN only

  both <- mr(em_cov=0.8,itn_cov=0.8,
             bites_Emanator=bites_Emanator,
             bites_Indoors=bites_Indoors,
             bites_Bed=bites_Bed,
             init_EIR=init_EIR) # ITN + EM

  nr1low <- mr(em_cov=0,itn_cov=0.8,
            bites_Emanator=bites_Emanator,
            bites_Indoors=bites_Indoors,
            bites_Bed=bites_Bed,
            init_EIR=init_EIR,Q0=0.16) # ITN only

  bothlow <- mr(em_cov=0.8,itn_cov=0.8,
             bites_Emanator=bites_Emanator,
             bites_Indoors=bites_Indoors,
             bites_Bed=bites_Bed,
             init_EIR=init_EIR,Q0=0.16) # ITN + EM

  p <- data.frame(x=1:(365*14),itn=nr1$prev,em=both$prev,itnlow=nr1low$prev,emlow=bothlow$prev) %>%
    ggplot(aes(x=x)) + theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18,axis_title_just = "centre") +
    geom_hline(yintercept=c(0.01,0.08224412),lty=2,alpha=0.6)+
    geom_line(aes(y=itn)) +
    geom_line(aes(y=em),col="red") +
    geom_line(aes(y=itnlow),lty=2) +
    geom_line(aes(y=emlow),col="red",lty=2) +
    #geom_hline(yintercept=c(0.000149,0.08224412),lty=2) +
    coord_cartesian(xlim=c((365*4):(361*14))) +
    scale_y_continuous(breaks=seq(0,0.08,0.02),labels=paste(seq(0,8,2),"%",sep="")) +
    scale_x_continuous(breaks=seq((365*5),(365*14),(365*3)),labels=paste(seq(0,9,3),sep="")) +
    geom_vline(xintercept=seq((365*5),(365*14),(365*3)),lty=2,alpha=0.5,col="blue") +
    xlab("Years since initial intervention") +
    ylab("Slide prevalence (0-5 year-olds)")

  return(p)
}

#' Plot Figure 4c
#'
#' This performs 330 model runs in total, it will take some time.
#' @importFrom ggplot2 ggplot geom_tile xlab ylab scale_x_continuous
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom viridis scale_fill_viridis
#' @importFrom hrbrthemes theme_ipsum_ps
#' @return plot
#' @author Joel Hellewell
#' @export
figure4c <- function(){

  dm <- matrix(0,ncol=11,nrow=30)

  for(j in 0:10){ # outdoor exposure 0%->50%
    for(i in 1:30){ # EIR 1->30
      init_EIR <- i
      bites_Indoors <- 1-(0.05*j)
      bites_Emanator <- 1 - bites_Indoors
      bites_Bed <- 0.8813754*bites_Indoors

      # ITN only
      nr1 <- mr(em_cov=0,itn_cov=0.8,
                bites_Emanator=bites_Emanator,
                bites_Indoors=bites_Indoors,
                bites_Bed=bites_Bed,
                init_EIR=init_EIR)
      # ITN + EM
      both <- mr(em_cov=0.8,itn_cov=0.8,
                 bites_Emanator=bites_Emanator,
                 bites_Indoors=bites_Indoors,
                 bites_Bed=bites_Bed,
                 init_EIR=init_EIR)

      # change in incidence per 1000 0-5 year olds
      dm[i,j+1] <- sum(nr1$inc05[(365*5):(365*6)]-both$inc05[(365*5):(365*6)])*1000
      print(paste("Model run",(j)*30+i,"of 330"))
    }
  }

  # Plot output
  ca <- data.frame(z=as.vector(dm[,-1]),y=rep(1:30,10),x=rep(seq(0.05,0.5,0.05),rep(30,10)))
  p <- ca %>% ggplot() + geom_tile(aes(x,y,fill=z)) +
    ylab("Infectious bites per year pre-intervetion") +
    xlab("Proportion of outdoor biting exposure pre-intervention") +
    scale_x_continuous(breaks=seq(0.05,0.5,0.05),labels=paste(seq(5,50,5),"%",sep="")) +
    scale_fill_viridis(name="Cases per 1000 \n0-5 year olds \naverted in first year",
                       breaks=seq(0,270,30),labels=seq(0,270,30),limits=c(0,270)) + theme_minimal() +
    theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18)
  p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  return(p)
}

#' Plot Figure 4d
#'
#' This performs 330 model runs in total, it will take some time.
#' @importFrom ggplot2 ggplot geom_tile xlab ylab scale_x_continuous
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom viridis scale_fill_viridis
#' @importFrom hrbrthemes theme_ipsum_ps
#' @return plot
#' @author Joel Hellewell
#' @export
figure4d <- function(){

  dm <- matrix(0,ncol=11,nrow=30)

  for(j in 0:10){ # outdoor exposure 0%->50%
    for(i in 1:30){ # EIR 1->30
      init_EIR <- i
      bites_Indoors <- 1-(0.05*j)
      bites_Emanator <- 1 - bites_Indoors
      bites_Bed <- 0.8813754*bites_Indoors

      # ITN only
      nr1 <- mr(em_cov=0,itn_cov=0.8,
                bites_Emanator=bites_Emanator,
                bites_Indoors=bites_Indoors,
                bites_Bed=bites_Bed,
                init_EIR=init_EIR,Q0=0.16)
      # ITN + EM
      both <- mr(em_cov=0.8,itn_cov=0.8,
                 bites_Emanator=bites_Emanator,
                 bites_Indoors=bites_Indoors,
                 bites_Bed=bites_Bed,
                 init_EIR=init_EIR,Q0=0.16)

      # change in incidence per 1000 0-5 year olds
      dm[i,j+1] <- sum(nr1$inc05[(365*5):(365*6)]-both$inc05[(365*5):(365*6)])*1000
      print(paste("Model run",(j)*30+i,"of 330"))
    }
  }

  # Plot output
  ca <- data.frame(z=as.vector(dm[,-1]),y=rep(1:30,10),x=rep(seq(0.05,0.5,0.05),rep(30,10)))
  p <- ca %>% ggplot() + geom_tile(aes(x,y,fill=z)) +
    ylab("Infectious bites per year pre-intervetion") +
    xlab("Proportion of outdoor biting exposure pre-intervention") +
    scale_x_continuous(breaks=seq(0.05,0.5,0.05),labels=paste(seq(5,50,5),"%",sep="")) +
    scale_fill_viridis(name="Cases per 1000 \n0-5 year olds \naverted in first year",
                       breaks=seq(0,270,30)) + theme_minimal() +
    theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18)
  p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  return(p)
}

#' Plot Figure 4e
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of outdoor exposure.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure4e <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  ITN_optEM_elim <- c()
  itn_cov <- 0.8

  for(i in 1:5){ #10% -> 50% outdoor biting exposure
    print(paste("Outdoor exposure = ",i*0.1))
    bites_Emanator <- i*0.1
    bites_Indoors <- 1 - bites_Emanator
    bites_Bed <- 0.8813754*bites_Indoors
    em_cov <- 0
    # ITN elimination
    print("ITN")
    ITN_elim[i] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                     surv_bioassay=0,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.92,d_EM0=0)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                        surv_bioassay=0,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.92,d_EM0=0)

    print("ITN + optimum EM")
    ITN_optEM_elim[i] <- find_all_boundary(r_EM0=1,em_loss=0,
                                        surv_bioassay=0,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.92,d_EM0=0)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0.1,0.5,0.1),low=ITN_elim*100,high=ITN_EM_elim*100,opt=ITN_optEM_elim*100) %>%
    ggplot() + coord_cartesian(ylim=c(0,20)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=20,ymin=high),fill=hi[1]) +
    geom_line(aes(x=x,y=opt),lty=2,col=hi[2],size=1.2) +
    geom_hline(yintercept=seq(0,20,2),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0.1,0.5,0.1),labels=paste(seq(10,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,20,2),labels=paste0(seq(0,20,2),"%")) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18) +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Proportion of bites during the evening coverage gap") +
    ylab("Slide prevalence in 0-5 year olds pre-intervention")

  return(p)
}

#' Plot Figure 4f
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of outdoor exposure.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure4f <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  ITN_optEM_elim <- c()
  itn_cov <- 0.8

  for(i in 1:5){ #10% -> 50% outdoor biting exposure
    print(paste("Outdoor exposure = ",i*0.1))
    bites_Emanator <- i*0.1
    bites_Indoors <- 1 - bites_Emanator
    bites_Bed <- 0.8813754*bites_Indoors
    em_cov <- 0
    # ITN elimination
    print("ITN")
    ITN_elim[i] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                     surv_bioassay=0,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.16,d_EM0=0)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                        surv_bioassay=0,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.16,d_EM0=0)

    ITN_optEM_elim[i] <- find_all_boundary(r_EM0=1,em_loss=0,
                                           surv_bioassay=0,
                                           bites_Emanator,bites_Indoors,bites_Bed,
                                           em_cov,itn_cov,Q0=0.16,d_EM0=0)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0.1,0.5,0.1),low=ITN_elim*100,high=ITN_EM_elim*100,opt=ITN_optEM_elim*100) %>%
    ggplot() + coord_cartesian(ylim=c(0,20)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=20,ymin=high),fill=hi[1]) +
    geom_line(aes(x=x,y=opt),lty=2,col=hi[2],size=1.2) +
    geom_hline(yintercept=seq(0,20,2),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0.1,0.5,0.1),labels=paste(seq(10,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,20,2),labels=paste0(seq(0,20,2),"%")) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18) +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Proportion of bites during the evening coverage gap") +
    ylab("Slide prevalence in 0-5 year olds pre-intervention")

  return(p)
}

#' Plot Figure supp 1
#' This performs no model runs.
#' @importFrom wesanderson wes_palette
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_hline geom_vline scale_x_continuous xlab ylab scale_y_continuous
#' @return plot
#' @author Joel Hellewell
#' @export
figuresupp1 <- function(){

  r_EM0 <- 0.6053263

  # Probability that LLINs kill a mosquito depending on bioassay survival
  ERG_d_ITN0<-c(0.5100000,0.5068975,0.5036445,0.5002420,0.4966923,0.4929991,0.4891676,0.4852048,0.4811190,0.4769203,0.4726204,0.4682320,0.4637694,
                0.4592474,0.4546811,0.4500857,0.4454754,0.4408630,0.4362588,0.4316702,0.4271002,0.4225471,0.4180033,0.4134549,0.4088806,0.4042520,
                0.3995331,0.3946808,0.3896450,0.3843701,0.3787955,0.3728577,0.3664919,0.3596334,0.3522205,0.3441954,0.3321887,0.3150637,0.2981613,
                0.2815357,0.2652385,0.2493177,0.2338177,0.2187786,0.2042359,0.1902202,0.1767575,0.1638685,0.1515691,0.1398703,0.1287785)

  # Probability that LLINs repel a mosquito depending on bioassay survival
  ERG_r_ITN0<-c(0.3100000,0.3127572,0.3156311,0.3186170,0.3217085,0.3248972,0.3281725,0.3315216,0.3349293,0.3383777,0.3418467,0.3453133,0.3487522,
                0.3521360,0.3554347,0.3586169,0.3616497,0.3644990,0.3671305,0.3695098,0.3716032,0.3733783,0.3748042,0.3758524,0.3764968,0.3767139,
                0.3764831,0.3757862,0.3746075,0.3729330,0.3707503,0.3680476,0.3648136,0.3610368,0.3567054,0.3518073,0.3514055,0.3580077,0.3638294,
                0.3688334,0.3729921,0.3762882,0.3787148,0.3802753,0.3809831,0.3808614,0.3799419,0.3782643,0.3758750,0.3728260,0.3691734)

  # Function to plot the value of Prob(bite outside | bite successful)
  # Prob(bite outside | bite successful) = Prob(bite successful | bite outside)*Prob(bite outside) / Prob(bite successful)
  # in terms of the model parameters this is (1-Prob(repelled emanator))*phie / (1-Prob(repelled emanator))*phie + (1-ERG_r_ITN0-ERG_d_ITN0)*phib

  pab <- function(phie,phii,rem,rllin,dllin,ce,cl){
    top <- (1-(ce*(rem)))*phie
    bot <- (1-(cl*(rllin+dllin)))*phii
    return(top/(top+bot))
  }

  # Data frame of outputs
  mdf <- data.frame(
    "noem"=c(
      pab(phie=0.1,phii=0.9,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0,cl=0.8),
      pab(phie=0.2,phii=0.8,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0,cl=0.8),
      pab(phie=0.3,phii=0.7,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0,cl=0.8),
      pab(phie=0.4,phii=0.6,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0,cl=0.8),
      pab(phie=0.5,phii=0.5,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0,cl=0.8)),
    "em"=c(
      pab(phie=0.1,phii=0.9,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0.8,cl=0.8),
      pab(phie=0.2,phii=0.8,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0.8,cl=0.8),
      pab(phie=0.3,phii=0.7,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0.8,cl=0.8),
      pab(phie=0.4,phii=0.6,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0.8,cl=0.8),
      pab(phie=0.5,phii=0.5,rem=0.6053263,rllin=0.31,dllin=0.51,ce=0.8,cl=0.8)),
    "phie"=rep(seq(0.1,0.5,0.1),2)
  )

  hi <- wesanderson::wes_palette(n=4,name="BottleRocket2")

  p <- mdf %>% ggplot() +
    geom_ribbon(aes(x=phie,ymin=0,ymax=phie),fill="darkgreen") + # P(outside | success) with emanator + LLIN
    geom_ribbon(aes(x=phie,ymin=0,ymax=phie*r_EM0),fill=hi[[2]]) + # P(outside | success) without emanator + LLIN
    geom_ribbon(aes(x=phie,ymax=1,ymin=phie),fill=hi[[3]]) + # P(inside | success) with LLIN
    geom_ribbon(aes(x=phie,ymax=phie,ymin=phie+(1-phie)*(1-ERG_d_ITN0[1]-ERG_r_ITN0[1])),fill=hi[[4]]) +
    geom_line(aes(x=phie,y=phie+(1-phie)*(1-ERG_d_ITN0[1]-ERG_r_ITN0[1])),col="white",lty=2) +
    geom_line(aes(x=phie,y=phie*r_EM0),col="white",lty=2) +
    geom_hline(yintercept=seq(0,1,0.2),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.4,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0.1,0.5,0.1),labels=paste(seq(10,50,10),"%",sep="")) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
    xlab("Proportion of feeding attempts that happen outdoors") +
    ylab("Proportion of feeding attempts") +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=paste(seq(0,100,20),"%",sep=""))
  return(p)
}

