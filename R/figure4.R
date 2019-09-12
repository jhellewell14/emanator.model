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

    itnonly <- emanator.model::mr(itn_cov=0.5,em_cov=0,
                                  bites_Emanator=bites_Emanator,
                                  bites_Indoors=bites_Indoors,
                                  bites_Bed=bites_Bed,
                                  init_EIR=init_EIR)
    itnonly2 <- emanator.model::mr(itn_cov=0.8,em_cov=0,
                                   bites_Emanator=bites_Emanator,
                                   bites_Indoors=bites_Indoors,
                                   bites_Bed=bites_Bed,
                                   init_EIR=init_EIR)

    # print(itnonly$bites_Emanator[1])

    r_EM0 <- 1
    em_loss <- 0
    no_outdoor <- emanator.model::mr(itn_cov=0.5,em_cov=1,
                                     bites_Emanator=bites_Emanator,
                                     bites_Indoors=bites_Indoors,
                                     bites_Bed=bites_Bed,
                                     init_EIR=init_EIR,
                                     r_EM0 = 1,em_loss=0)
    no_outdoor2 <- emanator.model::mr(itn_cov=0.8,em_cov=1,
                                      bites_Emanator=bites_Emanator,
                                      bites_Indoors=bites_Indoors,
                                      bites_Bed=bites_Bed,
                                      init_EIR=init_EIR,
                                      r_EM0 = 1,em_loss=0)

    outdoor_exp_low[i] <- sum(itnonly$inc05[(365*5):(365*6)]-no_outdoor$inc05[(365*5):(365*6)])/sum(itnonly$inc05[(365*5):(365*6)])
    outdoor_exp_high[i] <- sum(itnonly2$inc05[(365*5):(365*6)]-no_outdoor2$inc05[(365*5):(365*6)])/sum(itnonly2$inc05[(365*5):(365*6)])
    print(conservative)
    k <- k + 1
  }

  # Plot output
  yo <- wesanderson::wes_palette(name="IsleofDogs1",n=2)

  p <- data.frame(x=seq(0,0.4,0.05),conservative) %>% ggplot() +
    geom_line(aes(x,conservative),size=1.2,col=yo[[1]]) +
    geom_line(aes(x,conservative2),size=1.2,col=yo[[2]]) +
    theme_bw() +
    scale_x_continuous(breaks=seq(0,1,0.1),labels=paste(seq(0,100,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,1,0.1),labels=paste(seq(0,100,10),"%",sep="")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size=18),
          axis.title = element_text(size=20)) +
    theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18,axis_title_just = "centre") +
    xlab(latex2exp::TeX("Proportion of feeding attempts that happen outdoors ($\\phi_E$)")) +
    ylab("Percentage of cases due to bites happening outdoors") +
    geom_abline(slope = 1,intercept = 0,lty=2)

  return(p)
}

figure4b <- function(){

  bites_Indoors <- 0.8
  bites_Emanator <- 1 - bites_Indoors
  bites_Bed <- 0.8813754*bites_Indoors # this is bed to indoors ratio
  init_EIR <- 4.3

  nr1 <- mr(em_cov=0,itn_cov=0.8,bites_Emanator=bites_Emanator,
            bites_Indoors=bites_Indoors,
            bites_Bed=bites_Bed,
            init_EIR=init_EIR) # ITN only

  both <- mr(em_cov=0.8,itn_cov=0.8,bites_Emanator=bites_Emanator,
             bites_Indoors=bites_Indoors,
             bites_Bed=bites_Bed,
             init_EIR=init_EIR) # ITN + EM

  p <- data.frame(x=1:(365*16),itn=nr1$prev,em=both$prev) %>%
    ggplot(aes(x=x)) + theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18,axis_title_just = "centre") +
    geom_line(aes(y=itn)) +
    geom_line(aes(y=em),col="red") +
    geom_hline(yintercept=c(0.000149,0.08224412),lty=2) +
    coord_cartesian(xlim=c((365*4):(361*14))) +
    scale_y_continuous(breaks=seq(0,0.08,0.02),labels=paste(seq(0,8,2),"%",sep="")) +
    scale_x_continuous(breaks=seq((365*5),(365*14),(365*3)),labels=paste(seq(0,9,3),sep="")) +
    geom_vline(xintercept=seq((365*5),(365*14),(365*3)),lty=2,alpha=0.5,col="blue") +
    xlab("Years since initial intervention") +
    ylab("Slide prevalence (0-5 year-olds)")

  return(p)
}

