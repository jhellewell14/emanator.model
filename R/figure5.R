#' Plot Figure 5a
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of bioassay survival in the local gambiae-like vector population.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure5a <- function(){

 ITN_elim <- c()
 ITN_EM_elim <- c()
 itn_cov <- 0.8
 bites_Emanator <- 0.2
 bites_Indoors <- 1 - bites_Emanator
 bites_Bed <- 0.8813754*bites_Indoors

 for(i in 0:5){#0% -> 50% bioassay survival
  print(paste("Bioassay survival:",i))
  surv_bioassay <- i*0.1
  em_cov <- 0

  # ITN elimination
  print("ITN")
  ITN_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                   surv_bioassay=surv_bioassay,
                                   bites_Emanator,bites_Indoors,bites_Bed,
                                   em_cov,itn_cov,Q0=0.92,d_EM0=0)

  # ITM + EM
  em_cov <- 0.8
  print("ITN + EM")
  ITN_EM_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                      surv_bioassay=surv_bioassay,
                                      bites_Emanator,bites_Indoors,bites_Bed,
                                      em_cov,itn_cov,Q0=0.92,d_EM0=0)
 }

 hi <- wes_palette(n=3,name="FantasticFox1")

 p <- data.frame(x=seq(0,0.5,0.1),low=ITN_elim,high=ITN_EM_elim) %>%
  ggplot() + coord_cartesian(ylim=c(0,3.3)) +
  geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
  geom_ribbon(aes(x=x,ymax=3.3,ymin=high),fill=hi[1]) +
  geom_hline(yintercept=seq(0,3,0.5),lty=2,alpha=0.5) +
  geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
  scale_x_continuous(breaks=seq(0,0.5,0.1),labels=paste(seq(0,50,10),"%",sep="")) +
  scale_y_continuous(breaks=seq(0,3,0.5)) +
  theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
  xlab("Resistance test (% survival)") +
  ylab("EIR (bites per year)")

 return(p)
}

#' Plot Figure 5b
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of bioassay survival in the local gambiae-like vector population. This plot includes a 20% mortality effect.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure5b <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  itn_cov <- 0.8
  bites_Emanator <- 0.2
  bites_Indoors <- 1 - bites_Emanator
  bites_Bed <- 0.8813754*bites_Indoors

  for(i in 0:5){#0% -> 50% bioassay survival
    print(paste("Bioassay survival:",i))
    surv_bioassay <- i*0.1
    em_cov <- 0

    # ITN elimination
    print("ITN")
    ITN_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                     surv_bioassay=surv_bioassay,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.92,d_EM0=0.2)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                        surv_bioassay=surv_bioassay,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.92,d_EM0=0.2)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0,0.5,0.1),low=ITN_elim,high=ITN_EM_elim) %>%
    ggplot() + coord_cartesian(ylim=c(0,3.3)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=3.3,ymin=high),fill=hi[1]) +
    geom_hline(yintercept=seq(0,3,0.5),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0,0.5,0.1),labels=paste(seq(0,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,3,0.5)) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Resistance test (% survival)") +
    ylab("EIR (bites per year)")

  return(p)
}

#' Plot Figure 5c
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of bioassay survival in the local gambiae-like vector population. This plot is for optimal emanators with 100% repellency and no decay in effect over time.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure5c <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  itn_cov <- 0.8
  bites_Emanator <- 0.2
  bites_Indoors <- 1 - bites_Emanator
  bites_Bed <- 0.8813754*bites_Indoors

  for(i in 0:5){#0% -> 50% bioassay survival
    print(paste("Bioassay survival:",i))
    surv_bioassay <- i*0.1
    em_cov <- 0

    # ITN elimination
    print("ITN")
    ITN_elim[i+1] <- find_all_boundary(r_EM0=1,em_loss=0,
                                     surv_bioassay=surv_bioassay,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.92,d_EM0=0)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i+1] <- find_all_boundary(r_EM0=1,em_loss=0,
                                        surv_bioassay=surv_bioassay,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.92,d_EM0=0)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0,0.5,0.1),low=ITN_elim,high=ITN_EM_elim) %>%
    ggplot() + coord_cartesian(ylim=c(0,3.3)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=3.3,ymin=high),fill=hi[1]) +
    geom_hline(yintercept=seq(0,3,0.5),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0,0.5,0.1),labels=paste(seq(0,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,3,0.5)) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Resistance test (% survival)") +
    ylab("EIR (bites per year)")

  return(p)
}

#' Plot Figure 5d
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of bioassay survival in the local arabiensis-like vector population.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure5d <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  itn_cov <- 0.8
  bites_Emanator <- 0.2
  bites_Indoors <- 1 - bites_Emanator
  bites_Bed <- 0.8813754*bites_Indoors

  for(i in 0:5){#0% -> 50% bioassay survival
    print(paste("Bioassay survival:",i*0.1))
    surv_bioassay <- i*0.1
    em_cov <- 0

    # ITN elimination
    print("ITN")
    ITN_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                     surv_bioassay=surv_bioassay,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.16,d_EM0=0)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                        surv_bioassay=surv_bioassay,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.16,d_EM0=0)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0,0.5,0.1),low=ITN_elim,high=ITN_EM_elim) %>%
    ggplot() + coord_cartesian(ylim=c(0,3)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=3.3,ymin=high),fill=hi[1]) +
    geom_hline(yintercept=seq(0,3,0.5),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0,0.5,0.1),labels=paste(seq(0,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,3,0.5)) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Resistance test (% survival)") +
    ylab("EIR (bites per year)")

  return(p)
}

#' Plot Figure 5e
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of bioassay survival in the local arabiensis-like vector population. This plot includes a 20% mortality effect.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure5e <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  itn_cov <- 0.8
  bites_Emanator <- 0.2
  bites_Indoors <- 1 - bites_Emanator
  bites_Bed <- 0.8813754*bites_Indoors

  for(i in 0:5){#0% -> 50% bioassay survival
    print(paste("Bioassay survival:",i))
    surv_bioassay <- i*0.1
    em_cov <- 0

    # ITN elimination
    print("ITN")
    ITN_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                     surv_bioassay=surv_bioassay,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.16,d_EM0=0.2)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i+1] <- find_all_boundary(r_EM0=0.6053263,em_loss=0.001954954,
                                        surv_bioassay=surv_bioassay,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.16,d_EM0=0.2)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0,0.5,0.1),low=ITN_elim,high=ITN_EM_elim) %>%
    ggplot() + coord_cartesian(ylim=c(0,3.3)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=3.3,ymin=high),fill=hi[1]) +
    geom_hline(yintercept=seq(0,3,0.5),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0,0.5,0.1),labels=paste(seq(0,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,3,0.5)) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Resistance test (% survival)") +
    ylab("EIR (bites per year)")

  return(p)
}

#' Plot Figure 5f
#'
#' This performs a unknown amount of model runs. It systematically searches for the maximum pre-intervention EIR that ITN and ITN+emanator
#' combinations will eliminate for varying levels of bioassay survival in the local arabiensis-like vector population. This plot is for optimal emanators with 100% repellency and no decay in effect over time.
#' @importFrom wesanderson wes_palette
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom ggplot2 ggplot coord_cartesian geom_ribbon geom_hline geom_vline scale_x_continuous theme xlab ylab
#' @return plot
#' @author Joel Hellewell
#' @export
figure5f <- function(){

  ITN_elim <- c()
  ITN_EM_elim <- c()
  itn_cov <- 0.8
  bites_Emanator <- 0.2
  bites_Indoors <- 1 - bites_Emanator
  bites_Bed <- 0.8813754*bites_Indoors

  for(i in 0:5){#0% -> 50% bioassay survival
    print(paste("Bioassay survival:",i))
    surv_bioassay <- i*0.1
    em_cov <- 0

    # ITN elimination
    print("ITN")
    ITN_elim[i+1] <- find_all_boundary(r_EM0=1,em_loss=0,
                                     surv_bioassay=surv_bioassay,
                                     bites_Emanator,bites_Indoors,bites_Bed,
                                     em_cov,itn_cov,Q0=0.16,d_EM0=0)

    # ITM + EM
    em_cov <- 0.8
    print("ITN + EM")
    ITN_EM_elim[i+1] <- find_all_boundary(r_EM0=1,em_loss=0,
                                        surv_bioassay=surv_bioassay,
                                        bites_Emanator,bites_Indoors,bites_Bed,
                                        em_cov,itn_cov,Q0=0.16,d_EM0=0)
  }

  hi <- wes_palette(n=3,name="FantasticFox1")

  p <- data.frame(x=seq(0,0.5,0.1),low=ITN_elim,high=ITN_EM_elim) %>%
    ggplot() + coord_cartesian(ylim=c(0,3.3)) +
    geom_ribbon(aes(x=x,ymin=0,ymax=low),fill=hi[3]) +
    geom_ribbon(aes(x=x,ymin=low,ymax=high),fill=hi[2]) +
    geom_ribbon(aes(x=x,ymax=3.3,ymin=high),fill=hi[1]) +
    geom_hline(yintercept=seq(0,3,0.5),lty=2,alpha=0.5) +
    geom_vline(xintercept=seq(0.1,0.5,0.1),lty=2,alpha=0.5) +
    scale_x_continuous(breaks=seq(0,0.5,0.1),labels=paste(seq(0,50,10),"%",sep="")) +
    scale_y_continuous(breaks=seq(0,3,0.5)) +
    theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=18)) +
    xlab("Resistance test (% survival)") +
    ylab("EIR (bites per year)")

  return(p)
}
