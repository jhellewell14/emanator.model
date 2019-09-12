#' Plot Figure 5a
#'
#' This performs 9 model runs in total
#' @importFrom ggplot2 ggplot geom_line xlab ylab scale_x_continuous theme_bw scale_x_continuous scale_y_continuous geom_abline theme
#' @importFrom wesanderson wes_palette
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @return plot
#' @author Joel Hellewell
#' @export
figure5a <- function(){

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
  geom_line(aes(x,outdoor_exp_low),size=1.2,col=yo[[1]]) +
  geom_line(aes(x,outdoor_exp_high),size=1.2,col=yo[[2]]) +
  geom_line(data=gamb_df,aes(x=x,y=gamb_out_low),size=1.2,alpha=0.4,col=yo[[1]],lty=2) +
  geom_line(data=gamb_df,aes(x=x,y=gamb_out_high),size=1.2,alpha=0.4,col=yo[[2]],lty=2) +
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

figure5b <- function(){

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
 ca <- data.frame(z=as.vector(dm),y=rep(1:30,10),x=rep(seq(0.1,0.5,0.1),rep(30,5)))
 p <- ca %>% ggplot() + geom_tile(aes(x,y,fill=z)) + ylab("Infectious bites per year") + xlab("Percentage of bites happening outdoors") +
  scale_x_continuous(breaks=seq(0.1,0.5,0.1),labels=paste(seq(10,50,10),"%",sep="")) +
  scale_fill_viridis(name="Cases per 1000 \n0-5 year olds \naverted in first year",
                     breaks=seq(0,180,30)) + theme_minimal() +
  theme_ipsum_ps(axis_text_size = 16,axis_title_size = 18)
 return(p)
}
