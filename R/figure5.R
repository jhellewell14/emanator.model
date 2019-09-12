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

 # Plot output
 yo <- wesanderson::wes_palette(name="IsleofDogs1",n=2)

 p <- data.frame(x=seq(0,0.4,0.05),outdoor_exp_low,outdoor_exp_high) %>% ggplot() +
  geom_line(aes(x,outdoor_exp_low),size=1.2,col=yo[[1]]) +
  geom_line(aes(x,outdoor_exp_high),size=1.2,col=yo[[2]]) +
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
