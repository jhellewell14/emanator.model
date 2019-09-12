#' Plot Figure 2
#'
#' @importFrom ggplot2 ggplot geom_point geom_errorbar geom_line geom_hline geom_vline scale_x_continuous scale_y_continuous xlab ylab theme_bw theme
#' @importFrom cowplot plot_grid
#' @return plot
#' @author Joel Hellewell
#' @export
figure2 <- function(){

 # Summarised field trial data
 tan_stdy <- data.frame(dist=c(0,2,5,10,20),averted=c(83,33,40,19,2),lb=c(70,0.61,10,-23,-48),ub=c(91,56,60,47,35))

 # Simple regression
 x1 <- tan_stdy$dist
 em <- lm(log(tan_stdy$averted)~x1)
 counts2 <- exp(predict(em,list("x1"=seq(0,20,1))))
 lfit <- data.frame(dist=seq(0,20,1),fit=counts2)
 my_D <- function(x){exp(predict(em,list("x1"=x)))/100}

 # Distribution of population distance
 half_within_m <- 1
 my_C <- function(x){dexp(x,rate=log(2)/half_within_m)}

 my_fun <- function(x){
  my_D(x)*my_C(x)
 }

 # Panel a

 counts2 <- exp(predict(em,list("x1"=seq(0,20,1))))
 pa <- ggplot() +
  geom_point(data=tan_stdy,aes(x=dist,y=averted),size=2.5) +
  geom_errorbar(data=tan_stdy,aes(x=dist,ymin=lb,ymax=ub)) +
  geom_line(data=lfit,aes(x=dist,y=fit),col="red") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept=0,col="blue",lty=2) +
  scale_y_continuous(breaks=seq(-50,90,10),labels=paste(seq(-50,90,10),sep="")) +
  scale_x_continuous(breaks=c(0,2,5,10,20),labels=paste(c(0,2,5,10,20),"m",sep="")) +
  xlab("Distance from emanator") + ylab("Relative protection against bites (%)") +
  theme_bw() +
  theme(legend.position = "right",axis.text=element_text(size=15),axis.title = element_text(size=18),
        strip.text=element_text(size=14),axis.text.x=element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,face="bold"))


 # Panel b

 pb <- ggplot()+
  geom_line(aes(x=seq(0,20,0.1),y=dexp(x = seq(0,20,0.1),rate = log(2)/1)),size=1) +
  geom_line(aes(x=seq(0,20,0.1),y=dexp(x = seq(0,20,0.1),rate = log(2)/2)),col="orange",size=1) +
  geom_line(aes(x=seq(0,20,0.1),y=dexp(x = seq(0,20,0.1),rate = log(2)/5)),col="blue",size=1) +
  geom_line(aes(x=seq(0,20,0.1),y=dexp(x = seq(0,20,0.1),rate = log(2)/10)),col="green3",size=1) +
  ylab("Proportion of time spent at this distance (%)") +
  xlab("Distance from emanator") + theme_bw() +
  scale_y_continuous(breaks=seq(0,0.7,0.1),labels=paste(seq(0,70,10),sep="")) +
  scale_x_continuous(breaks=c(0,2,5,10,20),labels=paste(c(0,2,5,10,20),"m",sep="")) +
  # scale_x_continuous(breaks=seq(0,20,2),labels=paste("<",seq(0,20,2),"m",sep="")) +
  theme(legend.position = "right",axis.text=element_text(size=15),axis.title = element_text(size=18),
        strip.text=element_text(size=14),axis.text.x=element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,face="bold"))

 # Panel c

 y <- c()
 for(i in 1:20){
  half_within_m <- i
  #p_dist <- p_dist - (sum(p_dist)-1)/length(p_dist) # check that this doesn't go negative
  y[i] <- integrate(f=my_fun,upper=Inf,lower=0)$value[[1]]
 }

 pc <- ggplot() +
  geom_line(aes(x=1:20,y=unlist(y)),size=1) + theme_bw() +
  ylab("Proportion of bites averted over all distances (%)") + xlab("Population median distance from emanator") +
  scale_y_continuous(breaks=seq(0,0.9,0.1),labels=paste(seq(0,90,10),sep="")) +
  scale_x_continuous(breaks=c(0,1,2,5,10,20),labels=paste(c(0,1,2,5,10,20),"m",sep="")) +
  theme(legend.position = "right",axis.text=element_text(size=15),axis.title = element_text(size=18),
        strip.text=element_text(size=14),axis.text.x=element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))

 return(cowplot::plot_grid(pa,pb,pc,ncol = 3))
}
