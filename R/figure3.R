#' Plot Figure 3
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon scale_x_continuous coord_cartesian scale_y_continuous theme geom_vline xlab ylab scale_fill_discrete geom_histogram theme_minimal
#' @importFrom hrbrthemes theme_ipsum_ps
#' @importFrom dplyr group_by %>% summarise filter mutate full_join
#' @importFrom cowplot plot_grid
#' @importFrom tidyr gather
#' @importFrom RColorBrewer brewer.pal
#' @return plot
#' @author Joel Hellewell
#' @export
figure3 <- function(){

 # load behaviour data


 # colour palette
 cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00",
                 "#CC79A7", "#F0E442")

 # Panel a

 dm <- data.frame(stack(dat_indoor[,3:13]),hour=rep(dat_indoor$Hour,11)) %>% group_by(hour) %>%  summarise(m=mean(values),uq=quantile(values,probs=0.975),lq=quantile(values,probs=0.025)) %>%
  filter(hour<12 | hour >15) %>% mutate(seq=c(10:20,1:9))
 pa <- ggplot() +
  geom_line(data=dm,aes(x=seq,y=m)) +
  geom_ribbon(data=dm,aes(x=seq,ymax=uq,ymin=lq),fill="darkorchid4",alpha=0.3,size=1) +
  scale_x_continuous(labels=c(16:24,1:15),breaks=1:24) +
  coord_cartesian(xlim=c(1,20)) + theme_ipsum_ps(axis_text_size = 17,axis_title_size = 18,axis_title_just = "centre") +
  scale_y_continuous(breaks=seq(0,1,0.2),labels=paste(seq(0,100,20),"%",sep="")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text = element_text(size=14),axis.title = element_text(size=14),
        panel.grid.minor = element_blank()) +
  geom_vline(xintercept = c(2,18),lty=2,size=1) +
  xlab("Time of day (hours)") + ylab("Proportion of population")

 # Panel b

 cols <- RColorBrewer::brewer.pal(n=5,name="Set1")

 mdf1 <- dat_mosq %>% group_by(Hour) %>% filter(Hour>=16 & Hour<=23) %>%
  summarise(h=quantile(Outside_mosq,probs=0.975),
            l=quantile(Outside_mosq,probs=0.025),
            m=median(Outside_mosq),ty="Outside")
 mdf1$od <- 1:8

 mdf2 <- dat_mosq %>% group_by(Hour) %>% filter(Hour %in% c(23,24,1,2,3,4,5)) %>%
  summarise(h=quantile(Inside_mosq,probs=0.975),
            l=quantile(Inside_mosq,probs=0.025),
            m=median(Inside_mosq),ty="Inside")
 mdf2$od <- c(10:14,8:9)

 mdf3 <- dat_mosq %>% group_by(Hour) %>% filter(Hour>=5 & Hour<=11) %>%
  summarise(h=quantile(Outside_mosq,probs=0.975),
            l=quantile(Outside_mosq,probs=0.025),
            m=median(Outside_mosq),ty="Outside")
 mdf3$od <- c(14:20)

 pb <- ggplot() +
  geom_ribbon(aes(x=2:8,ymin=0,ymax=0.17),fill="darkorchid4",alpha=0.1) +
  geom_ribbon(aes(x=8:14,ymin=0,ymax=0.17),fill="darkorchid4",alpha=0.3)+
  geom_ribbon(aes(x=14:18,ymin=0,ymax=0.17),fill="darkorchid4",alpha=0.1) +
  geom_line(data=mdf1,aes(x=od,y=m),col=cols[2],size=1.1) +
  geom_line(data=mdf2,aes(x=od,y=m),col=cols[1],size=1.1) +
  geom_line(data=mdf3,aes(x=od,y=m),col=cols[2],size=1.1) +
  geom_ribbon(data=mdf1,aes(x=od,ymax=h,ymin=l),fill=cols[2],alpha=0.4,lty=2,size=1) +
  geom_ribbon(data=mdf2,aes(x=od,ymax=h,ymin=l),fill=cols[1],alpha=0.4,lty=2,size=1) +
  geom_ribbon(data=mdf3,aes(x=od,ymax=h,ymin=l),fill=cols[2],alpha=0.4,lty=2,size=1) +
  geom_vline(xintercept = c(2,8,14,18),lty=2,size=1) +
  scale_x_continuous(labels=c(16:24,1:15),breaks=1:24) + theme_ipsum_ps(axis_text_size = 15,axis_title_size = 18,axis_title_just = "centre") +
  scale_fill_discrete(guide=FALSE) + scale_color_discrete(guide=FALSE) +
  xlab("Time of day (hours)") + ylab("Rate of mosquito biting") +
  theme(plot.title = element_text(hjust = 0.5),axis.text = element_text(size=18),axis.title = element_text(size=20))


 # Panel c

 # Human activity by hour
 hum <- dat_indoor2 %>%
  tidyr::gather("h.study","Inside_hum",-Hour)

 # Mosquito indoor activity by hour
 x2 <- dat_mosq2 %>%
  tidyr::gather("study","Inside_mosq",-Indoor.mosquitoes) %>%
  mutate(Hour=Indoor.mosquitoes)



 # Mosquito outdoor activity by hour
 x3 <- dat_mosq3 %>%
  tidyr::gather("study","Outside_mosq",-Hour)

 mosq <- full_join(x2,x3,by=c("Hour","study"))
 x <- full_join(hum,mosq,by="Hour")

 pc <- x %>% group_by(h.study,study) %>%
  summarise(phi=sum(Inside_hum*Inside_mosq)/sum((1-Inside_hum)*Outside_mosq + Inside_hum*Inside_mosq)) %>%
  ggplot(aes(1-phi)) + geom_histogram(fill="green4") + theme_minimal() + geom_vline(xintercept = 1-c(0.13,0.87,1),lty=2) +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=paste0(seq(0,100,10),"%")) +
  xlab("Proportion of attempted bites that happen outdoors") + ylab("Frequency") +
  theme(axis.text = element_text(size=14),axis.title = element_text(size=15))

 cowplot::plot_grid(pa,pb,pc,ncol=3)
}
