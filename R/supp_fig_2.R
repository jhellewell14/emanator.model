suppfig2 <- function(){

 rITN <- 0.56 # probability of ITN repel
 dITN <- 0.41 # probability of ITN kill
 cITN <- 0.8 # ITN coverage
 phiI <- 0.9 # pre-intervention % of bites indoors
 Q0 <- seq(0,1,0.05) # probability that mosquito bites again (% of bites taken on humans)

 # bite again
 # prob(repelled) * prob(covered by ITN)
 bite_again <- (rITN * cITN) * Q0

 # bites indoors
 # prob(not repelled or killed) * prob(covered by ITN) + prob(not covered by ITN) + prob(bite again inside)
 bite_inside <- ((1-rITN-dITN)*cITN)*phiI + (1-cITN)*phiI + bite_again*phiI

 # bites outdoors
 # prob bite outside + prob(bite again outside)
 bite_outside <- 1-phiI + bite_again*(1-phiI)

 df <- data.frame(Q0, bite_inside,bite_outside,bite_again)
 library(dplyr)
 library(ggplot2)
 df %>% ggplot(aes(x=Q0)) +
  geom_line(aes(y=1-bite_inside)) +
  geom_line(aes(y=1-bite_outside),col="red") +
  geom_line(aes(y=bite_again),col="green") +
  geom_line(aes(y=bite_outside/(bite_inside+bite_outside)),col="blue") +
  xlab("Probability of biting a human") +
  scale_x_continuous(labels=paste0(seq(0,100,25),"%")) +
  scale_y_continuous(breaks=seq(0,1,0.1),labels=paste0(seq(0,100,10),"%")) +
  ylab("")

}
