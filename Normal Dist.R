moment <- function(x, y){
  mean((x-mean(x))^y)
}

moment(x = anorexia$Prewt, y = 2)
ggplot(Cars93, aes(x = Horsepower))+
  geom_histogram(aes(y = after_stat(density)))+
  geom_density()+
  facet_wrap(~Origin)

x.values <- seq(40, 160, 1)
sd.values <- seq(40, 160, 15)
zeroes9 <- rep(0, 9)
library(ggplot2)

## Normal Dist Curve
ggplot(data = NULL, aes(x = x.values, y = dnorm(x.values, mean = 100, sd = 15)))+
  geom_line()+
  scale_x_continuous(breaks = sd.values, labels = sd.values)+
  labs(x = "IQ", y = "f(IQ)")+
  geom_segment(aes(x = sd.values, y = zeroes9 ,
                   xend = sd.values, yend = dnorm(sd.values, mean = 100, sd = 15)
  ), linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))

pnormGC(c(65, 130), region = "between", mean = 100, sd = 15, graph = T)

## Normal Cumulative Curve
ggplot(data = NULL, aes(x = x.values, y = pnorm(x.values, mean = 100, sd = 15)))+
  geom_line()+
  scale_x_continuous(breaks = sd.values, labels = sd.values)+
  labs(x = "IQ", y = "f(IQ)")+
  geom_segment(aes(x = sd.values, y = zeroes9, xend = sd.values, yend = pnorm(sd.values, mean = 100, sd = 15)
  ), linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))

qnorm(.45, mean = 100, sd = 15, lower.tail = TRUE)

qnormGC(.45, region = "below", mean = 100, sd = 15, graph = T)

skim(Cars93)