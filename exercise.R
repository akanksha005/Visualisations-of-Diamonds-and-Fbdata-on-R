?diamond
library(ggplot2)
data(diamonds)
summary(diamonds)
?quantile
library('dplyr')

ggplot(aes(x=price,y=carat),data = diamonds)+geom_point(alpha=1/20)+xlim(0,quantile(diamonds$price,(0.99)))+
  ylim(0,quantile(diamonds$carat,(0.99)))

diamonds$volume <- diamonds$x*diamonds$y*diamonds$z

ggplot(aes(x=price,y=volume),data = diamonds)+geom_point(alpha=1/20)+ylim(0,quantile(diamonds$volume,(0.99)))

install.packages("plyr")
library("plyr")

detach("package:plyr", unload=TRUE) 

count(diamonds$volume==0)

subsetd <-subset(diamonds,diamonds$volume>0 & diamonds$volume<800)

cor(subsetd$price,subsetd$volume)

ggplot(aes(x=price,y=volume),data = subsetd)+
geom_point(alpha=1/20)+
ylim(0,quantile(diamonds$volume,(0.99)))+geom_smooth(method = 'lm')


diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise( 
              mean_price=mean(price), 
             median_price=median(price),
             min_price=min(price), 
             max_price=max(price),n=n()) %>%
  arrange(clarity)
head(diamondsByClarity)

library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
p1 <- ggplot(aes(x=clarity,y=mean_price),data=diamonds_mp_by_clarity)+geom()

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
p2 <- ggplot(aes(x=color,y=mean_price),data=diamonds_mp_by_color)+geom_hist()

library(gridExtra)
grid.arrange(p1,p2,ncol=1)
