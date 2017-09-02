getwd()
setwd("C:/Users/AKANKSHA/Desktop/")
list.files(
)
fbdata <- read.csv('pseudo_facebook.tsv',sep='\t')
str(fbdata)
names(fbdata)
library(ggplot2)
qplot(x=friend_count,data=subset(fbdata,!is.na(gender)),xlim = c(0,1000))

qplot(x=friend_count,data=subset(fbdata,!is.na(gender)), binwidth=25)+scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender)

qplot(x=friend_count,data=subset(fbdata,na.omit(gender)), binwidth=25)+scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender)

table (fbdata$gender)
by (fbdata$friend_count,fbdata$gender,summary)

qplot(x=tenure/365,data=fbdata,binwidth=1,
      color=I('black'),fill=I('green'))

qplot(x=age,data=fbdata,binwidth=5,
      color=I('black'),fill=I('green'))+scale_x_continuous(limits = c(0,100),breaks = seq(0,100,5))

summary(fbdata$age)

data(diamonds)
summary(diamonds)
?diamonds

qplot(x=price,data=diamonds)

subset(diamonds,price<250)

qplot(x = price, data = diamonds)

ggsave('priceHistogram.png')

qplot(x = log10(price), data = diamonds,binwidth=.1)+facet_wrap(~cut ,scales="free_y")

table(diamonds$color)
by(diamonds$price,diamonds$color,summary)

qplot(x = color, y = price/carat, data = diamonds,  geom='boxplot',color=color)

qplot(x = color, y = price/carat, data = diamonds,  geom='frequency_polygon',color=I('green'))

qplot(x=www_likes,y=..count../sum(..count..),data=subset(fbdata,!is.na(gender)), binwidth=200,color=gender,geom='freqpoly')+scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender)

ggplot(aes(x = www_likes), data = subset(fbdata, !is.na(gender))) +
  geom_freqpoly(aes(color = gender)) +
  scale_x_log10()


qplot(x=carat,data=diamonds,geom='freqpoly',binwidth=.01)

table(diamonds$carat)

qplot(age,friend_count,data=fbdata)

?coord_trans
ggplot(aes(x=age,y=friendships_initiated),data=fbdata)+geom_point(alpha=1/20,position = position_jitter(h=0))+xlim(13,90)+
coord_trans(y="sqrt")

??fbdata
install.packages('dplyr')
library(dplyr)

age_groups <- group_by(fbdata,age)
fbdata.fc_by_age <-summarise(age_groups,fb_mean=mean(friend_count),fb_median=median(friend_count),n=n())
fbdata.fc_by_age <-arrange(fbdata.fc_by_age,age)
head(fbdata.fc_by_age)

fbdata$age_with_months <- (fbdata$age+((12-fbdata$dob_month)/12))
head(fbdata.age_with_months)
fbdata.fc_by_age <-fbdata %>%
  group_by(age) %>%
  summarise(fb_mean=mean(friend_count),fb_median=median(friend_count),n=n()) %>%
  arrange(age)

qplot(x=age,y=fb_mean,data=fbdata.fc_by_age)

q10 <- function(x) {
  r <- quantile(x, probs = c(0.1))
}

q90 <- function(x) {
  r <- quantile(x, probs = c(0.9))
}

p1<-ggplot(aes(x=age,y=fb_mean),data=subset(fbdata.fc_by_age,age<71))+geom_line()

ggplot(aes(x=age,y=friend_count),data=fbdata)+
  geom_point(alpha=1/20,position = position_jitter(h=0),color="orange")+ 
  xlim(13,90)+
  coord_trans(y = 'sqrt') + 
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_line(stat = 'summary', fun.y = q90,linetype = 2, color = 'blue')+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = c(probs=.1),
            linetype = 2, color = 'blue')

cor(fbdata$age,fbdata$friend_count)

subset(fbdata,fbdata$age<70)

with(subset(fbdata,fbdata$age<70),cor(age,friend_count))

?xlim
ggplot(aes(x=likes_received,y=www_likes_received),data=fbdata)+geom_point(alpha=1,color='orange')+
  xlim(0,quantile(fbdata$likes_received,(.95)))+
  ylim(0,quantile(fbdata$www_likes_received,(.95)))+
  geom_smooth(method ='lm')

cor(fbdata$likes_received,fbdata$www_likes_received)

fbdata.age_with_months <- fbdata$age + (12 - fbdata$dob_month)/12

names(fbdata)
pf.fc_by_age_months <- fbdata %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean=mean(friend_count),friend_count_median=median(friend_count),n=n())%>%
  arrange(age_with_months)
p2<-ggplot(aes(x=age_with_months,y=friend_count_mean),data=subset(pf.fc_by_age_months,age_with_months<71))+geom_line()

install.packages('gridExtra')
library(gridExtra)

grid.arrange(p2,p1,ncol=1)
