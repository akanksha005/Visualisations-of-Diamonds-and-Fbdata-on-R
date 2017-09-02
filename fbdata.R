getwd()
setwd("C:/Users/AKANKSHA/Desktop/")
list.files(
)
fbdata <- read.csv('pseudo_facebook.tsv',sep='\t')
table(fbdata$gender)
by(fbdata$friend_count,fbdata$gender,summary)
library(ggplot2)
qplot(data=fbdata,x=gender,y=friend_count,geom='boxplot')+coord_cartesian(ylim=c(0,250))
