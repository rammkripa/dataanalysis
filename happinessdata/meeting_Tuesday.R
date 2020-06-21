data = read.csv(file.choose())
data
library(ggplot2)
ggplot(data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Ladder.score ~ Logged.GDP.per.capita,data)
summary(x)
# richer people happier
ggplot(data,aes(x=Healthy.life.expectancy,y=Ladder.score))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Ladder.score ~ Healthy.life.expectancy,data)
summary(x)
# higher life expectancy happier
ggplot(data,aes(x=Logged.GDP.per.capita,y=Healthy.life.expectancy))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Healthy.life.expectancy ~ Logged.GDP.per.capita,data)
summary(x)
# more gdp per capita, higher life expectancy
ggplot(data,aes(x=Social.support,y=Healthy.life.expectancy))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Healthy.life.expectancy ~ Social.support,data)
summary(x)
# More social support, more life expectancy
ggplot(data,aes(x=Social.support,y=Logged.GDP.per.capita))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Logged.GDP.per.capita ~ Social.support,data)
summary(x)
# more gdp more social support
ggplot(data,aes(x=Social.support,y=..density..))+
  geom_histogram(bins = 20,color = "blue")
# Social support doesn't follow normal curve
ggplot(data,aes(x=Logged.GDP.per.capita,y=..density..))+
  geom_histogram(bins = 20,color = "blue")
# GDP roughly follows normal curve
ggplot(data,aes(x=Ladder.score,y=..density..))+
  geom_histogram(bins = 10,color = "blue")
# Ladder Score roughly follows normal curve
plot_corr = function(param1, param2){
ggplot(data,aes(x=param1,y=param2))+
  geom_point()+
  geom_smooth(method="lm")
}
plot_corr(data$Perceptions.of.corruption,data$Ladder.score)
cor(data$Perceptions.of.corruption,data$Ladder.score)
# ladder score and corruption -ve corr
plot_corr(data$Perceptions.of.corruption,data$Freedom.to.make.life.choices)
cor(data$Perceptions.of.corruption,data$Freedom.to.make.life.choices)
#
plot_corr(data$Freedom.to.make.life.choices,data$Ladder.score)
cor(data$Freedom.to.make.life.choices,data$Ladder.score)
# more freedom more happiness
plot_corr(data$Social.support,data$Generosity)
cor(data$Social.support,data$Generosity)
# low correlation small negative
plot_corr(data$Social.support,data$Ladder.score)
cor(data$Social.support,data$Ladder.score)
# strong positive corr --> like scandinavian countries

# done a bunch of correlation above
# now maybe some z tests?
# can do hyp tests on regression lines' slopes

