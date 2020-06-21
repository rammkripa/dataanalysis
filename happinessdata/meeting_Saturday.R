# Meeting saturday
# Govt Stuff: Allen
# Health stuff: Robert
# Economic stuff: Anni
# over time stuff: Ram, Shashwat
# splitting up the data set :
# for example correlation of life expectancy and happiness for countries with similar gdps

data = read.csv(file.choose())
data
library(ggplot2)
library(dplyr)
# looking at how rich countries and poor countries respond to gdp changes with respect to happiness
rich_countries_data = filter(data,Logged.GDP.per.capita>(mean(data$Logged.GDP.per.capita)+sd(data$Logged.GDP.per.capita))) 
# expand why
ggplot(rich_countries_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point()+
  geom_smooth(method="lm")
# among rich countries, no real correlation
poor_countries_data = filter(data,Logged.GDP.per.capita<(mean(data$Logged.GDP.per.capita)+sd(data$Logged.GDP.per.capita))) 
# expand why
ggplot(poor_countries_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point()+
  geom_smooth(method="lm")
# among poor countries, real correlation between gdp and happiness
mean(rich_countries_data$Freedom.to.make.life.choices)
mean(poor_countries_data$Freedom.to.make.life.choices)
# Can this difference be explained by chance? --> Hypothesis test material
mean(rich_countries_data$Perceptions.of.corruption)
mean(poor_countries_data$Perceptions.of.corruption)
# Can this difference be explained by chance? --> Hypothesis test material
mean(rich_countries_data$Generosity)
mean(poor_countries_data$Generosity)
# Can this difference be explained by chance? --> Hypothesis test material
ggplot(rich_countries_data,aes(x=Social.support,y=Healthy.life.expectancy))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Healthy.life.expectancy ~ Social.support,rich_countries_data)
summary(x)
# correlation may or may not be real because p value
ggplot(poor_countries_data,aes(x=Social.support,y=Healthy.life.expectancy))+
  geom_point()+
  geom_smooth(method="lm")
x=lm(Healthy.life.expectancy ~ Social.support,poor_countries_data)
summary(x)
# correlation is real Social support has higher impact on life expectancy in poor countries than rich.


# among poor countries, real correlation between gdp and happiness

