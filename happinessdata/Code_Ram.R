# Ram's Code
data2019 = read.csv(file.choose())
dataprev = read.csv(file.choose())
data2019
dataprev
library(ggplot2)
library(dplyr)

# EXPLORATORY
# Relationship between Ladder Score and gdp per capita
# define rich as top 15% economies
ggplot(data2019,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color="red")+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP")+
  labs(x="Logged GDP per capita",y="Happiness Score")
cor(data2019$Ladder.score,data2019$Logged.GDP.per.capita)
# It looks like there is a positive correlation
# Does Logged GDP per capita follow normal curve?
ggplot(data2019,aes(x=Logged.GDP.per.capita,y=..density..))+
  geom_histogram(bins = 20,color='green')
# roughly normal
# Hypothesis test: Question: Is this correlation real?
'do hyp test'
# Answer: looks like it is real
# Question: Does this correlation hold for poor countries and rich countries separately?
# Let's define rich countries as countries having GDP above the mean
# rich countries
rich_data = filter(data2019,Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)+sd(data2019$Logged.GDP.per.capita)))#+sd(data2019$Logged.GDP.per.capita)))
ggplot(rich_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color='red')+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP for Rich countries")+
  labs(x="Logged GDP per capita",y="Happiness Score")
cor(rich_data$Ladder.score,rich_data$Logged.GDP.per.capita)
# Let's define poor countries as countries having GDP below the mean
# poorcountries
poor_data = filter(data2019,Logged.GDP.per.capita<(mean(data2019$Logged.GDP.per.capita)-sd(data2019$Logged.GDP.per.capita)))#-sd(data2019$Logged.GDP.per.capita)))
ggplot(poor_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color='green')+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP for Poor countries")+
  labs(x="Logged GDP per capita",y="Happiness Score")
cor(poor_data$Ladder.score,poor_data$Logged.GDP.per.capita)
#middle countries
#middle_data = filter(data2019,(Logged.GDP.per.capita<(Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)+sd(data2019$Logged.GDP.per.capita))) AND (Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)-sd(data2019$Logged.GDP.per.capita)))
ggplot(poor_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color='green')+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP for Poor countries")+
  labs(x="Logged GDP per capita",y="Happiness Score")
cor(poor_data$Ladder.score,poor_data$Logged.GDP.per.capita)
# Answer: It looks like the correlation is stronger for rich countries than poor countries
# Question: what if a country gdp increases over time?
# first, plot for all countries, all years
ggplot(dataprev,aes(x=Log.GDP.per.capita,y=Life.Ladder,color=factor(year)))+
  geom_point()+
  geom_smooth(method='lm',color = 'black')+
  ggtitle("Correlation between Happiness and GDP for countries over time")+
  labs(x="Logged GDP per capita",y="Happiness Score")

# let's take the case of just one country, India for example
india_data = filter(dataprev,Country.name=='India')
ggplot(india_data,aes(x=Log.GDP.per.capita,y=Life.Ladder,color=factor(year)))+
  geom_point()+
  geom_smooth(method='lm',color = 'black')+
  ggtitle("Correlation between Happiness and GDP for India over time")+
  labs(x="Logged GDP per capita",y="Happiness Score")
# let's look at the US
us_data = filter(dataprev,Country.name=='United States')
ggplot(us_data,aes(x=Log.GDP.per.capita,y=Life.Ladder,color=factor(year)))+
  geom_point()+
  geom_smooth(method='lm',color = 'black')+
  ggtitle("Correlation between Happiness and GDP for US over time")+
  labs(x="Logged GDP per capita",y="Happiness Score")
# looks like people are less happy
ggplot(us_data,aes(y=Life.Ladder,x=year))+
  geom_point()+
  geom_smooth(method='lm')
# looks like happiness decreasing over time
ggplot(india_data,aes(y=Life.Ladder,x=year))+
  geom_point()+
  geom_smooth(method='lm')
# india is a mood
cor(india_data$Life.Ladder,india_data$year)
ggplot(us_data,aes(y=Life.Ladder,x=year))+
  geom_point()+
  geom_smooth(method='lm')
