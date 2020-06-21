#Code_ram 2

### Classification of Countries
WHR20_data = read.csv(file.choose())
WHR20_previous_years = read.csv(file.choose())
data2019 = WHR20_data
dataprev = WHR20_previous_years
rich_data = filter(data2019,Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)+sd(data2019$Logged.GDP.per.capita)))
poor_data = filter(data2019,Logged.GDP.per.capita<(mean(data2019$Logged.GDP.per.capita)-sd(data2019$Logged.GDP.per.capita)))
middle_data = filter(data2019,(Logged.GDP.per.capita<(mean(data2019$Logged.GDP.per.capita)+sd(data2019$Logged.GDP.per.capita))) & (Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)-sd(data2019$Logged.GDP.per.capita))))
###
## Classify dataprev
rich_data = filter(data2019,Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)+sd(data2019$Logged.GDP.per.capita)))
poor_data = filter(data2019,Logged.GDP.per.capita<(mean(data2019$Logged.GDP.per.capita)-sd(data2019$Logged.GDP.per.capita)))
middle_data = filter(data2019,(Logged.GDP.per.capita<(mean(data2019$Logged.GDP.per.capita)+sd(data2019$Logged.GDP.per.capita))) & (Logged.GDP.per.capita>(mean(data2019$Logged.GDP.per.capita)-sd(data2019$Logged.GDP.per.capita))))
### Plotting scatterplots for all three categories

## rich
ggplot(rich_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color='red')+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP for Rich countries")+
  labs(x="Logged GDP per capita",y="Happiness Score")
rich_corr = cor(rich_data$Ladder.score,rich_data$Logged.GDP.per.capita)

## poor
ggplot(poor_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color='red')+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP for Poor countries")+
  labs(x="Logged GDP per capita",y="Happiness Score")
poor_corr = cor(poor_data$Ladder.score,poor_data$Logged.GDP.per.capita)

## middle
ggplot(middle_data,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_point(color='red')+
  geom_smooth(method='lm')+
  ggtitle("Correlation between Happiness and GDP for Middle countries")+
  labs(x="Logged GDP per capita",y="Happiness Score")
middle_corr = cor(middle_data$Ladder.score,middle_data$Logged.GDP.per.capita)


# correlation is much stronger for middle countries

### All countries over time gdp vs ladder score
ggplot(dataprev,aes(x=Log.GDP.per.capita,y=Life.Ladder,color=factor(year)))+
  geom_point()+
  geom_smooth(method='lm',color = 'black')+
  ggtitle("Correlation between Happiness and GDP for countries over time")+
  labs(x="Logged GDP per capita",y="Happiness Score")
lm(dataprev$Life.Ladder~dataprev$Log.GDP.per.capita)
r = 0.7465

### time vs ladder score: Is there a general decreaseing trend in happiness?
ggplot(dataprev,aes(x=year,y=Log.GDP.per.capita,color=factor(year)))+
  geom_boxplot()

data2008 = filter(dataprev,year==2008)
data2009 = filter(dataprev,year==2009)
#2008 data --> financial crisis z test--> gdp
# Hypothesis test
# was 2008 gdp lower than most?
all_gdp = c(dataprev$Log.GDP.per.capita,data2019$Logged.GDP.per.capita)
obs = mean(data2008$Log.GDP.per.capita,na.rm=TRUE)
exp = mean(all_gdp,na.rm=TRUE)
se = sd(all_gdp,na.rm=TRUE)/sqrt(length(data2008$Log.GDP.per.capita))
z = (obs-exp)/se
pnorm(z)


#2008 data --> financial crisis z test --> Happiness
# Hypothesis test
# was 2008 happiness lower than most?
all_ladder = c(dataprev$Life.Ladder,data2019$Ladder.score)
obs = mean(data2008$Life.Ladder,na.rm=TRUE)
exp = mean(all_ladder,na.rm=TRUE)
se = sd(all_ladder,na.rm=TRUE)/sqrt(length(data2008$Life.Ladder))
z = (obs-exp)/se
pnorm(z)
# people were not unusually unhappy
# was 2009 faith in govt lower than most?
all_conf = dataprev$Confidence.in.national.government
obs = mean(data2009$Confidence.in.national.government,na.rm=TRUE)
exp = mean(all_conf,na.rm=TRUE)
se = sd(all_conf,na.rm=TRUE)/sqrt(length(data2009$Life.Ladder))
z = (obs-exp)/se
pnorm(z)
# people were not unusually unhappy

### now for rich poor and middle, a chi sq test to see the difference!
rich_data = filter(dataprev,Log.GDP.per.capita>(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)+sd(dataprev$Log.GDP.per.capita,na.rm=TRUE)))
poor_data = filter(dataprev,Log.GDP.per.capita<(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)-sd(dataprev$Log.GDP.per.capita,na.rm=TRUE)))
middle_data = filter(dataprev,(Log.GDP.per.capita<(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)+sd(dataprev$Log.GDP.per.capita,na.rm=TRUE))) & (Log.GDP.per.capita>(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)-sd(dataprev$Log.GDP.per.capita,na.rm=TRUE))))

### Chi squared test on rich, poor and middle countries
## Let's define happy as countries with above average ladder score

avg_ladder = mean(dataprev$Life.Ladder,na.rm=TRUE)
grand_total = length(rich_data$Life.Ladder)+length(poor_data$Life.Ladder)+length(middle_data$Life.Ladder)
rich_more = length(filter(rich_data,Life.Ladder>avg_ladder)$Life.Ladder)
poor_more = length(filter(poor_data,Life.Ladder>avg_ladder)$Life.Ladder)
middle_more = length(filter(middle_data,Life.Ladder>avg_ladder)$Life.Ladder)
more = c(rich_more,middle_more,poor_more)
label = c("rich","middle","poor")
less = c((length(rich_data$Country.name)-rich_more),(length(middle_data$Country.name)-middle_more),(length(poor_data$Country.name)-poor_more))
test_frame = data.frame(label,more,less)
num_more = rich_more+poor_more+middle_more
num_less = grand_total-num_more
### Chi Square test

chisqr = ((rich_more-(length(rich_data$Life.Ladder)*num_more/grand_total))^2)/(length(rich_data$Life.Ladder)*num_more/grand_total)+
  ((poor_more-(length(poor_data$Life.Ladder)*num_more/grand_total))^2)/(length(poor_data$Life.Ladder)*num_more/grand_total)+
  ((middle_more-(length(middle_data$Life.Ladder)*num_more/grand_total))^2)/(length(middle_data$Life.Ladder)*num_more/grand_total)+
  ((less[1]-(length(rich_data$Life.Ladder)*num_less/grand_total))^2)/(length(rich_data$Life.Ladder)*num_less/grand_total)+
  ((less[2]-(length(middle_data$Life.Ladder)*num_less/grand_total))^2)/(length(middle_data$Life.Ladder)*num_less/grand_total)+
  ((less[3]-(length(poor_data$Life.Ladder)*num_less/grand_total))^2)/(length(poor_data$Life.Ladder)*num_less/grand_total)
df = (3-1)*(2-1)
p = 1-pchisq(chisqr,df)
p

curve(dchisq(x,df),from = 0, to = 10)
# there is clearly a difference between the groups
"



"
rich_data = filter(dataprev,Log.GDP.per.capita>(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)+sd(dataprev$Log.GDP.per.capita,na.rm=TRUE)))
poor_data = filter(dataprev,Log.GDP.per.capita<(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)-sd(dataprev$Log.GDP.per.capita,na.rm=TRUE)))
middle_data = filter(dataprev,(Log.GDP.per.capita<(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)+sd(dataprev$Log.GDP.per.capita,na.rm=TRUE))) & (Log.GDP.per.capita>(mean(dataprev$Log.GDP.per.capita,na.rm=TRUE)-sd(dataprev$Log.GDP.per.capita,na.rm=TRUE))))
avg_ladder = mean(dataprev$Life.Ladder,na.rm=TRUE)
grand_total = length(rich_data$Life.Ladder)+length(poor_data$Life.Ladder)+length(middle_data$Life.Ladder)
rich_more = length(filter(rich_data,Life.Ladder>avg_ladder)$Life.Ladder)
poor_more = length(filter(poor_data,Life.Ladder>avg_ladder)$Life.Ladder)
middle_more = length(filter(middle_data,Life.Ladder>avg_ladder)$Life.Ladder)
more = c(rich_more,middle_more,poor_more)
label = c("rich","middle","poor")
less = c((length(rich_data$Country.name)-rich_more),(length(middle_data$Country.name)-middle_more),(length(poor_data$Country.name)-poor_more))
test_frame = data.frame(label,more,less)
num_more = rich_more+poor_more+middle_more
num_less = grand_total-num_more
test_frame
### Chi Square test

chisqr = ((rich_more-(length(rich_data$Life.Ladder)*num_more/grand_total))^2)/(length(rich_data$Life.Ladder)*num_more/grand_total)+
  ((poor_more-(length(poor_data$Life.Ladder)*num_more/grand_total))^2)/(length(poor_data$Life.Ladder)*num_more/grand_total)+
  ((middle_more-(length(middle_data$Life.Ladder)*num_more/grand_total))^2)/(length(middle_data$Life.Ladder)*num_more/grand_total)+
  ((less[1]-(length(rich_data$Life.Ladder)*num_less/grand_total))^2)/(length(rich_data$Life.Ladder)*num_less/grand_total)+
  ((less[2]-(length(middle_data$Life.Ladder)*num_less/grand_total))^2)/(length(middle_data$Life.Ladder)*num_less/grand_total)+
  ((less[3]-(length(poor_data$Life.Ladder)*num_less/grand_total))^2)/(length(poor_data$Life.Ladder)*num_less/grand_total)
print("chi sq statistic is ")
print(chisqr)
df = (3-1)*(2-1)
p = 1-pchisq(chisqr,df)
print("p value is")
print(p)
curve(dchisq(x,df),from = 0, to = 4)

ggplot(dataprev,aes(x=Log.GDP.per.capita,y=Life.Ladder,color=factor(year)))+
  geom_point()+
  #geom_point(data2019,aes(x=Logged.GDP.per.capita,y=Ladder.score))+
  geom_smooth(method='lm',color = 'black')+
  geom_point(data2019)+
  ggtitle("Correlation between Happiness and GDP")+
  labs(x="Logged GDP per capita",y="Happiness Score")

rich_data$countrytypes <- factor(c("richcountry"))
middle_data$countrytypes <- factor(c("middlecountry"))
poor_data$countrytypes <- factor(c("poorcountry"))
datanew <- rbind(rich_data, middle_data, poor_data, stringsAsFactors = TRUE)
ggplot(datanew,aes(x=Log.GDP.per.capita,y=Life.Ladder,color=countrytypes))+
  geom_point(alpha=0.1)+
  geom_smooth(method='lm')+
  geom_smooth(method='lm',color="black")+
  ggtitle("Correlation between Happiness and GDP")+
  labs(x="Logged GDP per capita",y="Happiness Score")
