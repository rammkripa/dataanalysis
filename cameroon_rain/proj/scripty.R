library(tidyverse)
dat = read.csv("CM000004910.csv")
glimpse(dat)
tidied_dat = dat %>%
  separate(col=DATE,into=c("Year","Month","Day"))
by_month = tidied_dat %>%
  group_by(Month)

jjabreaks = tidied_dat %>%
  filter(PRCP<=3,(Month=='06'|Month=='07'|Month=='08'))
summ = jjabreaks %>%
  group_by(Year)%>%
  summarize(count=n())
ggplot(summ,aes(x=Year,y=count))+
  geom_point()