
library(tidyverse)
library(lubridate)

dat = read_csv("data/CM000004910.csv")
glimpse(dat)
tidied_dat = dat %>%
  mutate(DATE=ymd(DATE)) %>%
  mutate(month=month(DATE),year = year(DATE))
  

jja <- tidied_dat %>%
  filter((month>=6)&(month<=8),year>=1976 & year<2020)
jto <- tidied_dat %>%
  filter((month>=6)&(month<=10))

jjaresults = jja %>%
  group_by(year) %>%
  summarize(total=sum(PRCP,na.rm=TRUE),num_dry=sum(PRCP<=3,na.rm=TRUE)) %>%
  filter(total!=0)
ggplot(jjaresults,mapping=aes(x=total))+
  geom_histogram(bins=10)

ggplot(jjaresults,mapping=aes(x=total,y=num_dry))+
  geom_point()+
  geom_smooth()

(1976+2019)/2

jjaresults %>%
  mutate(type=if_else(year<1997,'before 1997','after 1997')) %>%
ggplot(mapping=aes(x=total,color=type))+
  geom_freqpoly(bins=10)



ggplot(jjaresults,mapping=aes(x=year,y=num_dry))+
  geom_line()

  
  