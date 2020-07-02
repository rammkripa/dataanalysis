library(tidyverse)
library(lubridate)
dat = read.csv("CM000004910.csv")
glimpse(dat)
tidied_dat = dat %>%
  mutate(DATE=ymd(DATE)) %>%
  mutate(month=month(DATE),year = year(DATE))
  

jja <- tidied_dat %>%
  filter((month>=6)&(month<=8))
jto <- tidied_dat %>%
  filter((month>=6)&(month<=10))
viz_breaks = function(df,lev,nam){
summ <- df %>%
  filter(year>=1976 & year<2020)%>%
  group_by(year)%>%
  filter(PRCP<=lev)%>%
  summarize(count=n())
spline.d <- as.data.frame(spline(summ$year, summ$count))
ggplot(data = spline.d, aes(x = x, y = y))+
  geom_line()+
  labs(title=nam)
}
viz_breaks(jja,3,"jja breaks")
viz_breaks(jto,3,"jto breaks")