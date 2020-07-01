library(tidyverse)
dat = read.csv("CM000004910.csv")
glimpse(dat)
tidied_dat = dat %>%
  separate(col=DATE,into=c("Year","Month","Day"))
by_month = tidied_dat %>%
  group_by(Month)

jja <- tidied_dat %>%
  filter((Month=='06'|Month=='07'|Month=='08'))
summ <- jja %>%
  filter(Year>=1976 & Year<2020)%>%
  group_by(Year)%>%
  filter(PRCP<=3)%>%
  summarize(count=n())
p <- ggplot(summ,aes(x=Year,y=count))+
  geom_point()
spline.d <- as.data.frame(spline(summ$Year, summ$count))
p + geom_line(data = spline.d, aes(x = x, y = y))
ggplot(data = spline.d, aes(x = x, y = y))+
  geom_line()