library(dplyr)
library(ggplot2)
corona = read.csv("~/Downloads/owid-covid-data.csv")
recent = filter(corona,date=='2020-05-25')
recent = filter(recent,location!='World')

### EXPLORATION

# is there a correlation between gdp per capita and total cases?
ggplot(recent,aes(x=gdp_per_capita,y=total_cases))+
  geom_point(color="skyblue")+
  geom_smooth(method='lm')
x = lm(gdp_per_capita ~ total_cases,recent)
summary(x)
cor(recent$gdp_per_capita,recent$total_cases,na.rm=TRUE)
#seems to be a slightly positive correlation


# is there a correlation between population density and total cases?
'ggplot(recent,aes(x=population_density,y=total_cases))+
  geom_point(color="green")+
  geom_smooth(method="lm")
x = lm(population_density ~ total_cases,recent)
summary(x)'
#seems to be no correlation however p value is high so it could be random

## What about cases per million?

# is there a correlation between gdp per capita and total cases?
ggplot(recent,aes(x=gdp_per_capita,y=total_cases_per_million))+
  geom_point(color="skyblue")+
  geom_smooth(method='lm')
x = lm(gdp_per_capita ~ total_cases_per_million,recent)
summary(x)
cor(recent$gdp_per_capita,recent$total_cases)
#seems to be a strong positive correlation
# more gdp = more cases??
#aha
# is there a correlation between population density and total cases?
'ggplot(recent,aes(x=population_density,y=total_cases_per_million))+
  geom_point(color="green")+
  geom_smooth(method="lm")
x = lm(population_density ~ total_cases_per_million,recent)
summary(x)'
#seems to be no correlation however p value is high so it could be random

## What about testing
## Idea: if there's more gdp, there's more testing, hence skewing the "real" total cases
ggplot(recent,aes(x=gdp_per_capita,y=total_tests_per_thousand))+
  geom_point()+
  geom_smooth(method='lm')
summary(lm(gdp_per_capita~total_tests_per_thousand,recent))
# more gdp = more tests
ggplot(recent,aes(x=total_cases_per_million,y=total_tests_per_thousand))+
  geom_point()+
  geom_smooth(method='lm')
summary(lm(total_cases_per_million~total_tests_per_thousand,recent))
# more testing = more cases detected  

## A hypothesis test:
## Are no. of tests per thousand and no. of cases per million independent
## Let's do a chi squared test

### but first
frame2 = filter(recent,!(is.na(total_tests_per_thousand)))
ggplot(frame2,aes(x=total_cases_per_million,y=total_tests_per_thousand))+
  geom_point()+
  geom_smooth(method='lm')
summary(lm(total_cases_per_million~total_tests_per_thousand,frame2))
###
avg_tests = mean(frame2$total_tests_per_thousand)
avg_cases = mean(frame2$total_cases_per_million)
above_avg_test = filter(frame2,total_tests_per_thousand>avg_tests)
below_avg_test = filter(frame2,total_tests_per_thousand<avg_tests)
aa = length(filter(above_avg_test,total_cases_per_million>avg_cases)$iso_code)
ab = length(filter(above_avg_test,total_cases_per_million<avg_cases)$iso_code)
ba = length(filter(below_avg_test,total_cases_per_million>avg_cases)$iso_code)
bb = length(filter(below_avg_test,total_cases_per_million<avg_cases)$iso_code)
actual = c(aa,ab,ba,bb)
evaa = (aa+ab)*(aa+ba)/(aa+ab+ba+bb)
evab = (aa+ab)*(ab+bb)/(aa+ab+ba+bb)
evba = (ba+bb)*(ba+aa)/(aa+ab+ba+bb)
evbb = (bb+ab)*(bb+ba)/(aa+ab+ba+bb)
exp = c(evaa,evab,evba,evbb)
chisqr = sum(((actual-exp)^2)/exp)
pval = (1-pchisq(chisqr,df=1))*100
pval
# pval is 1.32 percent so reject null
# dependent

## okay so now linear modelling
### training
usa_data = filter(corona,iso_code=="USA")
usa_data$dayno = 1:147
ggplot(usa_data,aes(x=total_tests_per_thousand,y=total_cases_per_million))+
  geom_point()+
  geom_smooth(method='lm')
plot(total_cases_per_million~total_tests_per_thousand,usa_data)
fit1 = lm(total_cases_per_million~total_tests_per_thousand+I(total_tests_per_thousand^2)+I(total_tests_per_thousand^4),usa_data)
fit1
summary(fit1)
plot(total_cases_per_million~total_tests_per_thousand,usa_data)
x = usa_data$total_tests_per_thousand
points(x[!is.na(x)],fitted(fit1),col='blue',pch=8)
### model has been created

## now to see if it predicts well

## Inputting new data

corona_updated = read.csv("~/Downloads/owid-covid-data-updated.csv")
usa_new = filter(corona_updated,iso_code=="USA")
usa_new$dayno = 1:length(usa_new$total_cases_per_million)
##predict(fit1)
test_data = filter(usa_new,(dayno<158 & dayno>146))
prediction_data = predict(fit1,data.frame(total_tests_per_thousand=test_data$total_tests_per_thousand),interval="confidence")
predicted_values = prediction_data[1:11]
test_data$predicted_values = predicted_values
ggplot(test_data)+
  geom_point(aes(x=total_tests_per_thousand,y=total_cases_per_million,col='red'))+
  geom_point(aes(x=total_tests_per_thousand,y=predicted_values,col='blue'))

### ALL TOGETHER
usa_new
pred_frame = predict(fit1,data.frame(total_tests_per_thousand=usa_new$total_tests_per_thousand),interval="confidence")
pred_vals = pred_frame[1:158]
usa_new$predicted = pred_vals
ggplot(usa_new)+
  geom_point(aes(x=total_tests_per_thousand,y=predicted,col='green'))+
  geom_point(aes(x=total_tests_per_thousand,y=total_cases_per_million,col='red'))

summary(fit1)

#### INDIA CORONAVIRUS DATA
### fit
india_old = filter(corona,iso_code=="IND")
india_new = filter(corona_updated,iso_code=="IND")
india_new$dayno = 1:158
plot(total_cases_per_million~total_tests_per_thousand,india_old)
fit_ind = lm(total_cases_per_million~total_tests_per_thousand,india_old)
x = india_old$total_tests_per_thousand
points(x[!is.na(x)],fitted(fit_ind),col='blue',pch=8)
### predict
test_data = filter(india_new,(dayno<158 & dayno>146))
prediction_data = predict(fit_ind,data.frame(total_tests_per_thousand=test_data$total_tests_per_thousand),interval="confidence")
predicted_values = prediction_data[1:11]
test_data$predicted_values = predicted_values
ggplot(test_data)+
  geom_point(aes(x=total_tests_per_thousand,y=total_cases_per_million,col='red'))+
  geom_point(aes(x=total_tests_per_thousand,y=predicted_values,col='blue'))
