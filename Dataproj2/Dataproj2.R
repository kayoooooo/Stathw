library(ggplot2)
library(dplyr)

dat = read.csv("C:/Users/redte/Documents/Stathw/Dataproj2/tomatoes.csv")

dat1 = dat[grepl("Comedy", dat$genres),] #any movies that are comedy and maybe other genres
dat2 = dat[grepl("Action", dat$genres),] #any movies that are action and another genre

hist(dat1$tomatometer_rating)
hist(dat2$tomatometer_rating)

ggplot(dat1, aes(x = tomatometer_rating, y = audience_rating)) + 
  geom_point()

ggplot(dat2, aes(x = tomatometer_rating, y = audience_rating)) + 
  geom_point()

model1 = lm(dat1$audience_rating ~ dat1$tomatometer_rating)
model2 = lm(dat2$audience_rating ~ dat2$tomatometer_rating)
summary(model1)
summary(model2)
deviance(model1)
deviance(model2)