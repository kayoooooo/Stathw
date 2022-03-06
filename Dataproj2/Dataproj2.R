library(ggplot2)
library(dplyr)

dat = read.csv("C:/Users/redte/Documents/Stathw/Dataproj2/tomatoes.csv")

dat1 = dat[grepl("Comedy", dat$genres),] #any movies that are comedy and maybe other genres
dat2 = dat[grepl("Action", dat$genres),] #any movies that are action and maybe other genres
dat1 = dat1[(!is.na(dat1$tomatometer_rating) & !is.na(dat1$audience_rating)),]
dat2 = dat2[(!is.na(dat2$tomatometer_rating) & !is.na(dat2$audience_rating)),]

hist(dat1$tomatometer_rating, xlab = "Tomatometer rating", main = "Tomatometer Ratings of Comedy Movies", ylim = c(0,800))
hist(dat2$tomatometer_rating, xlab = "Tomatometer rating", main = "Tomatometer Ratings of Action Movies", ylim = c(0,500))
print(length(dat1[(dat1$tomatometer_rating>80 & dat1$tomatometer_rating<90),]$tomatometer_rating))

ggplot(dat1, aes(x = tomatometer_rating, y = audience_rating)) + 
  ggtitle("Comedy Movie Rating Comparisons") + 
  xlab("Tomatometer Rating") + 
  ylab("Audience Rating") + 
  geom_point()


ggplot(dat2, aes(x = tomatometer_rating, y = audience_rating)) + 
  ggtitle("Action Movie Rating Comparisons") + 
  xlab("Tomatometer Rating") + 
  ylab("Audience Rating") + 
  geom_point()

model1 = lm(dat1$audience_rating ~ dat1$tomatometer_rating)
model2 = lm(dat2$audience_rating ~ dat2$tomatometer_rating)
summary(model1)
summary(model2)
deviance(model1)
deviance(model2)
print(cor(dat1$tomatometer_rating, dat1$audience_rating))