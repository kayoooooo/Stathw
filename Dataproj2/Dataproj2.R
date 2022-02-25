library(ggplot2)
library(dplyr)

dat = read.csv("C:/Users/redte/Documents/Stathw/Dataproj2/tomatoes.csv")

dat1 = dat[dat$genres == "Comedy",]
dat2 = dat[grepl("Action", dat$genres),]

ggplot(dat1, aes(x = tomatometer_rating, y = audience_rating)) + 
  geom_point()

ggplot(dat2, aes(x = tomatometer_rating, y = audience_rating)) + 
  geom_point()