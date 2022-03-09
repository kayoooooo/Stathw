library("ggplot2")
library("dplyr")

#Part A
dat = read.csv("C:/Users/redte/Documents/Stathw/Dataproj3/cholesterol.csv")

dat = mutate(dat, rand1 = runif(1000))
median1 = median(dat$rand1)
for (x in c(1:1000)) {
  if (dat[x,]$rand1 > median1) {
    dat[x,]$rand1 = "A"
  }
  else {
    dat[x,]$rand1 = "B"
  }
}

datA = dat[(dat$rand1 == "A"),]
datB = dat[(dat$rand1 == "B"),]

#Part B
datM = dat[(dat$Gender == "Male"),]
datF = dat[(dat$Gender == "Female"),]