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

hist(datA$A.improvement)
hist(datB$B.improvement)
boxplot(datA$A.improvement)
boxplot(datB$B.improvement)


print(quantile(datA$A.improvement))
print(mean(datA$A.improvement))
print(IQR(datA$A.improvement))
print(sd(datA$A.improvement))

print(quantile(datB$B.improvement))
print(mean(datB$B.improvement))
print(IQR(datB$B.improvement))
print(sd(datB$B.improvement))

#Part B
datM = dat[(dat$Gender == "Male"),]
datF = dat[(dat$Gender == "Female"),]