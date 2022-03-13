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

hist(datA$A.improvement, xlab = "Change in cholesterol level", main = "Cholesterol change using drug A", ylim = c(0,100), xlim = c(-5,25))
hist(datB$B.improvement, xlab = "Change in cholesterol level", main = "Cholesterol change using drug B", ylim = c(0,100), xlim = c(-5,25))
boxplot(datA$A.improvement, main = "Drug A Recipients", ylab = "Change in cholesterol level", ylim = c(-5,25))
boxplot(datB$B.improvement, main = "Drug B Recipients", ylab = "Change in cholesterol level", ylim = c(-5,25))


print(quantile(datA$A.improvement))
print(mean(datA$A.improvement))
print(IQR(datA$A.improvement))
print(sd(datA$A.improvement))

print(quantile(datB$B.improvement))
print(mean(datB$B.improvement))
print(IQR(datB$B.improvement))
print(sd(datB$B.improvement))

#Part B
dat$rand1 = runif(1000)
datM = dat[(dat$Gender == "Male"),]
median1 = median(datM$rand1)
for (x in c(1:500)) {
  if (datM[x,]$rand1 > median1) {
    datM[x,]$rand1 = "A"
  }
  else {
    datM[x,]$rand1 = "B"
  }
}
datMA = datM[(datM$rand1 == "A"),]
datMB = datM[(datM$rand1 == "B"),]

datF = dat[(dat$Gender == "Female"),] 
median1 = median(datF$rand1)
for (x in c(1:500)) {
  if (datF[x,]$rand1 > median1) {
    datF[x,]$rand1 = "A"
  }
  else {
    datF[x,]$rand1 = "B"
  }
}
datFA = datF[(datF$rand1 == "A"),]
datFB = datF[(datF$rand1 == "B"),]