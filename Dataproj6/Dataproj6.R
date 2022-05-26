library(ggplot2)
library(dplyr)
library(stats)
#library(tidyverse)
library(BSDA)

dat = read.csv("~/homicide.csv")
dat[dat == "Unknown"] = NA #easier to work with NA than string "Unknown"
colnames(dat)
head(dat)

#Q1 (1sample t-interval): Construct a 95% confidence interval for the age of victims in cases reported by the FOIA.
dat1 = dat[dat$Record.Source == "FOIA",]$Victim.Age
dat1 = dat1[!is.na(dat1)]
dat1 = dat1[!(dat1 == 0 | dat1 == 99 | dat1 > 200)]
age = dat1 #rename for histogram
hist(age)
age_sample = sample(age, 2000, replace = F)
hist(age_sample)
mu = mean(age_sample)
sigma = sd(age_sample)
se = sigma/sqrt(2000)
df = length(age_sample) - 1
tscore = - qt(.025, df)
cIntLow = mu - tscore * se
cIntHigh = mu + tscore * se
print(cIntLow)
print(cIntHigh)
  
#Q2 (2sample t-test): Are perpetrators roughly the same age as victims in cases from Michigan?
dat2 = dat[dat$State == "Michigan",]
perp = dat2$Perpetrator.Age
vict = dat2$Victim.Age
perp = perp[!(perp == 0 | perp == 99 | perp > 200 | is.na(perp))]
vict = perp[!(vict == 0 | vict == 99 | vict > 200 | is.na(vict))]
hist(perp)
hist(vict)
perp_sample = sample(perp, 1700, replace = F)
vict_sample = sample(vict, 1600, replace = F)
hist(perp_sample)
hist(vict_sample)
muP = mean(perp_sample)
muV = mean(vict_sample)
sigP = sd(perp_sample)
sigV = sd(vict_sample)
SE = sqrt(sigP^2 / 1700 + sigV^2 / 1600)
df = 1599
tscore = (muP-muV)/SE
pt(-tscore, 1900) * 2 #for two tailed
#Q3 (2proportion z-test): 
dat3 = dat[dat$State == "Michigan",]
pm = dat3$Perpetrator.Sex 
vm = dat3$Victim.Sex
pm = pm[!is.na(pm)]
vm = vm[!is.na(vm)]
pms = sample(pm, 1900, replace = F)
vms = sample(vm, 2800, replace = F)
pmp = length(pms[pms == "Male"]) / (length(pms))
vmp = length(vms[vms == "Male"]) / (length(vms))
pPool = (length(pms[pms == "Male"]) + length(vms[vms == "Male"])) / (length(pms)+length(vms))
SE = sqrt((pPool * (1 - pPool) * (1/ length(pms) + 1/length(vms))))
