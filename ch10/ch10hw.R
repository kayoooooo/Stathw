library(ggplot2)
library(dplyr)

#Problem 24
weight = c(56,62,69,77,85,94,105)
lift = c(305, 325, 357.5, 367.5,390,405,425)
dat = data.frame(weight, lift)

ggplot(dat, aes(x = weight, y = lift)) + 
  geom_point()

summary(lm(dat$lift ~ dat$weight))
b = cor(weight,lift) * sd(lift) / sd(weight)
print(b)

a = mean(lift) - b * mean(weight)
print(a)
LSRL = function(x) {
  return ( a + b * x)
}
dat = mutate(dat, res = dat$lift - LSRL(dat$weight))

ggplot(dat, aes(x = lift, y = res)) + 
  geom_hline(yintercept = 0) + 
  geom_point()

dat = mutate(dat, newx = dat$weight ** (-1/2))
b = cor(dat$newx,lift) * sd(lift) / sd(dat$newx)
print(b)

a = mean(lift) - b * mean(dat$newx)
print(a)
LSRL = function(x) {
  return ( a + b * x)
}

dat = mutate(dat, res2 = dat$lift - LSRL(dat$newx))

ggplot(dat, aes(x = newx, y = lift)) + 
  geom_point()



ggplot(dat, aes(x = lift, y = res2)) + 
  geom_hline(yintercept = 0) + 
  geom_point()

#Problem 32
age = c(1:10) * 2
diameter = c(2.1, 3.9, 5.2, 6.2, 6.9, 7.6, 8.3, 9.1, 10.0, 11.4)
dat = data.frame(age, diameter)
summary(lm(dat$diameter ~ dat$age))