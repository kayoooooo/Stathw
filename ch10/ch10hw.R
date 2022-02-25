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


#Problem 32