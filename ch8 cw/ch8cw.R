library(ggplot2)
library(dplyr)
 
dat = read.csv("C:/Users/redte/Documents/Stathw/ch8 cw/data.csv")

dat = dat[(!is.na(dat$Armspan_cm)),]

r = cor(dat$Armspan_cm, dat$Travel_time_to_School)
xbar = mean(dat$Armspan_cm) 
sx = sd(dat$Armspan_cm)
ybar = mean(dat$Travel_time_to_School)
sy = sd(dat$Travel_time_to_School)

b = r * sy / sx
print(b)

a = ybar - b * xbar
print(a)

print(lm(dat$Travel_time_to_School ~ dat$Armspan_cm))
summary(lm(dat$Travel_time_to_School ~ dat$Armspan_cm))

LSRL = function(x) {
  return(a + b * x)
}
dat = dat %>% mutate(residual = dat$Travel_time_to_School - LSRL(dat$Armspan_cm))

ggplot(dat, aes(x = Armspan_cm, y = Travel_time_to_School)) + 
  geom_abline(intercept = a, slope = b) + 
  geom_point()

ggplot(dat, aes(x = Travel_time_to_School, y = residual)) + 
  geom_hline(yintercept = 0) + 
  geom_point()

dat = read.csv("C:/Users/redte/Documents/Stathw/ch8 cw/data.csv")

dat = dat[((!is.na(dat$Armspan_cm)) & (dat$Armspan_cm > 100)),]

r = cor(dat$Armspan_cm, dat$Height_cm)
ybar = mean(dat$Armspan_cm) 
sy = sd(dat$Armspan_cm)
xbar = mean(dat$Height_cm)
sx = sd(dat$Height_cm)

b = r * sy / sx
print(b)

a = ybar - b * xbar
print(a)

print(lm(dat$Armspan_cm ~ dat$Height_cm))
summary(lm(dat$Armspan_cm ~ dat$Height_cm))

LSRL = function(x) {
  return(a + b * x)
}
dat = dat %>% mutate(residual = dat$Armspan_cm - LSRL(dat$Height_cm))

ggplot(dat, aes(x = Height_cm, y = Armspan_cm)) + 
  geom_abline(intercept = a, slope = b) + 
  geom_point()

ggplot(dat, aes(x = Armspan_cm, y = residual)) + 
  geom_hline(yintercept = 0) + 
  geom_point()