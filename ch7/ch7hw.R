library(ggplot2)
library(dplyr)

#15
cal = c(472,498,465,456,423,437,508,431,479,454,450,410,504,437,489,436,480,439,444,408)
time = c(21.4,30.8,37.7,33.5,32.8,39.5,22.8,34.1,33.9,43.8,42.4,43.1,29.2,31.3,28.6,32.9,30.6,35.1,33.0,43.7)
cor(cal, time)

#35
planet = c(1:9)
dist = c(36,67,93,142,484,887,1784,2796,3666)
dat = data.frame(planet, dist)
print(dat)
plot(planet, dist)
ggplot(dat, aes(x = planet, y = dist)) +
  geom_point()
dat$dist = log(dat$dist, 10)
ggplot(dat, aes(x = planet, y = dist)) +
  geom_point()