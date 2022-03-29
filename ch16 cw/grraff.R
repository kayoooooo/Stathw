library("ggplot2")
library("dplyr")
#randm = rnorm(50,17,3.2)
#print(mean(randm))
#print(sd(randm))
#randf = rnorm(50,15,2.8)
#print(mean(randf))
#print(sd(randf))
#already run so no redo

couple = randm + randf
dat = data.frame(randm, randf)
ggplot(data = dat, aes(x = randm, y = randf)) + 
  geom_point()
print(summary(lm(randf~randm)))
print(cor(randf,randm))
dat = mutate(dat, couple)
print(mean(couple))
print(sd(couple))

sortm = sort(randm)
sortf = sort(randf)
dat1 = data.frame(sortm,sortf)
ggplot(data = dat1, aes(x = sortm, y = sortf)) + 
  geom_point()
print(summary(lm(sortf~sortm)))
print(cor(sortf,sortm))