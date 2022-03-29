library("ggplot2")
library("dplyr")
# randm = rnorm(50,17,3.2)
# print(mean(randm))
# print(sd(randm))
# randf = rnorm(50,15,2.8)
# print(mean(randf))
# print(sd(randf))
# already run so no redo

couple = randm + randf
dat = data.frame(randm, randf)
ggplot(data = dat, aes(x = randm, y = randf)) + 
  geom_point()
print(summary(lm(randf~randm)))
print(cor(randf,randm))
dat = mutate(dat, couple)
print(mean(couple))
print(sd(couple))

#sorted
sortm = sort(randm)
sortf = sort(randf)
dat1 = data.frame(sortm,sortf)
ggplot(data = dat1, aes(x = sortm, y = sortf)) + 
  geom_point()
print(summary(lm(sortf~sortm)))
print(cor(sortf,sortm))
dat1 = mutate(dat1, combine = dat1$sortm+dat1$sortf)
print(mean(dat1$combine))
print(sd(dat1$combine))

#female descending
descf = sort(randf, TRUE)
dat2 = data.frame(sortm,descf)
ggplot(data = dat2, aes(x = sortm, y = descf)) + 
  geom_point()
print(summary(lm(descf~sortm)))
print(cor(descf,sortm))
dat2 = mutate(dat2, combine = dat2$sortm+dat2$descf)
print(mean(dat2$combine))
print(sd(dat2$combine))

#The very hard to do simulation

lowm = sortm[0:10]
midm = sortm[11:40]
highm = sortm[41:50]
lowf = sortf[0:10]
midf = sortf[11:40]
highf = sortf[41:50]
dat3 = data.frame(matrix(ncol = 2, nrow = 0))
colnames(dat3) = c("m", "f")
# for (x in c(1:10)) {
#   rand = sample(1:length(lowf), 1)
#   print(rand)
#   dat3[nrow(dat3) + 1,] = c(lowm[x],lowf[rand])
#   lowf = lowf[-rand]
# }
# for (x in c(1:30)) {
#   rand = sample(1:length(midf), 1)
#   print(rand)
#   dat3[nrow(dat3) + 1,] = c(midm[x],midf[rand])
#   midf = midf[-rand]
# }
# for (x in c(1:10)) {
#   rand = sample(1:length(highf), 1)
#   print(rand)
#   dat3[nrow(dat3) + 1,] = c(highm[x],highf[rand])
#   highf = highf[-rand]
# }
print(dat3)
ggplot(dat = dat3, aes(x = m, y = f)) + 
  geom_point()
print(summary(lm(dat3$f ~ dat3$m)))
print(cor(dat3$f, dat3$m))
