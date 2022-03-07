#17 (15 simulation run)
rand = data.frame(x = floor(runif(100, min=1, max=11)))
print(length(rand[rand$x == 7,]))

#21 simulation
dat = c()
for (x in c(1:1000)) {
  points = 0
  for (x in c(1:2)) {
    var = runif(1)
    if (var < .72) {
      points = points + 1
    }
  }
  dat = append(dat, points)
}
print(mean(dat))