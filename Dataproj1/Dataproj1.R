library(ggplot2)
library(dplyr)
dat1 = read.csv("C:/Users/redte/Documents/Stathw/Dataproj1/dataprojlusby4.csv")
dat2 = read.csv("C:/Users/redte/Documents/Stathw/Dataproj1/dataprojlusby7.csv")
dat3 = read.csv("C:/Users/redte/Documents/Stathw/Dataproj1/dataprojstein2.csv")
ggplot(dat1, aes(x = Handed)) +
  xlab("Lusby Period 4 Handedness") + 
  geom_bar()
ggplot(dat2, aes(x = Handed)) +
  xlab("Lusby Period 7 Handedness") + 
  geom_bar()
ggplot(dat3[(dat3$Handed != ""),], aes(x = Handed)) +
  xlab("Stein Period 2 Handedness") + 
  geom_bar()
ggplot(dat1, aes(x = Handed)) + ggplot(dat2, aes(x = Handed)) + 
  geom_bar(position = position_dodge())
dat4 = read.csv("C:/Users/redte/Documents/Stathw/Dataproj1/ohio.csv")