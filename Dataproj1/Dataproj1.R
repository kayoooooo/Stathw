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

dat4 = read.csv("C:/Users/redte/Documents/Stathw/Dataproj1/ohio.csv")
print(mean(dat1$Armspan_cm, na.rm = TRUE))
print(sd(dat1$Armspan_cm, na.rm = TRUE))
print(var(dat1$Armspan_cm, na.rm = TRUE))
print(quantile(dat1$Armspan_cm, na.rm = TRUE))
print(IQR(dat1$Armspan_cm, na.rm = TRUE))

print(mean(dat4$Armspan_cm, na.rm = TRUE))
print(sd(dat4$Armspan_cm, na.rm = TRUE))
print(var(dat4$Armspan_cm, na.rm = TRUE))
print(quantile(dat4$Armspan_cm, na.rm = TRUE))
print(IQR(dat4$Armspan_cm, na.rm = TRUE))
boxplot(dat1$Armspan_cm, xlab = "Lusby Period 4", ylab = "Arm Span (cm)")
boxplot(dat4$Armspan_cm, xlab = "Ohio Random Sample", ylab = "Arm Span (cm)")
hist(dat1$Armspan_cm, xlab = "Lusby Period 4", ylab = "Arm Span (cm)")
hist(dat4$Armspan_cm, xlab = "Ohio Random Sample", ylab = "Arm Span (cm)")

print(dat1[((dat1$Armspan_cm > 202) | (dat1$Armspan_cm < 130)),]$Armspan_cm)
print(dat4[((dat4$Armspan_cm > 223.525) | (dat4$Armspan_cm < 94.125)),]$Armspan_cm)
dat1 = dat1[((dat1$Armspan_cm < 202) & (dat1$Armspan_cm > 130)),]
dat4 = dat4[((dat4$Armspan_cm < 223.525) & (dat4$Armspan_cm > 94.125)),]
print(mean(dat1$Armspan_cm, na.rm = TRUE))
print(sd(dat1$Armspan_cm, na.rm = TRUE))
print(var(dat1$Armspan_cm, na.rm = TRUE))
print(quantile(dat1$Armspan_cm, na.rm = TRUE))
print(IQR(dat1$Armspan_cm, na.rm = TRUE))

print(mean(dat4$Armspan_cm, na.rm = TRUE))
print(sd(dat4$Armspan_cm, na.rm = TRUE))
print(var(dat4$Armspan_cm, na.rm = TRUE))
print(quantile(dat4$Armspan_cm, na.rm = TRUE))
print(IQR(dat4$Armspan_cm, na.rm = TRUE))