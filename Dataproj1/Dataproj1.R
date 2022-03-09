library(ggplot2)
library(qqplotr)
library(dplyr)

#Part 1
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

#Part 2
dat4 = read.csv("C:/Users/redte/Documents/Stathw/Dataproj1/ohio.csv")
dat1 = dat1[(!is.na(dat1$Armspan_cm)),]
dat4 = dat4[(!is.na(dat1$Armspan_cm)),]
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
boxplot(dat1$Armspan_cm, xlab = "Lusby Period 4", ylab = "Arm Span (cm)")
boxplot(dat4$Armspan_cm, xlab = "Ohio Random Sample", ylab = "Arm Span (cm)")
hist(dat1$Armspan_cm, xlab = "Lusby Period 4", ylab = "Arm Span (cm)")
hist(dat4$Armspan_cm, xlab = "Ohio Random Sample", ylab = "Arm Span (cm)")

meand1 = mean(dat1$Armspan_cm, na.rm = TRUE)
sdd1 = sd(dat1$Armspan_cm, na.rm = TRUE)
dat1 = dat1 %>%
  mutate(zscore = (Armspan_cm - meand1) / sdd1)
meand4 = mean(dat4$Armspan_cm, na.rm = TRUE)
sdd4 = sd(dat4$Armspan_cm, na.rm = TRUE)
dat4 = dat4 %>%
  mutate(zscore = (Armspan_cm - meand4) / sdd4)
ggplot(mapping = aes(sample = dat1$Armspan_cm)) +
  stat_qq_point(size = 2) +
  stat_qq_line(color = "blue") +
  xlab("Lusby Period 4") + 
  ylab("Armspan (cm)")
ggplot(mapping = aes(sample = dat4$Armspan_cm)) +
  stat_qq_point(size = 2) +
  stat_qq_line(color = "blue") +
  xlab("Ohio Random Sample") + 
  ylab("Armspan (cm)")
print(length(dat1[((dat1$zscore) < 1 & (dat1$zscore > -1)),]$zscore)/length(dat1$zscore))
print(length(dat1[((dat1$zscore) < 2 & (dat1$zscore > -2)),]$zscore)/length(dat1$zscore))
print(length(dat1[((dat4$zscore) < 1 & (dat4$zscore > -1)),]$zscore)/length(dat4$zscore))
print(length(dat1[((dat4$zscore) < 2 & (dat4$zscore > -2)),]$zscore)/length(dat4$zscore))