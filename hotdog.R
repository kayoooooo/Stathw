library(dplyr)
library(ggplot2)

data = read.csv(file = "C:\\Users\\redte\\Documents\\Stathw\\HOTDOG.csv")
print(data)

x = data$beef
y = data$meat
z = data$poultry
hist(x, xlab = "Calories in beef hotdogs", xlim = c(80,200), breaks = c(80,90,100,110,120,130,140,150,160,170,180,190,200), na.rm = TRUE)
hist(y, xlab = "Calories in meat hotdogs", xlim = c(80,200), breaks = c(80,90,100,110,120,130,140,150,160,170,180,190,200), na.rm = TRUE)
hist(z, xlab = "Calories in poultry hotdogs", xlim = c(80,200), breaks = c(80,90,100,110,120,130,140,150,160,170,180,190,200), na.rm = TRUE)
print(median(x))
print(median(y, na.rm = TRUE))
print(median(z, na.rm = TRUE))
print(quantile(x))
print(quantile(y, na.rm = TRUE))
print(quantile(z, na.rm = TRUE))
print(IQR(x))
print(IQR(y, na.rm = TRUE))
print(IQR(z, na.rm = TRUE))