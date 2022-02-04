library(dplyr)

data = c(3.72,2.69,2.64,2.62,2.61,2.51,2.44,2.37,2.26,2.25,2.22,2.16,2.13,2.07,2.07,2.02,1.93,1.90,1.87,1.86)

print(mean(data, na.rm = TRUE))
print(median(data, na.rm = TRUE))
print(IQR(data, na.rm = TRUE))
print(sd(data, na.rm = TRUE))
hist(data)
boxplot(data)