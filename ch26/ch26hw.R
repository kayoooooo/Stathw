library(questionr)

#12 test
x = c(21900, 4058,3898,1333,359,218,4281,806,415,89,12,10)
dim(x) = c(6,2)
chisq.test(x)
chisq.residuals(x)

#20 test
x = c(142,274,152,396,51,131,244,173,416,51)
dim(x) = c(5,2)
chisq.test(x)

#25 test
x = c(87,83,8,34)
dim(x) = c(2,2)
chisq.test(x)
