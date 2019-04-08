library(fUnitRoots)
library(tseries)


# time series

data(AirPassengers)
air= AirPassengers

# airdata=read.csv( 'C:\\Users\\levey\\Desktop\\timeseries\\AirPassengers.csv')


# EDA
class(air)

start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)

summary(AirPassengers)


# Detailed Metrics

plot(AirPassengers)

abline(reg=lm(AirPassengers~time(AirPassengers)))

#This will print the cycle across years.
cycle(AirPassengers)


#This will aggregate the cycles and display a year on year trend
plot(aggregate(AirPassengers,FUN= mean)) #sd  median

#Box plot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))


# Important Inferences ----
# The year on year trend clearly shows that the #passengers have been increasing without fail.
# The variance and the mean value in July and August is much higher than rest of the months.
# Even though the mean value of each month is quite different their variance is small. 
# Hence, we have strong seasonal effect with a cycle of 12 months or less.

# adf test

adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

adfTest(AirPassengers);
adfTest(log(AirPassengers));
adfTest(diff(AirPassengers));


# plot 
acf(log(AirPassengers))

acf(diff(log(AirPassengers)))

pacf(diff(log(AirPassengers)))


# modelling

fit <- arima(log(AirPassengers), c(0, 1, 1),
              seasonal = list(order = c(0, 1, 1), period = 12))

pred <- predict(fit, n.ahead = 10*12)
pred1 <- predict(fit, n.ahead = 15*12)

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))




fit2 <- arima(log(AirPassengers), c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1), period = 10))
pred2 <- predict(fit, n.ahead = 10*12)






