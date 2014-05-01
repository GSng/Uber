library('forecast')
library('tseries')
hourly_demand <- read.csv("~/Dropbox/GS/uber/ipy/hourly_demand.csv")
View(hourly_demand)
demand <- hourly_demand[,1]
demand
demand <- hourly_demand[,2]
demand
start_index = 25#pass this in from python
month_index = 0#
demand <- train_data_hourly
train_data_hourly <-demand
train_data_ts <- ts(train_data_hourly,start=c(1,(168-start_index+2)),frequency=168)+0.01
plotforecasterrors
demand_d1 <- diff(demand,difference=1)
demand_d2 <- diff(demand,difference=2)
demand_d3 <- diff(demand,difference=3)
pacf(demand)
pacf(demand, lag,max=24)
pacf(demand)
acf
acf(demand)
kpss.test(demand,null="Level")
kpss.test(demand,null="trend")
kpss.test(demand,null="Trend")
demand
kpss.test(demand_d1,null="Level")
kpss.test(demand_d2,null="Level")
kpss.test(demand_d,null="Level")
kpss.test(demand_d3,null="Level")
kpss.test(demand_d3,null="Trend")
kpss.test(demand_d2,null="Trend")
kpss.test(demand_d1,null="Trend")
kpss.test(demand,null="Level")
kpss.test(demand_d2,null="Level")
kpss.test(demand_d3,null="Level")
kpss.test(demand_d2,null="Level")
kpss.test(demand,null="Trend")
pp.test(demand)
adf.test(demand)
adf.test(demand_d1)
adf.test(demand_d2)
adf.test(demand_d3)
kpss.test(demand,c="Level")
kpss.test(demand,null="Trend")
kpss.test(demand,null="Level")
kpss.test(demand_d1,c="Level")
kpss.test(demand_d1,null="Level")
pacf(demand)
demand
plot(demand)
pacf(demand)
traffic = demand
pacf(demand)
pacf(traffic)
pacf(diff(traffic,2)
)
acf(traffic)
help plot
plot
help(plot)
plot(traffic,"l")
plot(traffic,type="l")
plot(hourly_demand[:,1],hourly_demand[:,2]")
plot(hourly_demand[:,1],hourly_demand[:,2])
hourly_demand[:,1]
hourly_demand(:,1)
hourly_demand(,1)
hourly_demand[,1]
plot(hourly_demand[,1],hourly_demand[,2])
demand
plot(train_data_ts,type="l")
train_data_daily <- unname(tapply(train_data_hourly, (seq_along(train_data_hourly)-1) %/% 24, sum))
train_data_mo_ts <- ts(train_data_daily,start=c(1,month_index),frequency=28)
plot(train_data_mo_ts,type="l")
index_end = start_index+168*7-1
train_data_hourly <- train_data_hourly[1:index_end]
train_data_daily <- unname(tapply(train_data_hourly, (seq_along(train_data_hourly)-1) %/% 24, sum))
#
# train hourly model over week #
train_data_ts <- ts(train_data_hourly,start=c(1,(168-start_index+2)),frequency=168)+0.01
hr_model <- HoltWinters(train_data_ts,beta=0,seasonal="m",start.periods=(168+start_index-1))
d_forecast = 14#pass this in from python
# forecast hourly model
hr_forecast <- forecast.HoltWinters(hr_model,h=(d_forecast*24))
plot.forecast(hr_forecast)
d_forecast = 14#pass this in from python
# forecast hourly model
hr_forecast <- forecast.HoltWinters(hr_model,h=(7*24))
plot.forecast(hr_forecast)
d_forecast = 14#pass this in from python
# forecast hourly model
hr_forecast <- forecast.HoltWinters(hr_model,h=(d_forecast*24))
plot.forecast(hr_forecast)
plot(hr_model)
fix(hr_forecast)
fix(hr_forecast)
accuracy(hr_model)
hr_model$sse
hr_forecast$residuals
acf(hr_forecast$residuals,max.lag=20)
pacf(hr_forecast$residuals,max.lag=20)
help(pacf)
pacf(hr_forecast$residuals,lag.max=20)
Box.test(hr_forecast$residuals,lag=20,type="Ljung-Box")
train_data_hourly <- traffic
train_data_ts <- ts(train_data_hourly,start=c(1,(168-start_index+2)),frequency=168)+0.01
hr_model <- HoltWinters(train_data_ts,beta=0,seasonal="m",start.periods=(168+start_index-1))
Box.test(hr_forecast$residuals,lag=20,type="Ljung-Box")
plot(hr_model)
accuracy(hr_model)
fix(hr_model)
hr_model$mean
hr_model$fitted
x <- hr$fitted
x <- hr_model$fitted
x
hr_model$x
x = x[,1]
1296+168
x
x_real <- hr_model$x
x_real = x_real[169:1464]
resid <- x-x_real
Box.test(resid,lag=20,type="Ljung-Box")
pacf(resid)
pacf(hr_model$residuls)
pacf(hr_model$residuals)
pacf(hr_model$residuals,max.lag=20)
hr_model <- HoltWinters(train_data_ts,beta=0,seasonal="m",start.periods=(168-start_index+1))
168-25+1
plot(hr_model)
accuracy(hr_model)
pacf(hr_model$residual)
pacf(hr_model$residuals)
pacf(hr_model$residuals,lag.max=20)
hr_model$residuals
hr_model$residual
hr_model$residuals
144+25
168-25
+1
144+24
plot(traffic,type="l")
plot(hr_model)
hr_model%residuals
x_hat <- hr_model$fitted
x_hat = x_hat[,1];
x_hat
resid <- x_hat-x-0.01
resid
resid <- x_hat-x_real-0.01
resid
pacf(resid)
residuals <- diff(resid)
pacf(residuals)
help(diff)
pacf(diff(resid, differences=1))
pacf(diff(resid, differences=2))
pacf(diff(resid, differences=3))
pacf(diff(resid, differences=10))
pacf(diff(resid, differences=100))
pacf(diff(resid))
pacf(resid)
help(pacf)
pacf(resid,plot='false')
max(0.01,resid-0.5)
help(max)
pacf(resid-0.5)
pacf(resid-5)
pacf(resid*0.5)
pacf(sqrt(abs(resid))
)
pacf(ln(abs(resid))
)
pacf(log(abs(resid))
)
pacf(resid)
pacf_results = pacf(resid,plot='False')
pacf_results
acf(pacf_results)
acf
plot.acf(pacf_results)
plotforecasterrors(resid)
plotForecastErrors <- function(forecasterrors)
{
# make a histogram of the forecast errors:
mybinsize <- IQR(forecasterrors)/4
mysd   <- sd(forecasterrors)
mymin  <- min(forecasterrors) - mysd*5
mymax  <- max(forecasterrors) + mysd*3
# generate normally distributed data with mean 0 and standard deviation mysd
mynorm <- rnorm(10000, mean=0, sd=mysd)
mymin2 <- min(mynorm)
mymax2 <- max(mynorm)
if (mymin2 < mymin) { mymin <- mymin2 }
if (mymax2 > mymax) { mymax <- mymax2 }
# make a red histogram of the forecast errors, with the normally distributed data overlaid:
mybins <- seq(mymin, mymax, mybinsize)
hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
# freq=FALSE ensures the area under the histogram = 1
# generate normally distributed data with mean 0 and standard deviation mysd
myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
# plot the normal curve as a blue line on top of the histogram of forecast errors:
points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotforecasterrors(resid)
plotForecastErrors(resid)
plotForecastErrors(resid*0.5)
plotForecastErrors(resid*0.6)
sd(resid)
sd(resid*0.5)
sd(resid*0.6)
Box.test(resid,lag=20,type="Ljung-Box")
fix(hr_forecast)
start_index
1464/168
168*7
index_end
1464-1200
264-168
168-start_index+2
train_data_ts <- ts(train_data_hourly[1:index_end],start=c(1,(168-start_index+2)),frequency=168)+0.01
hr_model <- HoltWinters(train_data_ts,beta=0,seasonal="m",start.periods=(168+start_index-1))
hr_forecast <- forecast.HoltWinters(hr_model,h=264)
plot(hr_forecast)
y_hat = hr_forecast$mean
y_hat
traffic(1401:1464)
traffic[1401:1464]
traffic[1201:1464]
y = traffic[1201:1464]
y = y+0.01
resid_outsple = y_hat-y
plot(y,type="l")
lines(y_hat,col="green")
y_hat
y_ts <- ts(y,start=c(1,9),frequency=168)
y_ts <- ts(y,start=c(1,9),frequency=168)
plot(y_ts,type="l")
lines(y_hat,col="green")
plot(y_hat,col="green")
y_ts
y_hat
plot(y_ts)
par(new=TRUE)
plot(y_hat,col="red")
plot(y_ts)
par(new=TRUE)
lines(y_hat,col="red")
lines(y_hat,col="red")
lines(y,col="red"
)
unclass(y_hat)
z_hat = unclass(y_hat)
lines(z_hat)
plot(y)
plot(y,type="l")
lines(z_hat,col="green")
plot(y,type="l")
lines(z_hat,col="red")
plot(y,type="l",main="Out of Sample Prediction", xlab="hours",ylab="traffic")
lines(z_hat,col="red")
[1,2]/[1,2]
resid_outsple/y
abs(resid_outsple/y)
sum(abs(resid_outsple/y))
sum(abs(resid_outsple/y))/264
sqrt(sum((resid_outsple/y)^2)/264)
sqrt(sum((resid/x_real)^2)/1296)
163.876
/2
163.876/2
167.4193/2
plotForecastErrors(resid_outsple)
sd(resid_outsple)
pacf(resid_outsple,lag.max=20)
hr_forecast
