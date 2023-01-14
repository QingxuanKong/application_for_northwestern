library(zoo)
library(xts)
library(tseries)
library(forecast)
library(stats)
library(DMwR)

x_train <- read.table('D:/ts_project/traffictrain.txt',header=FALSE, sep = ',')
colnames(x_train) <- c('date','s1','s2','s3','s4','s5','s6','s7')
rownames(x_train) <- x_train$date
x_test <- read.table('D:/ts_project/traffictest.txt',header=FALSE, sep = ',')
colnames(x_test) <- c('date','s1','s2','s3','s4','s5','s6','s7')
rownames(x_test) <- x_test$date

x_month <- read.table('D:/ts_project/trafficmonth.txt',header=FALSE, sep = ',')
colnames(x_month) <- c('date','s1','s2','s3','s4','s5','s6','s7')
rownames(x_month) <- x_month$date
s1_m <- xts(x_month$s1,order.by=as.POSIXct(rownames(x_month)))
attr(s1_m, 'frequency') <- 24
s1.m.decom <- decompose(as.ts(s1_m))
s1.m.i <- unclass(s1.m.decom$random)
s1.m.residual = s1.m.i[13:108]
s1.m.residual.ts = xts(s1.m.residual,order.by=as.POSIXct(rownames(x_month)[13:108]),frequency=24)
plot(s1.m.decom$trend)
plot(s1.m.decom$seasonal)
plot(s1.m.residual.ts)

x_dp <- read.table('D:/ts_project/trafficdp.txt',header=FALSE, sep = ',')
colnames(x_dp) <- c('date','s1','s2','s3','s4','s5','s6','s7')
rownames(x_dp) <- x_dp$date
s1_dp <- xts(x_dp$s1,order.by=as.POSIXct(rownames(x_dp)))
attr(s1_dp, 'frequency') <- 365
s1.dp.decom <- decompose(as.ts(s1_dp))
s1.dp.i <- unclass(s1.dp.decom$random)
s1.dp.residual = s1.dp.i[4:172]
s1.dp.residual.ts = xts(s1.dp.residual,order.by=as.POSIXct(rownames(x_dp)[4:172]),frequency=7)
plot(s1.dp.decom$trend)
plot(s1.dp.decom$seasonal)
plot(s1.dp.residual.ts)

par (mfrow=c(1,2),cex=0.4)

AIC.to.AICC <- function (aic, n, pq1) {
  aic - 2 * pq1 + 2 * n * pq1 / (n - 1 - pq1)
}

#s1_test <- xts(x_test$s1,order.by=as.POSIXct(rownames(x_test)))
s1_d <- xts(x_train$s1,order.by=as.POSIXct(rownames(x_train)))
attr(s1_d, 'frequency') <- 24
s1.decom <- decompose(as.ts(s1_d))
s1.i <- unclass(s1.decom$random)
s1.residual = s1.i[13:4188]
s1.residual.ts = xts(s1.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s1.decom$trend)
plot(s1.decom$seasonal)
plot(s1.residual.ts)
acf(s1.residual.ts)
pacf(s1.residual.ts)
qqnorm(s1.residual.ts)
qqline(s1.residual.ts)
s1.residual.ts.10 <- arima(s1.residual.ts,order=c(1,0,0))
s1.residual.ts.11 <- arima(s1.residual.ts,order=c(1,0,1))
s1.residual.ts.12 <- arima(s1.residual.ts,order=c(1,0,2))
s1.residual.ts.13 <- arima(s1.residual.ts,order=c(1,0,3))
s1.residual.ts.20 <- arima(s1.residual.ts,order=c(2,0,0))
s1.residual.ts.21 <- arima(s1.residual.ts,order=c(2,0,1))
s1.residual.ts.22 <- arima(s1.residual.ts,order=c(2,0,2))
s1.residual.ts.23 <- arima(s1.residual.ts,order=c(2,0,3))
s1.residual.ts.30 <- arima(s1.residual.ts,order=c(3,0,0))
s1.residual.ts.31 <- arima(s1.residual.ts,order=c(3,0,1))
s1.residual.ts.32 <- arima(s1.residual.ts,order=c(3,0,2))
s1.residual.ts.33 <- arima(s1.residual.ts,order=c(3,0,3))
s1.residual.ts.40 <- arima(s1.residual.ts,order=c(4,0,0))
s1.residual.ts.41 <- arima(s1.residual.ts,order=c(4,0,1))
s1.residual.ts.42 <- arima(s1.residual.ts,order=c(4,0,2))
s1.residual.ts.43 <- arima(s1.residual.ts,order=c(4,0,3))
s1.residual.ts.50 <- arima(s1.residual.ts,order=c(5,0,0))
s1.residual.ts.51 <- arima(s1.residual.ts,order=c(5,0,1))
s1.residual.ts.52 <- arima(s1.residual.ts,order=c(5,0,2))
s1.residual.ts.53 <- arima(s1.residual.ts,order=c(5,0,3))
# Computing AICC for the sequence of models
s1.aic.aicc <- matrix(c(s1.residual.ts.10$aic,AIC.to.AICC (s1.residual.ts.10$aic, 4296, 2),
                        s1.residual.ts.11$aic,AIC.to.AICC (s1.residual.ts.11$aic, 4296, 3),
                        s1.residual.ts.12$aic,AIC.to.AICC (s1.residual.ts.12$aic, 4296, 4),
                        s1.residual.ts.13$aic,AIC.to.AICC (s1.residual.ts.13$aic, 4296, 5),
                        s1.residual.ts.11$aic,AIC.to.AICC (s1.residual.ts.20$aic, 4296, 3),
                        s1.residual.ts.21$aic,AIC.to.AICC (s1.residual.ts.21$aic, 4296, 4),
                        s1.residual.ts.22$aic,AIC.to.AICC (s1.residual.ts.22$aic, 4296, 5),
                        s1.residual.ts.23$aic,AIC.to.AICC (s1.residual.ts.23$aic, 4296, 6),
                        s1.residual.ts.30$aic,AIC.to.AICC (s1.residual.ts.30$aic, 4296, 4),
                        s1.residual.ts.31$aic,AIC.to.AICC (s1.residual.ts.31$aic, 4296, 5),
                        s1.residual.ts.32$aic,AIC.to.AICC (s1.residual.ts.32$aic, 4296, 6),
                        s1.residual.ts.33$aic,AIC.to.AICC (s1.residual.ts.33$aic, 4296, 7),
                        s1.residual.ts.40$aic,AIC.to.AICC (s1.residual.ts.40$aic, 4296, 5),
                        s1.residual.ts.41$aic,AIC.to.AICC (s1.residual.ts.41$aic, 4296, 6),
                        s1.residual.ts.42$aic,AIC.to.AICC (s1.residual.ts.42$aic, 4296, 7),
                        s1.residual.ts.43$aic,AIC.to.AICC (s1.residual.ts.43$aic, 4296, 8),
                        s1.residual.ts.50$aic,AIC.to.AICC (s1.residual.ts.50$aic, 4296, 6),
                        s1.residual.ts.51$aic,AIC.to.AICC (s1.residual.ts.51$aic, 4296, 7),
                        s1.residual.ts.52$aic,AIC.to.AICC (s1.residual.ts.52$aic, 4296, 8),
                        s1.residual.ts.53$aic,AIC.to.AICC (s1.residual.ts.53$aic, 4296, 9))
                      ,ncol=2,byrow=TRUE)
colnames(s1.aic.aicc) <- c('aic','aicc')
rownames(s1.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43','arma50','arma51','arma52','arma53')
s1.aic.aicc <- s1.aic.aicc[order(s1.aic.aicc[,'aic']),]

rownames(s1.aic.aicc)[1]
s1.best <- s1.residual.ts.21
tsdisplay(residuals(s1.residual.ts.21))

s2_d <- xts(x_train$s2,order.by=as.POSIXct(rownames(x_train)))
attr(s2_d, 'frequency') <- 24
s2.decom <- decompose(as.ts(s2_d))
s2.i <- unclass(s2.decom$random)
s2.residual = s2.i[13:4188]
s2.residual.ts = xts(s2.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s2.decom$trend)
plot(s2.decom$seasonal)
plot(s2.residual.ts)
acf(s2.residual.ts)
pacf(s2.residual.ts)
qqnorm(s2.residual.ts)
qqline(s2.residual.ts)
s2.residual.ts.10 <- arima(s2.residual.ts,order=c(1,0,0))
s2.residual.ts.11 <- arima(s2.residual.ts,order=c(1,0,1))
s2.residual.ts.12 <- arima(s2.residual.ts,order=c(1,0,2))
s2.residual.ts.13 <- arima(s2.residual.ts,order=c(1,0,3))
s2.residual.ts.20 <- arima(s2.residual.ts,order=c(2,0,0))
s2.residual.ts.21 <- arima(s2.residual.ts,order=c(2,0,1))
s2.residual.ts.22 <- arima(s2.residual.ts,order=c(2,0,2))
s2.residual.ts.23 <- arima(s2.residual.ts,order=c(2,0,3))
s2.residual.ts.30 <- arima(s2.residual.ts,order=c(3,0,0))
s2.residual.ts.31 <- arima(s2.residual.ts,order=c(3,0,1))
s2.residual.ts.32 <- arima(s2.residual.ts,order=c(3,0,2))
s2.residual.ts.33 <- arima(s2.residual.ts,order=c(3,0,3))
s2.residual.ts.40 <- arima(s2.residual.ts,order=c(4,0,0))
s2.residual.ts.41 <- arima(s2.residual.ts,order=c(4,0,1))
s2.residual.ts.42 <- arima(s2.residual.ts,order=c(4,0,2))
s2.residual.ts.43 <- arima(s2.residual.ts,order=c(4,0,3))
# Computing AICC for the sequence of models
s2.aic.aicc <- matrix(c(s2.residual.ts.10$aic,AIC.to.AICC (s2.residual.ts.10$aic, 4296, 2),
                        s2.residual.ts.11$aic,AIC.to.AICC (s2.residual.ts.11$aic, 4296, 3),
                        s2.residual.ts.12$aic,AIC.to.AICC (s2.residual.ts.12$aic, 4296, 4),
                        s2.residual.ts.13$aic,AIC.to.AICC (s2.residual.ts.13$aic, 4296, 5),
                        s2.residual.ts.11$aic,AIC.to.AICC (s2.residual.ts.20$aic, 4296, 3),
                        s2.residual.ts.21$aic,AIC.to.AICC (s2.residual.ts.21$aic, 4296, 4),
                        s2.residual.ts.22$aic,AIC.to.AICC (s2.residual.ts.22$aic, 4296, 5),
                        s2.residual.ts.23$aic,AIC.to.AICC (s2.residual.ts.23$aic, 4296, 6),
                        s2.residual.ts.30$aic,AIC.to.AICC (s2.residual.ts.30$aic, 4296, 4),
                        s2.residual.ts.31$aic,AIC.to.AICC (s2.residual.ts.31$aic, 4296, 5),
                        s2.residual.ts.32$aic,AIC.to.AICC (s2.residual.ts.32$aic, 4296, 6),
                        s2.residual.ts.33$aic,AIC.to.AICC (s2.residual.ts.33$aic, 4296, 7),
                        s2.residual.ts.40$aic,AIC.to.AICC (s2.residual.ts.40$aic, 4296, 5),
                        s2.residual.ts.41$aic,AIC.to.AICC (s2.residual.ts.41$aic, 4296, 6),
                        s2.residual.ts.42$aic,AIC.to.AICC (s2.residual.ts.42$aic, 4296, 7),
                        s2.residual.ts.43$aic,AIC.to.AICC (s2.residual.ts.43$aic, 4296, 8))
                      ,ncol=2,byrow=TRUE)
colnames(s2.aic.aicc) <- c('aic','aicc')
rownames(s2.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43')
s2.aic.aicc <- s2.aic.aicc[order(s2.aic.aicc[,'aic']),]
rownames(s2.aic.aicc)[1]
s2.best <- s2.residual.ts.31
tsdisplay(residuals(s2.residual.ts.31))

s3_d <- xts(x_train$s3,order.by=as.POSIXct(rownames(x_train)))
attr(s3_d, 'frequency') <- 24
s3.decom <- decompose(as.ts(s3_d))
s3.i <- unclass(s3.decom$random)
s3.residual = s3.i[13:4188]
s3.residual.ts = xts(s3.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s3.decom$trend)
plot(s3.decom$seasonal)
plot(s3.residual.ts)
acf(s3.residual.ts)
pacf(s3.residual.ts)
qqnorm(s3.residual.ts)
qqline(s3.residual.ts)
s3.residual.ts.10 <- arima(s3.residual.ts,order=c(1,0,0))
s3.residual.ts.11 <- arima(s3.residual.ts,order=c(1,0,1))
s3.residual.ts.12 <- arima(s3.residual.ts,order=c(1,0,2))
s3.residual.ts.13 <- arima(s3.residual.ts,order=c(1,0,3))
s3.residual.ts.20 <- arima(s3.residual.ts,order=c(2,0,0))
s3.residual.ts.21 <- arima(s3.residual.ts,order=c(2,0,1))
s3.residual.ts.22 <- arima(s3.residual.ts,order=c(2,0,2))
s3.residual.ts.23 <- arima(s3.residual.ts,order=c(2,0,3))
s3.residual.ts.30 <- arima(s3.residual.ts,order=c(3,0,0))
s3.residual.ts.31 <- arima(s3.residual.ts,order=c(3,0,1))
s3.residual.ts.32 <- arima(s3.residual.ts,order=c(3,0,2))
s3.residual.ts.33 <- arima(s3.residual.ts,order=c(3,0,3))
s3.residual.ts.40 <- arima(s3.residual.ts,order=c(4,0,0))
s3.residual.ts.41 <- arima(s3.residual.ts,order=c(4,0,1))
s3.residual.ts.42 <- arima(s3.residual.ts,order=c(4,0,2))
s3.residual.ts.43 <- arima(s3.residual.ts,order=c(4,0,3))
# Computing AICC for the sequence of models
s3.aic.aicc <- matrix(c(s3.residual.ts.10$aic,AIC.to.AICC (s3.residual.ts.10$aic, 4296, 2),
                        s3.residual.ts.11$aic,AIC.to.AICC (s3.residual.ts.11$aic, 4296, 3),
                        s3.residual.ts.12$aic,AIC.to.AICC (s3.residual.ts.12$aic, 4296, 4),
                        s3.residual.ts.13$aic,AIC.to.AICC (s3.residual.ts.13$aic, 4296, 5),
                        s3.residual.ts.11$aic,AIC.to.AICC (s3.residual.ts.20$aic, 4296, 3),
                        s3.residual.ts.21$aic,AIC.to.AICC (s3.residual.ts.21$aic, 4296, 4),
                        s3.residual.ts.22$aic,AIC.to.AICC (s3.residual.ts.22$aic, 4296, 5),
                        s3.residual.ts.23$aic,AIC.to.AICC (s3.residual.ts.23$aic, 4296, 6),
                        s3.residual.ts.30$aic,AIC.to.AICC (s3.residual.ts.30$aic, 4296, 4),
                        s3.residual.ts.31$aic,AIC.to.AICC (s3.residual.ts.31$aic, 4296, 5),
                        s3.residual.ts.32$aic,AIC.to.AICC (s3.residual.ts.32$aic, 4296, 6),
                        s3.residual.ts.33$aic,AIC.to.AICC (s3.residual.ts.33$aic, 4296, 7),
                        s3.residual.ts.40$aic,AIC.to.AICC (s3.residual.ts.40$aic, 4296, 5),
                        s3.residual.ts.41$aic,AIC.to.AICC (s3.residual.ts.41$aic, 4296, 6),
                        s3.residual.ts.42$aic,AIC.to.AICC (s3.residual.ts.42$aic, 4296, 7),
                        s3.residual.ts.43$aic,AIC.to.AICC (s3.residual.ts.43$aic, 4296, 8))
                      ,ncol=2,byrow=TRUE)
colnames(s3.aic.aicc) <- c('aic','aicc')
rownames(s3.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43')
s3.aic.aicc <- s3.aic.aicc[order(s3.aic.aicc[,'aic']),]
rownames(s3.aic.aicc)[1]
s3.best <- s3.residual.ts.41
tsdisplay(residuals(s3.residual.ts.41))

s4_d <- xts(x_train$s4,order.by=as.POSIXct(rownames(x_train)))
attr(s4_d, 'frequency') <- 24
s4.decom <- decompose(as.ts(s4_d))
s4.i <- unclass(s4.decom$random)
s4.residual = s4.i[13:4188]
s4.residual.ts = xts(s4.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s4.decom$trend)
plot(s4.decom$seasonal)
plot(s4.residual.ts)
acf(s4.residual.ts)
pacf(s4.residual.ts)
qqnorm(s4.residual.ts)
qqline(s4.residual.ts)
s4.residual.ts.10 <- arima(s4.residual.ts,order=c(1,0,0))
s4.residual.ts.11 <- arima(s4.residual.ts,order=c(1,0,1))
s4.residual.ts.12 <- arima(s4.residual.ts,order=c(1,0,2))
s4.residual.ts.13 <- arima(s4.residual.ts,order=c(1,0,3))
s4.residual.ts.20 <- arima(s4.residual.ts,order=c(2,0,0))
s4.residual.ts.21 <- arima(s4.residual.ts,order=c(2,0,1))
s4.residual.ts.22 <- arima(s4.residual.ts,order=c(2,0,2))
s4.residual.ts.23 <- arima(s4.residual.ts,order=c(2,0,3))
s4.residual.ts.30 <- arima(s4.residual.ts,order=c(3,0,0))
s4.residual.ts.31 <- arima(s4.residual.ts,order=c(3,0,1))
s4.residual.ts.32 <- arima(s4.residual.ts,order=c(3,0,2))
s4.residual.ts.33 <- arima(s4.residual.ts,order=c(3,0,3))
s4.residual.ts.40 <- arima(s4.residual.ts,order=c(4,0,0))
s4.residual.ts.41 <- arima(s4.residual.ts,order=c(4,0,1))
s4.residual.ts.42 <- arima(s4.residual.ts,order=c(4,0,2))
s4.residual.ts.43 <- arima(s4.residual.ts,order=c(4,0,3))
# Computing AICC for the sequence of models
s4.aic.aicc <- matrix(c(s4.residual.ts.10$aic,AIC.to.AICC (s4.residual.ts.10$aic, 4296, 2),
                        s4.residual.ts.11$aic,AIC.to.AICC (s4.residual.ts.11$aic, 4296, 3),
                        s4.residual.ts.12$aic,AIC.to.AICC (s4.residual.ts.12$aic, 4296, 4),
                        s4.residual.ts.13$aic,AIC.to.AICC (s4.residual.ts.13$aic, 4296, 5),
                        s4.residual.ts.11$aic,AIC.to.AICC (s4.residual.ts.20$aic, 4296, 3),
                        s4.residual.ts.21$aic,AIC.to.AICC (s4.residual.ts.21$aic, 4296, 4),
                        s4.residual.ts.22$aic,AIC.to.AICC (s4.residual.ts.22$aic, 4296, 5),
                        s4.residual.ts.23$aic,AIC.to.AICC (s4.residual.ts.23$aic, 4296, 6),
                        s4.residual.ts.30$aic,AIC.to.AICC (s4.residual.ts.30$aic, 4296, 4),
                        s4.residual.ts.31$aic,AIC.to.AICC (s4.residual.ts.31$aic, 4296, 5),
                        s4.residual.ts.32$aic,AIC.to.AICC (s4.residual.ts.32$aic, 4296, 6),
                        s4.residual.ts.33$aic,AIC.to.AICC (s4.residual.ts.33$aic, 4296, 7),
                        s4.residual.ts.40$aic,AIC.to.AICC (s4.residual.ts.40$aic, 4296, 5),
                        s4.residual.ts.41$aic,AIC.to.AICC (s4.residual.ts.41$aic, 4296, 6),
                        s4.residual.ts.42$aic,AIC.to.AICC (s4.residual.ts.42$aic, 4296, 7),
                        s4.residual.ts.43$aic,AIC.to.AICC (s4.residual.ts.43$aic, 4296, 8))
                      ,ncol=2,byrow=TRUE)
colnames(s4.aic.aicc) <- c('aic','aicc')
rownames(s4.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43')
s4.aic.aicc <- s4.aic.aicc[order(s4.aic.aicc[,'aic']),]
rownames(s4.aic.aicc)[1]
s4.best <- s4.residual.ts.42
tsdisplay(residuals(s4.residual.ts.42))

s5_d <- xts(x_train$s5,order.by=as.POSIXct(rownames(x_train)))
attr(s5_d, 'frequency') <- 24
s5.decom <- decompose(as.ts(s5_d))
s5.i <- unclass(s5.decom$random)
s5.residual = s5.i[13:4188]
s5.residual.ts = xts(s5.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s5.decom$trend)
plot(s5.decom$seasonal)
plot(s5.residual.ts)
acf(s5.residual.ts)
pacf(s5.residual.ts)
qqnorm(s5.residual.ts)
qqline(s5.residual.ts)
s5.residual.ts.10 <- arima(s5.residual.ts,order=c(1,0,0))
s5.residual.ts.11 <- arima(s5.residual.ts,order=c(1,0,1))
s5.residual.ts.12 <- arima(s5.residual.ts,order=c(1,0,2))
s5.residual.ts.13 <- arima(s5.residual.ts,order=c(1,0,3))
s5.residual.ts.20 <- arima(s5.residual.ts,order=c(2,0,0))
s5.residual.ts.21 <- arima(s5.residual.ts,order=c(2,0,1))
s5.residual.ts.22 <- arima(s5.residual.ts,order=c(2,0,2))
s5.residual.ts.23 <- arima(s5.residual.ts,order=c(2,0,3))
s5.residual.ts.30 <- arima(s5.residual.ts,order=c(3,0,0))
s5.residual.ts.31 <- arima(s5.residual.ts,order=c(3,0,1))
s5.residual.ts.32 <- arima(s5.residual.ts,order=c(3,0,2))
s5.residual.ts.33 <- arima(s5.residual.ts,order=c(3,0,3))
s5.residual.ts.40 <- arima(s5.residual.ts,order=c(4,0,0))
s5.residual.ts.41 <- arima(s5.residual.ts,order=c(4,0,1))
s5.residual.ts.42 <- arima(s5.residual.ts,order=c(4,0,2))
s5.residual.ts.43 <- arima(s5.residual.ts,order=c(4,0,3))
# Computing AICC for the sequence of models
s5.aic.aicc <- matrix(c(s5.residual.ts.10$aic,AIC.to.AICC (s5.residual.ts.10$aic, 4296, 2),
                        s5.residual.ts.11$aic,AIC.to.AICC (s5.residual.ts.11$aic, 4296, 3),
                        s5.residual.ts.12$aic,AIC.to.AICC (s5.residual.ts.12$aic, 4296, 4),
                        s5.residual.ts.13$aic,AIC.to.AICC (s5.residual.ts.13$aic, 4296, 5),
                        s5.residual.ts.11$aic,AIC.to.AICC (s5.residual.ts.20$aic, 4296, 3),
                        s5.residual.ts.21$aic,AIC.to.AICC (s5.residual.ts.21$aic, 4296, 4),
                        s5.residual.ts.22$aic,AIC.to.AICC (s5.residual.ts.22$aic, 4296, 5),
                        s5.residual.ts.23$aic,AIC.to.AICC (s5.residual.ts.23$aic, 4296, 6),
                        s5.residual.ts.30$aic,AIC.to.AICC (s5.residual.ts.30$aic, 4296, 4),
                        s5.residual.ts.31$aic,AIC.to.AICC (s5.residual.ts.31$aic, 4296, 5),
                        s5.residual.ts.32$aic,AIC.to.AICC (s5.residual.ts.32$aic, 4296, 6),
                        s5.residual.ts.33$aic,AIC.to.AICC (s5.residual.ts.33$aic, 4296, 7),
                        s5.residual.ts.40$aic,AIC.to.AICC (s5.residual.ts.40$aic, 4296, 5),
                        s5.residual.ts.41$aic,AIC.to.AICC (s5.residual.ts.41$aic, 4296, 6),
                        s5.residual.ts.42$aic,AIC.to.AICC (s5.residual.ts.42$aic, 4296, 7),
                        s5.residual.ts.43$aic,AIC.to.AICC (s5.residual.ts.43$aic, 4296, 8))
                      ,ncol=2,byrow=TRUE)
colnames(s5.aic.aicc) <- c('aic','aicc')
rownames(s5.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43')
s5.aic.aicc <- s5.aic.aicc[order(s5.aic.aicc[,'aic']),]
rownames(s5.aic.aicc)[1]
s5.best <- s5.residual.ts.32
tsdisplay(residuals(s5.residual.ts.32))

s6_d <- xts(x_train$s6,order.by=as.POSIXct(rownames(x_train)))
attr(s6_d, 'frequency') <- 24
s6.decom <- decompose(as.ts(s6_d))
s6.i <- unclass(s6.decom$random)
s6.residual = s6.i[13:4188]
s6.residual.ts = xts(s6.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s6.decom$trend)
plot(s6.decom$seasonal)
plot(s6.residual.ts)
acf(s6.residual.ts)
pacf(s6.residual.ts)
qqnorm(s6.residual.ts)
qqline(s6.residual.ts)
s6.residual.ts.10 <- arima(s6.residual.ts,order=c(1,0,0))
s6.residual.ts.11 <- arima(s6.residual.ts,order=c(1,0,1))
s6.residual.ts.12 <- arima(s6.residual.ts,order=c(1,0,2))
s6.residual.ts.13 <- arima(s6.residual.ts,order=c(1,0,3))
s6.residual.ts.20 <- arima(s6.residual.ts,order=c(2,0,0))
s6.residual.ts.21 <- arima(s6.residual.ts,order=c(2,0,1))
s6.residual.ts.22 <- arima(s6.residual.ts,order=c(2,0,2))
s6.residual.ts.23 <- arima(s6.residual.ts,order=c(2,0,3))
s6.residual.ts.30 <- arima(s6.residual.ts,order=c(3,0,0))
s6.residual.ts.31 <- arima(s6.residual.ts,order=c(3,0,1))
s6.residual.ts.32 <- arima(s6.residual.ts,order=c(3,0,2))
s6.residual.ts.33 <- arima(s6.residual.ts,order=c(3,0,3))
s6.residual.ts.40 <- arima(s6.residual.ts,order=c(4,0,0))
s6.residual.ts.41 <- arima(s6.residual.ts,order=c(4,0,1))
s6.residual.ts.42 <- arima(s6.residual.ts,order=c(4,0,2))
s6.residual.ts.43 <- arima(s6.residual.ts,order=c(4,0,3))
# Computing AICC for the sequence of models
s6.aic.aicc <- matrix(c(s6.residual.ts.10$aic,AIC.to.AICC (s6.residual.ts.10$aic, 4296, 2),
                        s6.residual.ts.11$aic,AIC.to.AICC (s6.residual.ts.11$aic, 4296, 3),
                        s6.residual.ts.12$aic,AIC.to.AICC (s6.residual.ts.12$aic, 4296, 4),
                        s6.residual.ts.13$aic,AIC.to.AICC (s6.residual.ts.13$aic, 4296, 5),
                        s6.residual.ts.11$aic,AIC.to.AICC (s6.residual.ts.20$aic, 4296, 3),
                        s6.residual.ts.21$aic,AIC.to.AICC (s6.residual.ts.21$aic, 4296, 4),
                        s6.residual.ts.22$aic,AIC.to.AICC (s6.residual.ts.22$aic, 4296, 5),
                        s6.residual.ts.23$aic,AIC.to.AICC (s6.residual.ts.23$aic, 4296, 6),
                        s6.residual.ts.30$aic,AIC.to.AICC (s6.residual.ts.30$aic, 4296, 4),
                        s6.residual.ts.31$aic,AIC.to.AICC (s6.residual.ts.31$aic, 4296, 5),
                        s6.residual.ts.32$aic,AIC.to.AICC (s6.residual.ts.32$aic, 4296, 6),
                        s6.residual.ts.33$aic,AIC.to.AICC (s6.residual.ts.33$aic, 4296, 7),
                        s6.residual.ts.40$aic,AIC.to.AICC (s6.residual.ts.40$aic, 4296, 5),
                        s6.residual.ts.41$aic,AIC.to.AICC (s6.residual.ts.41$aic, 4296, 6),
                        s6.residual.ts.42$aic,AIC.to.AICC (s6.residual.ts.42$aic, 4296, 7),
                        s6.residual.ts.43$aic,AIC.to.AICC (s6.residual.ts.43$aic, 4296, 8))
                      ,ncol=2,byrow=TRUE)
colnames(s6.aic.aicc) <- c('aic','aicc')
rownames(s6.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43')
s6.aic.aicc <- s6.aic.aicc[order(s6.aic.aicc[,'aic']),]
rownames(s6.aic.aicc)[1]
s6.best <- s6.residual.ts.33
tsdisplay(residuals(s6.residual.ts.33))

s7_d <- xts(x_train$s7,order.by=as.POSIXct(rownames(x_train)))
attr(s7_d, 'frequency') <- 24
s7.decom <- decompose(as.ts(s7_d))
s7.i <- unclass(s7.decom$random)
s7.residual = s7.i[13:4188]
s7.residual.ts = xts(s7.residual,order.by=as.POSIXct(rownames(x_train)[13:4188]),frequency=24)
plot(s7.decom$trend)
plot(s7.decom$seasonal)
plot(s7.residual.ts)
acf(s7.residual.ts)
pacf(s7.residual.ts)
qqnorm(s7.residual.ts)
qqline(s7.residual.ts)
s7.residual.ts.10 <- arima(s7.residual.ts,order=c(1,0,0))
s7.residual.ts.11 <- arima(s7.residual.ts,order=c(1,0,1))
s7.residual.ts.12 <- arima(s7.residual.ts,order=c(1,0,2))
s7.residual.ts.13 <- arima(s7.residual.ts,order=c(1,0,3))
s7.residual.ts.20 <- arima(s7.residual.ts,order=c(2,0,0))
s7.residual.ts.21 <- arima(s7.residual.ts,order=c(2,0,1))
s7.residual.ts.22 <- arima(s7.residual.ts,order=c(2,0,2))
s7.residual.ts.23 <- arima(s7.residual.ts,order=c(2,0,3))
s7.residual.ts.30 <- arima(s7.residual.ts,order=c(3,0,0))
s7.residual.ts.31 <- arima(s7.residual.ts,order=c(3,0,1))
s7.residual.ts.32 <- arima(s7.residual.ts,order=c(3,0,2))
s7.residual.ts.33 <- arima(s7.residual.ts,order=c(3,0,3))
s7.residual.ts.40 <- arima(s7.residual.ts,order=c(4,0,0))
s7.residual.ts.41 <- arima(s7.residual.ts,order=c(4,0,1))
s7.residual.ts.42 <- arima(s7.residual.ts,order=c(4,0,2))
s7.residual.ts.43 <- arima(s7.residual.ts,order=c(4,0,3))
# Computing AICC for the sequence of models
s7.aic.aicc <- matrix(c(s7.residual.ts.10$aic,AIC.to.AICC (s7.residual.ts.10$aic, 4296, 2),
                        s7.residual.ts.11$aic,AIC.to.AICC (s7.residual.ts.11$aic, 4296, 3),
                        s7.residual.ts.12$aic,AIC.to.AICC (s7.residual.ts.12$aic, 4296, 4),
                        s7.residual.ts.13$aic,AIC.to.AICC (s7.residual.ts.13$aic, 4296, 5),
                        s7.residual.ts.11$aic,AIC.to.AICC (s7.residual.ts.20$aic, 4296, 3),
                        s7.residual.ts.21$aic,AIC.to.AICC (s7.residual.ts.21$aic, 4296, 4),
                        s7.residual.ts.22$aic,AIC.to.AICC (s7.residual.ts.22$aic, 4296, 5),
                        s7.residual.ts.23$aic,AIC.to.AICC (s7.residual.ts.23$aic, 4296, 6),
                        s7.residual.ts.30$aic,AIC.to.AICC (s7.residual.ts.30$aic, 4296, 4),
                        s7.residual.ts.31$aic,AIC.to.AICC (s7.residual.ts.31$aic, 4296, 5),
                        s7.residual.ts.32$aic,AIC.to.AICC (s7.residual.ts.32$aic, 4296, 6),
                        s7.residual.ts.33$aic,AIC.to.AICC (s7.residual.ts.33$aic, 4296, 7),
                        s7.residual.ts.40$aic,AIC.to.AICC (s7.residual.ts.40$aic, 4296, 5),
                        s7.residual.ts.41$aic,AIC.to.AICC (s7.residual.ts.41$aic, 4296, 6),
                        s7.residual.ts.42$aic,AIC.to.AICC (s7.residual.ts.42$aic, 4296, 7),
                        s7.residual.ts.43$aic,AIC.to.AICC (s7.residual.ts.43$aic, 4296, 8))
                      ,ncol=2,byrow=TRUE)
colnames(s7.aic.aicc) <- c('aic','aicc')
rownames(s7.aic.aicc) <- c('arma10','arma11','arma12','arma13','arma20','arma21','arma22','arma23','arma30','arma31','arma32','arma33','arma40','arma41','arma42','arma43')
s7.aic.aicc <- s7.aic.aicc[order(s7.aic.aicc[,'aic']),]
rownames(s7.aic.aicc)[1]
s7.best <- s7.residual.ts.32
tsdisplay(residuals(s7.residual.ts.32))

s1.f <- Arima(s1.residual.ts,order=c(2,0,1))
plot(as.numeric(s1.f$x),type='l',col="red")
lines(as.numeric(fitted(s1.f)),type='l',col="blue")

accmeasures1=regr.eval(as.numeric(s1.f$x), as.numeric(fitted(s1.f)))

predict(s1.best,n.head=5)

s.residual=merge(s1.residual.ts,s2.residual.ts,s3.residual.ts,s4.residual.ts,s5.residual.ts,s6.residual.ts,s7.residual.ts,all=TRUE)
s.residual.acf = acf(s.residual,lag.max=10)
s.residual.acf$acf
s.residual.acf$lag




s1.residual.forecast <- predict(s1.best,1)
s1.trend.forecast <- predict(s1.decom$trend[13:4188],1)
s1.seasonal.forecast <- predict(s1.decom$seasonal[13:4188],1)
predicted <- as.numeric(s1.trend.forecast$mean)+as.numeric(s1.seasonal.forecast$mean)+as.numeric(s1.residual.forecast$pred)
x_test$s1[1:12]

s2.residual.forecast <- forecast(s2.best,120)
s2.trend.forecast <- forecast(s2.decom$trend)
s2.seasonal.forecast <- forecast(s2.decom$seasonal)

s3.residual.forecast <- forecast(s3.best)
s3.trend.forecast <- forecast(s3.decom$trend)
s3.seasonal.forecast <- forecast(s3.decom$seasonal)

s4.residual.forecast <- forecast(s4.best)
s4.trend.forecast <- forecast(s4.decom$trend)
s4.seasonal.forecast <- forecast(s4.decom$seasonal)

s5.residual.forecast <- forecast(s5.best)
s5.trend.forecast <- forecast(s5.decom$trend)
s5.seasonal.forecast <- forecast(s5.decom$seasonal)

s6.residual.forecast <- forecast(s6.best)
s6.trend.forecast <- forecast(s6.decom$trend)
s6.seasonal.forecast <- forecast(s6.decom$seasonal)

s7.residual.forecast <- forecast(s7.best)
s7.trend.forecast <- forecast(s7.decom$trend)
s7.seasonal.forecast <- forecast(s7.decom$seasonal)