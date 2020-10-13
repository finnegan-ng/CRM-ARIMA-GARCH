#setwd("D:/Fynn-data/Hunter/Project-Spring-2020")

#install.packages("quantmod")
#install.packages("tidyquant")
#install.packages("tidyverse")
#install.packages("forecast")
#install.packages("fpp2")
#install.packages("tseries")
#install.packages("TSA")
#install.packages('tsbox')
#install.packages('dynlm')
#install.packages('FinTS')
#install.packages('fGarch')
#install.packages('TSPred')
#install.packages("rugarch")


library(quantmod)
library(tidyquant)
library(tidyverse)
library(forecast)
library(fpp2)
library(tseries)
library(TSA)
library(tsbox)
library(dynlm)
library(FinTS)
library(fGarch)
library(TSPred)
library(rugarch)



#4 years, daily closing price

#quartz(width=10,height=6)

#Salesforce.com
crm_prices<-tq_get("CRM",get = "stock.prices", from="2016-01-01", to="2019-12-31") #4 years, daily
crm_prices

#chart 
win.graph(10,6)
crm_prices %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = "blue3", size = 0.6) +
  theme_classic() +
  labs(x = ' ',
       y = " ") +
  scale_y_continuous(breaks = seq(0,300,10))

#chart with time index

plot(crm_prices$adjusted, type='l', col='blue3', lwd=2)
legend(x= 'topleft', legend='CRM', lty = 1, lwd = 2, col='midnightblue')

#dataframe
crm<-data.frame("Date" = crm_prices$date, "Closed" = crm_prices$adjusted)
#nrow(crm)
#nrow(na.omit(crm))

#ts

crm_ts = ts(data = crm$Closed,frequency = 365, start = c(2016,1))


#ACF
win.graph(10,6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_ts))

ggAcf(crm_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#pacf
win.graph(10,6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_ts))

ggPacf(crm_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")
#adf test
adf.test(crm_ts, alternative = "stationary")

#p-val > 0.05, not stationary 

#first difference 

crm_dif_closed <- diff(crm$Closed, differences = 1)
length(crm_dif_closed)

crm_date <- crm$Date[-1]

crm_dif <- data.frame("Date" = crm_date, "difClosed" = crm_dif_closed)

#plot diff 
win.graph(15,10)
crm_dif %>%
  ggplot(aes(x = Date, y = difClosed)) +
  geom_line(color = "midnightblue", size = 0.6) +
  theme_classic() +
  labs(x = 'Date',
       y = "Difference Closing Share Price",
       title = "Salesforce difference price chart") +
  scale_y_continuous(breaks = seq(0,300,10))


#ts

crm_dif_ts = ts(data = crm_dif$difClosed,frequency = 365, start = c(2016,1))

#ACF
win.graph(15,10)
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_dif_ts))

ggAcf(crm_dif_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#pacf
win.graph(15,10)
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_dif_ts))

ggPacf(crm_dif_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")


#adf test
adf.test(crm_dif_ts, alternative = "stationary")

#################################################################
####daily return (ln of diff)(in R, natural log function is log)

crm_return <- crm_prices %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'crm_ret')


crm_return#plot daily return
win.graph(10,6)
crm_return %>%
  ggplot(aes(x = date, y = crm_ret)) +
  geom_line(color = "black", size = 0.6) +
  theme_classic() +
  labs(x = " ", y = " ") +
  ggtitle(" ") +
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  scale_y_continuous(breaks = seq(-0.5, 0.6, 0.05)) 
  # +geom_hline(yintercept=0, color ='red', size = 0.7)

#daily return histogram
win.graph(15,10)
crm_return %>%
  ggplot(aes(x = crm_ret)) +
  geom_histogram(binwidth = 0.015) +
  theme_classic() +
  labs(x = "Daily returns") +
  ggtitle("Daily Returns for Salesforce") +
  scale_x_continuous(breaks = seq(-0.5,0.6,0.05)) +
  annotate(geom = 'text', x = -0.30, y= 200, label = "Extremely\nnegative\nreturns") +
  annotate(geom = 'segment', x = -0.305, xend = -0.35,  y = 120, yend = 20, color = 'red', arrow = arrow()) +
  annotate(geom = 'segment', x = 0.405, xend = 0.42,  y = 120, 
           yend = 20, color = 'blue', arrow = arrow(type = "open")) +
  annotate(geom = 'text', x = 0.430, y = 200, label = "Extremely\npositive\nreturns")

#daily return ts
inds <- seq(as.Date("2016-01-04"), as.Date("2019-12-30"), by = "day")
inds

crm_return_ts = ts(data = crm_return$crm_ret,frequency = 365, start = c(2016,as.numeric(format(inds[1], "%j"))))
length(crm_return_ts)

#####split into testing and training
#crm_return_testing_ts <- crm_return_ts[986:1005]
#length(crm_return_testing_ts)
#crm_return_testing_ts

#crm_return_training_ts <- crm_return_ts[1:985]
#length(crm_return_training_ts)
#crm_return_training_ts

N=length(crm_return_ts)
n=20

N-n+1

crm_return_training_ts<-ts(data = crm_return_ts[1:(N-n)], frequency = 365, start = c(2016,as.numeric(format(inds[1], "%j"))))
crm_return_testing_ts<-ts(data = crm_return_ts[(N-n+1):N], frequency = 365, start = c(2019,as.numeric(format(inds[2], "%j"))))

length(crm_return_training_ts)
class(crm_return_training_ts)
#ACF
win.graph(10,6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_return_training_ts))

ggAcf(crm_return_training_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#pacf
win.graph(10,6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_return_training_ts))

ggPacf(crm_return_training_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#adf test
adf.test(crm_return_training_ts, alternative = "stationary")

#eacf

eacf(crm_return_training_ts)


eacf(crm_dif_ts)
#####################################################################

#monthly return (bar chart)
crm_monthly_return <- crm_prices %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'crm_ret')

crm_monthly_return %>%
  ggplot(aes(x = date, y = crm_ret)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Date", y = "Monthly returns") +
  ggtitle("Monthly Returns for Netflix") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-0.6,0.8,0.1),
                     labels = scales::percent) +
  scale_x_date(date_breaks = "years", date_labels = "%Y")



###########################################
####zlag
#plot lag 1
win.graph(10,10)
plot(crm_return_training_ts, x=zlag(crm_return_training_ts), ylab = expression(Y[t], xlab = expression(Y[t-1]), type = 'p'))
#plot lag 2
win.graph(10,10)
plot(crm_return_training_ts, x=zlag(crm_return_ts,2), ylab = expression(Y[t], xlab = expression(Y[t-2]),type = 'p'))

#lag 1 diff
plot(crm_dif_ts, x=zlag(crm_dif_ts), ylab = expression(Y[t], xlab = expression(Y[t-1]), type = 'p'))


##########################################################################
####Square
#Square diff plot
win.graph(15,10)
crm_dif %>%
  ggplot(aes(x = Date, y = difClosed^2)) +
  geom_line(color = "midnightblue", size = 0.6) +
  theme_classic() +
  labs(x = 'Date',
       y = "Difference Closing Daily Price Square",
       title = "Salesforce difference price chart") +
  scale_y_continuous(breaks = seq(0,300,10))

#Square return plot
win.graph(10,6)
crm_return %>%
  ggplot(aes(x = date, y = crm_ret^2)) +
  geom_line(color = "blue3", size = 0.6) +
  theme_classic() +
  labs(x = " ", y = " ") +
  scale_x_date(date_breaks = 'years', date_labels = '%Y') 
  
#Square return ts

crm_return_sq_ts = ts(data = crm_return$crm_ret^2,frequency = 365, start = c(2016,1))

eacf(crm_return_sq_ts)
#ACF
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_return_sq_ts))

ggAcf(crm_return_sq_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#pacf
ci2 = qnorm((1 + .95)/2)/sqrt(length(crm_return_ts))

ggPacf(crm_return_ts, lag.max = 20) + ggtitle(" ") + xlab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#########################################
####McLeod-Li Test Statistics for Daily crm Returns
win.graph(15,10)
McLeod.Li.test(y=crm_return_ts)

#QQ Normal Plot of Daily CRM Return

qqnorm(crm_return_ts); qqline(crm_return_ts)

#strong evidence for ARCH

####ARCH
# Step 1: Estimate mean equation r = beta + error
crm.mean <-dynlm(crm_ret ~ 1, data = crm_return)

# Step 2: Retrieve the residuals from the former model and square them
ehatsq <- ts(resid(crm.mean)^2)

# Step 3: regress squared residuals on one-lagged squared residuals
crm.arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)
summary(crm.arch)

#ARCH effects test
crm.archTest <- ArchTest(crm_return$crm_ret, lags = 1, demean = TRUE)
crm.archTest #p-value < 0.05, we reject the null hypothesis and conclude the presence of ARCH(1) effects

####Estimating ARCH Models
arch.fit10 <- garchFit(~garch(1,0), data = crm_return$crm_ret, trace = F)
arch.fit11 <- garchFit(~garch(1,1), data = crm_return$crm_ret, trace = F)
arch.fit20 <- garchFit(~garch(2,0), data = crm_return$crm_ret, trace = F)
arch.fit21 <- garchFit(~garch(2,1), data = crm_return$crm_ret, trace = F)#Best
arch.fit12 <- garchFit(~garch(1,2), data = crm_return$crm_ret, trace = F)
arch.fit22 <- garchFit(~garch(2,2), data = crm_return$crm_ret, trace = F)

summary(arch.fit10);summary(arch.fit11);summary(arch.fit20);
summary(arch.fit21);summary(arch.fit12);summary(arch.fit22)



#Plotting the conditional variance
crm_return$ht <- arch.fit21@h.t
ggplot(crm_return, aes(y = ht, x = date)) +
  geom_line(col='#ff9933')+
  ylab('Conditional Variance') +
  xlab('Date')

#ACF of standardized residuals and  squared standardized residuals
stand.res <- arch.fit21@residuals/arch.fit21@sigma.t
acf(stand.res)
acf(stand.res^2)
#######################################################
###rugarch
#get data from yahoo
crm.prices=get.hist.quote(
  instrument = "^GSPC",
  quote = "Adj",
  provider = c("yahoo"), method = NULL,
  origin = "2016-01-01", compression = "d",
  retclass = c("zoo"), quiet = FALSE, drop = FALSE
)
crm1=as.data.frame(crm.prices)
N=length(crm1[,1])
crm.ret=100*(log(crm1[2:N,])-log(crm1[1:(N-1),]))



#Fit a model using ruGarch
time <- 1:length(crm_return_training_ts)

res_garch21_spec <- ugarchspec(variance.model = list(garchOrder = c(2,1)), 
                               mean.model = list(armaOrder = c(2,4)))

res_garch21_fit <- ugarchfit(spec = res_garch21_spec, data = crm_return_training_ts ) 
res_garch21_fit

win.graph(10,6)
plot(res_garch21_fit, col = "blue3") 

crm.garch.for <- ugarchforecast(res_garch21_fit, n.ahead = 20)
win.graph(10,6)
plot(crm.garch.for)
 
fitted(crm.garch.for)

residuals(res_garch21_fit)

arch.y = garch(crm_return_training_ts, order = c(2,1))
plot(arch.y)

model_one = garchFit(crm_ret~garch(2,1), data=crm_return_training_ts, trace=F)
summary(model_one)



#######################################################
####ARIMA
#auto.arima
arimaModel_auto <- auto.arima(crm_return_training_ts)

#arima models
arimaModel_1=Arima(crm_return_training_ts,order = c(0,0,2))
arimaModel_2=Arima(crm_return_training_ts,order = c(1,0,2))
arimaModel_3=Arima(crm_return_training_ts,order = c(0,0,3))
arimaModel_4=Arima(crm_return_training_ts,order = c(2,0,2))
arimaModel_5=Arima(crm_return_training_ts,order = c(1,0,3)) 
arimaModel_6=Arima(crm_return_training_ts,order = c(2,0,3)) #second
arimaModel_7=Arima(crm_return_training_ts,order = c(3,0,3))
arimaModel_8=Arima(crm_return_training_ts,order = c(1,0,4))
arimaModel_9=Arima(crm_return_training_ts,order = c(2,0,4)) #Best
arimaModel_10=Arima(crm_return_training_ts,order = c(3,0,4))

print(arimaModel_1);print(arimaModel_2);print(arimaModel_3);print(arimaModel_4);print(arimaModel_5);
print(arimaModel_6);print(arimaModel_7);print(arimaModel_8);print(arimaModel_9);print(arimaModel_auto)

#check residuals
checkresiduals(arimaModel_9)

#Arima Forecasting
#ARIMA forecasting

crm_fcast<-forecast(arimaModel_9, h = 20)
win.graph(10,6)
plot(crm_fcast)

#fit original data and fitted values
plot(crm_fcast$x, col = "red")
#lines(fitted(arimaModel_9), col = 'blue')
lines(fitted(crm_fcast), col ='blue')

#arima and original
plotarimapred(crm_return_testing_ts, arimaModel_9, xlim = c(2016,2019), range.percent = 0.2, xreg = NULL,
              ylab = NULL, xlab = NULL, main = NULL)
#not working

#############################################################
####block bootstrap


tsboot <- tsbootstrap(crm_return_ts, nb = 10, m = 1, b=100.5,
            type="block", coef = NULL, include.intercept = TRUE, 
            series = NULL)
head(tsboot)

win.graph(18,15)
plot(tsboot)


#############################################################
####Convert return to price
#df <- data.frame(price = c(161.00,161.57,156.43,158.22,158.01,157.48,156.40,156.39,158.59,161.13,161.96,161.63,161.48,163.33,164.55,163.74,163.25,164.51,164.98,162.44))

#df$log = log(df$price)
#df$logr = c(NA, diff(df$log))
#df$logr_na0 = ifelse(is.na(df$logr), 0, df$logr)
#df$cuml_log= cumsum(df$logr_na0)
#df$reconstructed_price_norm = exp(df$cuml_log)

p = c(161.57,156.43,158.22,158.01,157.48,156.40,156.39,158.59,161.13,161.96,161.63,161.48,163.33,164.55,163.74,163.25,164.51,164.98,162.44)


fp = c()
class(crm_fcast$mean)
crm_fcast$mean

for (i in 1:20){
  fp[i+1]= p[i] * exp(crm_fcast$mean[i+1])
  fp[1]= 161.00 * exp(crm_fcast$mean[1])
}

fp

####standard error
#convert lower value of 95% 
crm_fcast$residuals

fl = c()

for (i in 1:20){
  fl[i+1]= p[i] * exp(crm_fcast$lower[i+1])
  fl[1]= 161.00 * exp(crm_fcast$lower[1])
}

fl

#SE base on lower
se =c()
for (i in 1:20){
  se[i] = (fp[i] - fl[i])/1.96
}

se

#se base on predict function log dif
pred <-predict(arimaModel_9, n.ahead = 20, se.fit =TRUE)

pred$pred
pred$se

#compare with loop log dif

fls = c()

for (i in 1:20){
  fls[i+1]= crm_fcast$mean[i] * exp(crm_fcast$lower[i+1])
  fl[1]= 161.00 * exp(crm_fcast$lower[1])
}

fl

############################
# 80% CI
#lower 80
crm_fcast$lower

fl80 = c()

for (i in 1:19){
  fl80[i+1]= p[i] * exp(crm_fcast$lower[i+1])
  fl80[1]= 161.00 * exp(crm_fcast$lower[1])
}

fl80

#upper 80
crm_fcast$upper[1]

fu80 = c()

for (i in 1:19){
  fu80[i+1]= p[i] * exp(crm_fcast$upper[i+1])
  fu80[1]= 161.00 * exp(crm_fcast$upper[1])
}

fu80




#################################
#GARCH
#list of pred log dif
crm.garch.for@forecast[["seriesFor"]]

#convert to pred price
fg=c()

for (i in 1:19){
  fg[i+1]= p[i] * exp(crm.garch.for@forecast[["seriesFor"]][i+1])
  fg[1]= 161.00 * exp(crm.garch.for@forecast[["seriesFor"]][1])
}

fg
##

model2_predict <- predict(arch.fit21, n.ahead = 20, mse="uncond")
print(model2_predict)

# sigma 80%

sig = 1.28*crm.garch.for@forecast[["sigmaFor"]]
sig

#lower 
sl80 = crm.garch.for@forecast[["seriesFor"]] - sig
sl80

sl=c()

for (i in 1:19){
  sl[i+1]= p[i] * exp(sl80[i+1])
  sl[1]= 161.00 * exp(sl80[1])
}

sl

#upper
su80 = crm.garch.for@forecast[["seriesFor"]] + sig
su80

su=c()

for (i in 1:19){
  su[i+1]= p[i] * exp(su80[i+1])
  su[1]= 161.00 * exp(su80[1])
}

su

################################################################
#Diagnostics
#ARIMA
#residual
require(graphics)
nobs(arimaModel_9)
win.graph(10,6)
tsdiag(arimaModel_9, gof=24, omit.initial = F)

win.graph(10,6)
plot(rstandard(arimaModel_9), col = "blue3");abline(h=0, col="red")

win.graph(10,4)
acf(rstandard(arimaModel_9), lag = 24)

qqnorm(residuals(arimaModel_9)); qqline(residuals(arimaModel_9))
win.graph(10,6)
ggqqplot(arimaModel_9$residuals)
shapiro.test(arimaModel_9$residuals)
##################
#plot Forecast Errors
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
win.graph(10,10)
plotForecastErrors(arimaModel_9$residuals)




########GARCH
#residual
require(graphics)
nobs(res_garch21_fit)
win.graph(10,6)
tsdiag(res_garch21_fit, gof=24, omit.initial = F)

win.graph(10,6)
plot(residuals(res_garch21_fit, standardize = T), col = "blue3");abline(h=0, col="red")

win.graph(10,6)
acf(residuals(res_garch21_fit), lag = 24)

qqnorm(residuals(res_garch21_fit)); qqline(residuals(res_garch21_fit))
win.graph(10,6)

ggqqplot(arimaModel_9$residuals)
shapiro.test(arimaModel_9$residuals)
shapiro.test(res_garch21_fit@fit[["residuals"]])


acf(residuals(res_garch21_fit), lag = 24)

#histogram
win.graph(10,10)
plotForecastErrors(res_garch21_fit@fit[["residuals"]])

#############accuracy

accuracy(crm_fcast)
acc

