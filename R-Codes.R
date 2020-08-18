#### Install Packages####
install.packages("tsDyn")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("astsa")
install.packages("dplyr")
install.packages("urca")
install.packages("vars")
install.packages("nnet")


####Making the data machine readable####
data<-Data_for_R_Nadya
m<-matrix(data,nrow=56,ncol=64)
df<-data.frame(data)
df<-df[-c(1:2)]
plot (df$Res_New, type="l",xlab="Time", ylab="Res_New", main="Res_New Volume", 
      col="Blue")
####Declaring the data as time Series####
df<-ts(df,start=c(2005,1), end=c(2018,4),frequency=4)
plot (df[ ,"Res_New"], type="l",xlab="Time", ylab="Res_New", main="Residential New Volume", 
      col="Blue")
####ACF & PACF for Res_New-Shows the MA and AR terms respectively####
acf(df[ ,"Res_New"], lag.max=30, plot=TRUE)
pacf(df[ ,"Res_New"], lag.max=30, plot=TRUE)
#Checking Stationary Condition
adf.test(df[ ,"Res_New"])
kpss.test(df[ ,"Res_New"])
####Making the External regressors machine readable by coverting it to a vector####
xreg<-c(df[ ,"Non_Res_New"],df[ ,"Non_Res_Imp"])
xreg<-matrix(xreg, nrow=56, ncol=2)
xreg<-ts(xreg,start=c(2005,1), end=c(2018,4),frequency=4)
####Auto-ARIMA First Try####
mymodel1=auto.arima (df[ ,"Res_New"], xreg=xreg)
mymodel1
mymodel1fore<-forecast(mymodel1,xreg=xreg)
mymodel1fore
plot(forecast(mymodel1fore), xlim=c(2005,2025))
accuracy(mymodel1fore)
####Manual ARIMA Test####
manuarima=arima(df[ ,"Res_New"],xreg=xreg, order=c(1,0,0),
          seasonal = list(order = c(0, 0, 0), period = 4))
manuarima
manuarimafore<-forecast(manuarima)
manuarimafore
plot(forecast(manuarimafore), xlim=c(2005,2025))
####Seasonal ARIMA Test####
install.packages("astsa")
library(astsa)
seasarima=sarima.for(df[ ,"Res_New"],20,1,0,0,2,1,0,4)
seasarima
####Manual ARIMA w/ External Regressors-No Good####
exarima<-arima(df[ ,"Res_New"], order = c(1, 0, 0),
              seasonal = c(2, 1, 0),xreg = xreg)
exarima
exarima.m<-matrix(exarima)
exarima.ts<-ts(exarima.m)
exarima.fore<-forecast(exarima.ts)
plot(forecast(exarima), xlim=c(2005,2025))
####Auto ARIMA w/ Extenal regressors-No Good####
arimax=auto.arima(df[ ,"Res_New"], d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
                  max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                  start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                  seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
                  nmodels = 94, trace = FALSE, method = NULL, truncate = NULL, xreg = xreg,
                  test = c("kpss", "adf", "pp"), test.args = list(),
                  seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                  seasonal.test.args = list(), allowdrift = TRUE, allowmean = TRUE,
                  lambda = NULL, biasadj = FALSE, parallel = FALSE, num.cores = 2)
arimax
plot(forecast(arimax),xlim=c(2005,2025))
####Auot.Arima W/ Ex Reg, Part 2-Success####

decom.Res_New<-decompose(df[ ,"Res_New"], type="multiplicative")
plot(decom.Res_New)

newtest<-auto.arima(df[ ,"Res_New"],xreg=xreg, trace=TRUE, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
                    max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                    start.q = 2, start.P = 1, start.Q = 1)
newtest
newtestfore<-forecast(newtest,xreg=xreg)
newtestfore
plot(forecast(newtestfore,xreg=xreg),xlim=c(2005,2025),ylim=c(0,2500),xlab="Time",
     ylab="Volume", main="Forecast for Residential New, ARIMA(0,1,0)")

####Arima Training & Testing-W/ external regressors####
training <- subset(df[ ,"Res_New"],end=length(df[ ,"Res_New"])-12)
test <- subset(df[ ,"Res_New"],start=length(df[ ,"Res_New"])-11)
xreg.train <-subset(xreg[1:44,])
xreg.train<-ts(xreg,start=c(2005,1), end=c(2015,4),frequency=4)
Res_New.train <- Arima(training,xreg=xreg.train, order=c(1,0,0),seasonal=c(0,0,0),method="CSS", lambda=0)
Res_New.train.forecast<- forecast(Res_New.train, xreg=xreg, h=12)
Res_New.train.forecast

Res_New.train.forecast %>%
  forecast %>%
  autoplot(xlim=c(2005,2019)) + autolayer(test)

##########
autoplot(training, series="Training data") +
  autolayer(fitted(Res_New.train, h=1),series="Fitted values")

####Neural Network-w/ external regressor-Good####
NN_Res_New=nnetar(df[ ,"Res_New"], repeats = 20, xreg = xreg, lambda = NULL,
                  model = NULL, subset = NULL, scale.inputs = TRUE)
NN_Res_New
NNfcast<-forecast(NN_Res_New, h = ifelse(NN_Res_New$m > 1, 2 * NN_Res_New$m,
                                         10), PI = FALSE, level = c(50,80, 95), fan = FALSE, xreg = xreg,
                  lambda = NN_Res_New$lambda, bootstrap = FALSE, npaths = 1000,
                  innov = NULL)
NNfcast
plot(NNfcast)
accuracy(NNfcast)
####VECM####
coint.series<-c(df[ ,"Non_Res_New"],df[ ,"Res_New"])
coint.series<-matrix(xreg, nrow=56, ncol=2)
coint.series<-ts(coint.series,start=c(2005,1), end=c(2018,4),frequency=4)
library (tsDyn)
cointest <- ca.jo(coint.series,K=Lagl,type = "eigen", ecdet = "const", 
                  spec = "transitory")
vecm_Res_New <- cajorls(cointest)
var <- vec2var(cointest)
vecm_Res_New_2<-VECM(coint.series,lag=2)
predict(vecm_Res_New_2, n.ahead=20)
predict_rolling(vecm_Res_New_2, nroll = 20, n.ahead = 20)
####VECM-2nd Try-No Good####
VECM(coint.series,lag, r = 1, include = c("const", "trend", "none", "both"),
     estim = c("2OLS", "ML"), LRinclude = c("none", "const",
                                            "trend", "both"), exogen = "df[ ,Others]")

---------------------------------------------------------------------------------------------------------------------------------------
  #####Testing-Manual ARIMA-New-Success######
fit <- Arima(df[,"Res_New"], xreg=xreg, order=c(2,1,0),
             seasonal = list(order = c(1,0,0), period = 4))
fcast <- forecast(fit, xreg=xreg)
fcast
autoplot(fcast) + xlab("Year") +
  ylab("Residential New")+ xlim(c(2005,2025)) + ylim(c(0,2500))

-----------------------------------------------------------------------------------
  #####Testing-Auto ARIMA-New-Success######
fit1 <- auto.arima(df[, "Res_New"], xreg = xreg)
fit1
fcast1<-forecast(fit1, xreg=xreg)
fcast1
autoplot(fcast1) + xlab("Year") +
  ylab("Residential New")+ xlim(c(2005,2025)) + ylim(c(0,2500))

------------------------------------------------------------------------------------
  ####Arima Training & Testing-W/ external regressors####
training <- subset(df[ ,"Res_New"],end=length(df[ ,"Res_New"])-12)
test <- subset(df[ ,"Res_New"],start=length(df[ ,"Res_New"])-11)
xreg.train <-subset(xreg[1:44,])
xreg.train<-ts(xreg,start=c(2005,1), end=c(2015,4),frequency=4)
Res_New.train <- Arima(training,xreg=xreg.train, order=c(2,1,0),seasonal=c(1,0,0),method="CSS", lambda=0)
Res_New.train.forecast<- forecast(Res_New.train, xreg=xreg, h=12)
Res_New.train.forecast

Res_New.train.forecast %>%
  forecast %>%
  autoplot(xlim=c(2005,2019)) + autolayer(test) +ylim (c(0,2500)) 