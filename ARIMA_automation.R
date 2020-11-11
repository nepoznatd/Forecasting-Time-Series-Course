library(forecast)
library(SciViews)

#Run each block individually (denoted by [i])

#[1] run the following line to choose your CSV file
df <- read.csv(file.choose(),sep=",",header=TRUE)




"""[2] the following lines extract data from your relevant columns and perform any relevant transformations (you decide). Make sure that the indexed column is correct (i.e df[[x]] below. Replace 'x' with the xth column that contains the time series data]"""
d0_Levels <- df[[2]]
d1_Levels <-diff(d0_Levels)
d0_ln_Levels <- ln(df[[2]])
d1_ln_Levels <- diff(d0_ln_Levels)




#[3] Use the following graphs to determine whether we need to transform and difference the time series. (Is there mean reversion?).

# Plot Levels and differences
plot.ts(d0_Levels,main="Time Series Plot",xlab="Index")
plot.ts(d1_Levels,main="Time Series Plot",xlab="Index")
plot.ts(d0_ln_Levels,main="Time Series Plot",xlab="Index")
plot.ts(d1_ln_Levels,main="Time Series Plot",xlab="Index")

#Plot ACF and PACF of Log(Levels) with x differences
forecast::Acf(d0_ln_Levels,lag.max=70,main="TITLE")
forecast::Pacf(d0_ln_Levels,lag.max=70,main="TITLE")

forecast::Acf(d1_ln_Levels,lag.max=70,main="TITLE")
forecast::Pacf(d1_ln_Levels,lag.max=70,main="TITLE")


"""[5] Replace the interger '0' in the variable below with the optimal number of differences that 
you determined from step [3]"""
d = 0




#[4] Calculate log likelihood, AIC, and AICc for possible combinations of p,q

#From the following output choose 'p' and 'q' that minimises AIC or AICc
#remember to change 
counter = 1
for(p in 0:2) {
  print(counter)
  arimar <- Arima(d0_ln_Levels,order=c(p,d,0),include.mean=FALSE,include.drift=FALSE)
  print(arimar)
  writeLines("")
  writeLines("")
  counter = counter + 1
  for(q in 1:2) {
    print(counter)
    arimar <- Arima(d0_ln_Levels,order=c(p,d,q),include.mean=FALSE,include.drift=FALSE)
    print(arimar)
    writeLines("")
    writeLines("")
    counter = counter + 1
  }
}


#[5] Replace the interger '0' in the variables below with the 'p' and 'q' that minimised AIC or AICc from step [4]
p = 0
q = 0

#[6] Replace the interger '50' in the variable below with the lead time you'd like to forecast
lead_time = 50


#[7] Display ARIMA coefficients and Plot Forecast
arima_model <-arima('VARIABLE', order=c(p,d,q))
print(arim_model)
#print p-values
(1-pnorm(abs(arima_model$coef)/sqrt(diag(arima_model$var.coef))))*2
plot(forecast(arima_model,h=lead_time),main="TITLE")


#[8] Plot Residuals
checkresiduals(d1_ln_Levels)
resd<-residuals(arima_model)
plot(resd,main="Residuals vs Order", xlab="Observation Order", ylab="Residual",type="p")
Acf(resd,lag.max=70,main="ACF of Residuals")
Pacf(resd,lag.max=70,main="PACF of Residuals")
