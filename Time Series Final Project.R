## (a) stock
  # (1) load data
  setwd("D:\\Program Files\\R") # set working directory
  stock = read.delim("元大金.txt")
  stock = stock[,2]
  T = length(stock)
  stock1 = ts(stock)
  
  
  # (2) Plots
  par(mfrow = c(3,1))
  plot(1:T, stock, type='l', xlab='')
  abline(a = mean(stock1), b = 0, col = 2) # a: intercept, b: slope, col: color
  acf(stock)
  pacf(stock)
  # Ljung-Box test: Q(10)
  Box.test(stock, lag=10, type='Ljung')
  
  
## (b) log price
  # (1) transfer to log price
  lgp = log(stock)
  lgp1 = ts(lgp)

  # (2) Plots
  par(mfrow = c(3,1))
  plot(1:T, lgp, type='l', xlab = '')
  abline(a = mean(lgp1),b = 0,col=2)
  acf(lgp)
  pacf(lgp)
  
  # (3) adfTest: p-value = 0.9443 => differencing
  library(fUnitRoots)
  adfTest(lgp,type = 'nc') # correct input
  adfTest(lgp,type = 'c') # wrong input
  
  
## (c) log return
  # (1) transfer to log return
  lrt = NULL
  for (t in 1:T-1){
    lrt[t] = (lgp[t+1]-lgp[t]) * 100
  }
  T1 = length(lrt)
  lrt1 = ts(lrt)
  
  
  # (2) Plots
  par(mfrow = c(3,1))
  plot(1:T1, lrt, type='l', xlab = '')
  abline(a = mean(lrt1),b = 0,col=2)
  acf(lrt,lag.max=log(T1))
  pacf(lrt)
  # Ljung-Box test: Q(10)
  Box.test(lrt, lag=10, type='Ljung')
  
  
  # (3) Model Fitting 
  m1 = ar(lrt, method="mle")
  m1$aic
  m1$order  
  
  aic = NULL
  for (i in 0:5){
    for (j in 0:5){
      m2 = arima(lrt[1:(T1-50)], order = c(i,0,j))
      if (aic > m2$aic || (i==0 && j == 0)){
        aic = m2$aic
        min_i = i; min_j = j;
      }
    }
  }
  
  ## -------- AR(4)------------
  ar = arima(lrt,order=c(4,0,0),fixed=c(NA,NA,0,NA,0))
  ar
  
  sqrt(ar$sigma2)
  
  Box.test(ar$residuals, lag=log(length(ar$residuals)), type='Ljung')
  tsdiag(ar, gof=floor(log(T1))+1) 
  
  p1    = c(1,-ar$coef[1:4])
  roots = polyroot(p1) 
  roots          
  modulus = Mod(roots)
  modulus
  
  k1 = 2*pi/acos(Re(roots[1])/modulus[1])
  k1
  k2 = 2*pi/acos(Re(roots[2])/modulus[2])
  k2
  
  ## -------- MA(2)------------
  ma = arima(lrt,order=c(0,0,2),fixed=c(NA,NA,0))
  ma
  
  sqrt(ma$sigma2)
  
  Box.test(ma$residuals, lag=log(length(ma$residuals)), type='Ljung')
  tsdiag(ma, gof=floor(log(T1))+1) 
  
  ## -------- ARMA(2,4)------------
  ARMA = arima(lrt, order=c(2,0,4),fixed =c(NA,NA,NA,NA,0,NA,0))
  ARMA
  
  sqrt(ARMA$sigma2) 
  
  # Ljung-Box test: Q(10)
  Box.test(ARMA$residuals, lag=10, type='Ljung')
  tsdiag(ARMA, gof=floor(log(T1))+1)  
  
  p1    = c(1,-ARMA$coef[1:2])
  roots = polyroot(p1) 
  roots          
  modulus = Mod(roots)
  modulus
  
  k = 2*pi/acos(Re(roots[1])/modulus[1])
  k
  
  # check whether GARCH effect exists: yes!
  acf(ARMA$residuals)
  acf((ARMA$residuals)^2)
  pacf(ARMA$residuals^2)
  
  # ARMA(2,4) + GARCH(1,0) AIC: 3.125410, exists
  library(fGarch)
  m1 = garchFit(~arma(2,4) + garch(1,0), data = lrt, trace=F, cond.dist = 'sstd')
  summary(m1)
  
  plot(m1)
  
  # ARMA(2,4) + GARCH(1,1) AIC: 3.103514, not exist
  m2 = garchFit(~arma(0,1) + garch(1,1), data = lrt, trace = F, cond.dist = 'sstd')
  summary(m2)
  
  plot(m2)
  
  # ARMA(2,4) + GARCH(1,2) AIC: 3.104699, not exist
  m3 = garchFit(~arma(2,4) + garch(1,2), data = lrt, trace = F, cond.dist = 'sstd')
  summary(m3)
  
  plot(m3)
  
  # ARMA(2,4) + GARCH(2,0) AIC: 3.112129, exists
  m4 = garchFit(~arma(2,4) + garch(2,0), data = lrt, trace = F, cond.dist = 'sstd')
  summary(m4)
  
  plot(m4)
  
  # ARMA(2,4) + GARCH(2,0) AIC: 3.111051, not exist
  m5 = garchFit(~arma(2,4) + garch(2,1), data = lrt,  cond.dist = 'sstd')
  summary(m5)
  
  plot(m5)
  
  # ARMA(2,4) + GARCH(2,0) AIC: 3.106298, not exist
  m6 = garchFit(~arma(2,4) + garch(2,2), data = lrt, trace = F,  cond.dist = 'sstd')
  summary(m6)
  
  plot(m6)
  
  ## Conclusion: ARMA(2,4) + GARCH(1,1) has the lowest AIC.
  
  
##(d) Prediction
  # (1)  ARMA 的20 步預測
  new = read.delim("2885_daily.txt")[,1]
  T2 = length(new)
  daily = NULL
  for (t in 1:T2){
    if (t==1){
      daily[1] = log(new[1]/stock[T])
    }
    else{
      daily[t] = log(new[t]/new[t-1]) 
    }
  }
  
  pred = predict(ARMA, 20)
  ll   = min(c(lrt[(T1-39):T1],pred$pred-2*pred$se))
  uu   = max(c(lrt[(T1-39):T1],pred$pred+2*pred$se))
  
  par(mfrow=c(1,1))
  plot(c(lrt[(T1-39):T1], pred$pred), type = 'l', ylim = c(ll,uu))
  lines(41:60,pred$pred+2*pred$se, lty = 2, ylim = c(ll,uu), col=2) 
  lines(41:60,pred$pred-2*pred$se, lty = 2, ylim = c(ll,uu), col=2)
  par(new=T1)
  plot(41:60, pred$pred, type = 'p', col=2, ylim = c(ll,uu), xlim=c(1,60),
       xlab='',ylab='')
  abline(a=mean(lrt1),b = 0,col=5)
  
  # (2) ARMA 的1 步預測 
  # sigma=1
  # set.seed(1)
  # at=rnorm(T1,0,sigma)
  m3 =  arima(lrt[1:(T1-50)],order=c(2,0,4),fixed =c(NA,NA,NA,NA,0,NA,NA))
  phi_1 = m3$coef[1];phi_2 = m3$coef[2];theta_1=m3$coef[3];theta_2=m3$coef[4];theta_3=m3$coef[5];theta_4=m3$coef[6];phi_0=(1-sum(m3$coef[1:2]))*m3$coef[7];
  at = vector(length=T1)
  at[1:(T1-50)]=m3$residuals
  r_hat_1 = NULL
  r_hat_1 = lrt
  for (t in 5:(T1-50)){
    r_hat_1[t] = phi_0 + phi_1*lrt[t-1] + phi_2*lrt[t-2] + theta_1*at[t-1]+ theta_2*at[t-2]+theta_3*at[t-3]+theta_4*at[t-4]  
    
  }
  for (t in (T1-49):T1){
    r_hat_1[t] = phi_0 + phi_1*lrt[t-1] + phi_2*lrt[t-2] + theta_1*at[t-1]+ theta_2*at[t-2]+ theta_3*at[t-3]+ theta_4*at[t-4]
    # at[t] = (lrt[t] - (phi_0 + phi_1*lrt[t-1] + phi_2*lrt[t-2]- theta_1*at[t-1]- theta_2*at[t-2]- theta_3*at[t-3]- theta_4*at[t-4]))/m3$sigma2
    at[t] = lrt[t] - r_hat_1[t]
  }
  # one-step ahead predition  
  # par(mfrow=c(3,2))
  plot((T1-249):T1, lrt[(T1-249):(T1)], type='l',xlab='',ylab = '',lwd=2, main='100*return')
  lines((T1-249):(T1-50), r_hat_1[(T1-249):(T1-50)], lwd=2, col=4)
  lines((T1-50):T1, r_hat_1[(T1-50):T1], lwd=2, col=2)
  abline(v=T1-50, lty=2, col=3,lwd=2)
  
  # Convert to price    
  Pt=NULL
  Pt_hat = NULL
  Pt[1]=stock[1]
  Pt_hat[1] = Pt[1]
  for (t in 2:T){
    Pt[t] = Pt[t-1]*(1+lrt[t]/100)
    Pt_hat[t] = Pt[t-1]*(1+r_hat_1[t]/100)
  }
  plot((T-249):T, Pt[(T-249):T], type='p',xlab='',ylab = '',lwd=2, main = 'Price')
  lines((T-249):(T-50), Pt_hat[(T-249):(T-50)], lwd=2, col=4)
  lines((T-50):T, Pt_hat[(T-50):T], lwd=2, lty = 2, col=2)
  abline(v=T1-50, lty=2, col=3,lwd=2)
  
  
  
  # (3) ARMA(2,4) + GARCH(1,1) 的10步預測
  predict(m2, n.ahead=10, plot = T, nx = 40) 
  
  # (4) ARMA(2,4) + GARCH(1,1) 的1步預測
  set.seed(1)
  epsilon=rnorm(T1,0,1)
  pred1 = garchFit(~arma(2,4)+garch(1,1), data = lrt[1:(T1-50)], trace = F, cond.dist = 'sstd')
  coef = coef(pred1)
  r_hat_2 = NULL;r_hat_2 = lrt
  sigma_2 = vector(length = T1);sigma_2[1:(T1-50)]=pred1@h.t 
  at=vector(length=T1);at[1:(T1-50)]=pred1@residuals
  for (t in 5:(T1-50)){
    r_hat_2[t] = coef[1] + coef[2]*lrt[t-1] + coef[3]*lrt[t-2] + coef[4]*at[t-1] + coef[5]*at[t-2] + coef[6]*at[t-3] + coef[7]*at[t-4];
  }
  for (t in (T1-49):T1){
    r_hat_2[t] = coef[1] + coef[2]*lrt[t-1] + coef[3]*lrt[t-2] + coef[4]*at[t-1] + coef[5]*at[t-2] + coef[6]*at[t-3] + coef[7]*at[t-4];
    sigma_2[t] = coef[8]+coef[9]*at[t-1]^2+coef[10]*sigma_2[t-1]
    at[t] = lrt[t] - r_hat_2[t]
  }
  # one-step ahead predition  
  plot((T1-249):T1, lrt[(T1-249):(T1)], type='l',xlab='',ylab = '',lwd=2, main='100*return')
  lines((T1-249):(T1-50), r_hat_2[(T1-249):(T1-50)], lwd=2, col=4)
  lines((T1-50):T1, r_hat_2[(T1-50):T1], lwd=2, col=2)
  abline(v=T1-50, lty=2, col=3,lwd=2)
  
  # Convert to price    
  Pt=NULL
  Pt_hat = NULL
  Pt[1]=stock[1]
  Pt_hat[1] = Pt[1]
  for (t in 2:T){
    Pt[t] = Pt[t-1]*(1+lrt[t]/100)
    Pt_hat[t] = Pt[t-1]*(1+r_hat_2[t]/100)
  }
  plot((T-249):T, Pt[(T-249):T], type='p',xlab='',ylab = '',lwd=2, main = 'Price')
  lines((T-249):(T-50), Pt_hat[(T-249):(T-50)], lwd=2, col=4)
  lines((T-50):T, Pt_hat[(T-50):T], lwd=2, lty = 2, col=2)
  abline(v=T1-50, lty=2, col=3,lwd=2)
  
  
  # (5) ARMA(0,1) + GARCH(1,1) 的1步預測
  set.seed(1)
  epsilon=rnorm(T1,0,1)
  pred1 = garchFit(~arma(0,1)+garch(1,1), data = lrt[1:(T1-50)], trace = F, cond.dist = 'sstd')
  coef = coef(pred1)
  r_hat_2 = NULL;r_hat_2 = lrt
  sigma_2 = vector(length = T1);sigma_2[1:(T1-50)]=pred1@h.t 
  at=vector(length=T1);at[1:(T1-50)]=pred1@residuals
  for (t in 2:(T1-50)){
    r_hat_2[t] = coef[1] + coef[2]*at[t-1]
  }
  for (t in (T1-49):T1){
    r_hat_2[t] = coef[1] + coef[2]*at[t-1]
    sigma_2[t] = coef[3]+coef[4]*at[t-1]^2+coef[5]*sigma_2[t-1]
    at[t] = lrt[t] - r_hat_2[t]   
  }
  # one-step ahead predition  
  plot((T1-249):T1, lrt[(T1-249):(T1)], type='l',xlab='',ylab = '',lwd=2, main='100*return')
  lines((T1-249):(T1-50), r_hat_2[(T1-249):(T1-50)], lwd=2, col=4)
  lines((T1-50):T1, r_hat_2[(T1-50):T1], lwd=2, col=2)
  abline(v=T1-50, lty=2, col=3,lwd=2)
  
  # Convert to price    
  Pt=NULL
  Pt_hat = NULL
  Pt[1]=stock[1]
  Pt_hat[1] = Pt[1]
  for (t in 2:T){
    Pt[t] = Pt[t-1]*(1+lrt[t]/100)
    Pt_hat[t] = Pt[t-1]*(1+r_hat_2[t]/100)
  }
  plot((T-249):T, Pt[(T-249):T], type='p',xlab='',ylab = '',lwd=2, main = 'Price')
  lines((T-249):(T-50), Pt_hat[(T-249):(T-50)], lwd=2, col=4)
  lines((T-50):T, Pt_hat[(T-50):T], lwd=2, lty = 2, col=2)
  abline(v=T1-50, lty=2, col=3,lwd=2)
  