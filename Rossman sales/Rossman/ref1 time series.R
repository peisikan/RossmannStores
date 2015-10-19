require(astsa)
setwd("D:/kps/2015 spring/8281 time series/final project")
cp=ts(scan("chickpox.txt"), start=1931, frequency=12)
# scatter plot
plot.ts(cp, ylab="", main="Monthly reported number of chickenpox, New York city, 1931-1972")

# ACF & PACF
par(mfrow=c(2,1))
acf(cp, 50)
pacf(cp,50)

# trend
train=ts(cp[1:448],start=1931,frequency=12)
test=ts(cp[449:498],start=1968+(1/3),frequency=12)
t=time(cp)
reg1=lm(cp~t)
summary(reg1)
t.train=time(train)
reg1=lm(train~t.train)
summary(reg1)
plot.ts(train, ylab="number of cases", main="" )
lines(fitted(reg1))

n=length(resid(reg1))
n
I=abs(fft(resid(reg1)))^2/448
f=0:223/448
plot(f, I[1:224], type="l", xlab="frequency", ylab="")
mtext("Periodogram: Residuals")
abline(v=0:6/12, col="gray80")

1/f[order(I[1:224])][224] #the largest one
1/f[order(I[1:224])][223] # the second largest one 
1/f[order(I[1:224])][222] # the third largest one 
1/f[order(I[1:224])][221] # the forth largest one

require(astsa)
setwd("D:/kps/2015 spring/8281 time series/final project")
cases=ts(scan("chickpox.txt"), start=1931, frequency=12)
t=time(cases)
t_1=t[1:(length(t)-1)]
t_2=t[1:(length(t)-2)]
t2=t^2
acf(cases,50)
pacf(cases,50)





fit3=lm(logcase~t)
summary(fit3)
fit4=lm(diffcase~t_1)
summary(fit4)

diffcase2=diff(diffcase)
fit5=lm(diffcase2~t_2)
summary(fit5)

fit1=lm(cases~t)
fit2=lm(fit1$residual~t2)
summary(fit2)
acf(fit1$residual)
plot(fit1$residual,lty=1,type="l")


a=sin(t*pi*2) # t=integer/12
b=cos(t*pi*2)
fit6=lm(cases~t+a+b)
summary(fit6)

###################################3
#2.1 a)
 setwd("D:/kps/2015 spring/8281 time series/chapter 1 data")
 jj= ts(scan("jj.dat"), start=1960, frequency=4)
 trend = time(jj) - 1970 # helps to ?center? time
 Q = factor(rep(1:4, 21)) # make (Q)uarter factors
 reg = lm(log(jj)~0 + trend + Q, na.action=NULL) # no intercept
 model.matrix(reg) # view the model matrix
 summary(reg) # view the results (not shown)
 
Q=factor(rep(1:12,42))
Q_1=Q[1:(length(Q)-1)]
Q_6=Q[1:(length(Q)-6)]
length(Q_6)
Q_7=Q_6[1:(length(Q_6)-1)]
a=sin(t*2*pi)
b=cos(t*2*pi)
fit7=lm(grow~t_1+Q_7, na.action=NULL)
summary(fit7)
par(mfrow=c(2,2))
plot(fit7)
plot.ts(fit7$residual)

n=length(resid(fit6))
n
I=abs(fft(resid(fit6)))^2/498
f=0:249/498
plot(f, I[1:250], type="l", xlab="frequency", ylab="")
mtext("Periodogram: Residuals")
abline(v=0:6/12, col="gray80")

1/f[order(I[1:250])][250] #the largest one
1/f[order(I[1:250])][249] # the second largest one 
1/f[order(I[1:250])][248] # the third largest one 
1/f[order(I[1:250])][247] # the forth largest one

##################################
grow=diff(log(cases))
plot.ts(grow)

a=sin(t_1*2*pi)
b=cos(t_1*2*pi)
c=sin(t_1*4*pi)
d=cos(t_1*4*pi)
e=sin(t_1*6*pi)
f=cos(t_1*6*pi)
fit8=lm(grow~t_1+a+b+c+d+e+f)
summary(fit8)
acf(resid(fit8))

n=length(resid(fit8))
n
I=abs(fft(resid(fit8)))^2/498
f=0:249/498
plot(f, I[1:250], type="l", xlab="frequency", ylab="")
mtext("Periodogram: Residuals")
abline(v=0:6/12, col="gray80")

1/f[order(I[1:250])][250] #the largest one
1/f[order(I[1:250])][249] # the second largest one 
1/f[order(I[1:250])][248] # the third largest one 
1/f[order(I[1:250])][247] # the forth largest one

######################################33
grow=diff(log(cases))
plot.ts(grow)

Q=factor(rep(1:12,38))
Q_1=Q[1:(length(Q)-1)]
Q_6=Q[1:(length(Q)-6)]
Q_7=Q_6[1:(length(Q_6)-1)]
Q_8=Q[1:(length(Q)-8)]

fit7=lm(grow~t_1+Q_7, na.action=NULL)
summary(fit7)f
par(mfrow=c(2,2))
plot(fit7)
plot(fit7$fitted)
par(mfrow=c(2,1))
acf(fit7$residual, 50)
pacf(fit7$residual,50)
a=sin(t_1*2*pi)
b=cos(t_1*2*pi)
fit8=lm(grow~t_1+a+b)
summary(fit8)

##############
aic=matrix(NA,4,4)
rownames(aic) = c("p=0",1:3)
colnames(aic) = c("q=0",1:3)

for(p in 0:3)
 for(q in 0:3)
  aic[p+1, q+1]=arima(fit7$residual, order=c(p,0,q))$aic

aic-min(aic)

aic

#arima(2,0,2)
arima22=arima(fit7$residual, order=c(2,0,2));tsdiag(arima22)

for (p in 0:3) for(q in 0:3) for(P in 0:2) for(Q in 0:3)
{
aic=arima(fit7$residual, order=c(p,0,q), seasonal=list(order=c(P,0,Q), period=12))$aic
cat(p,1,q,P,1,Q,aic,"\n")
}

fit9=arima(fit7$residual, order=c(1,0,1), seasonal=list(order=c(2,0,2), period=12)); tsdiag(fit9)

###################
aic=matrix(NA,4,4)
rownames(aic) = c("p=0",1:3)
colnames(aic) = c("q=0",1:3)

for(p in 0:3)
 for(q in 0:3)
  aic[p+1, q+1]=arima(grow, order=c(p,0,q))$aic

aic-min(aic)

aic
# arima(3,0,3)
arima33=arima(grow, order=c(3,0,3));tsdiag(arima33)

for (p in 0:3) for(q in 0:3) for(P in 0:2) for(Q in 0:3)
{
aic=arima(grow, order=c(p,0,q), seasonal=list(order=c(P,0,Q), period=12))$aic
cat(p,0,q,P,0,Q,aic,"\n")
}

#arima(1,0,1)*(2,0,2)
fit10=arima(grow, order=c(1,0,1), seasonal=list(order=c(2,0,2), period=12)); tsdiag(fit10)
acf(resid(fit10),50)
arima(oil.gr, order=c(4,0,4)); tsdiag(fit10)

############################
# Example 6.10
############################
require(astsa)
setwd("D:/kps/2015 spring/8281 time series/final project")
cp=ts(scan("chickpox.txt"), start=1931, frequency=12)
num = length(cp)
grow=diff(log(cp))
exp(mean(grow))
A = cbind(1,1,0,0,0,0,0,0,0,0,0,0)                             
# Function to Calculate Likelihood 
Linn=function(para){
 Phi = diag(0,12); Phi[1,1] =para[1] 
 Phi[2,]=c(0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1); 
 Phi[3,]=c(0,1,0,0,0,0,0,0,0,0,0,0); 
 Phi[4,]=c(0,0,1,0,0,0,0,0,0,0,0,0);
 Phi[5,]=c(0,0,0,1,0,0,0,0,0,0,0,0);
 Phi[6,]=c(0,0,0,0,1,0,0,0,0,0,0,0);
 Phi[7,]=c(0,0,0,0,0,1,0,0,0,0,0,0);
 Phi[8,]=c(0,0,0,0,0,0,1,0,0,0,0,0);
 Phi[9,]=c(0,0,0,0,0,0,0,1,0,0,0,0);
 Phi[10,]=c(0,0,0,0,0,0,0,0,1,0,0,0);
 Phi[11,]=c(0,0,0,0,0,0,0,0,0,1,0,0);
 Phi[12,]=c(0,0,0,0,0,0,0,0,0,0,1,0);
 cQ1 = para[2]; cQ2 = para[3]     # sqrt q11 and sqrt q22
 cQ=diag(0,12); cQ[1,1]=cQ1; cQ[2,2]=cQ2
 cR = para[4]       # sqrt r11
 kf = Kfilter0(num,cp,A,mu0,Sigma0,Phi,cQ,cR)
 return(kf$like)  
}

# Initial Parameters 
mu0 = c(-10,0,0,0,0,0,0,0,0,0,0,0);  Sigma0 = diag(.04,12)  
init.par = c(1.01,.1,.1,.5)   # Phi[1,1], the 2 Qs and R

# Estimation
est = optim(init.par, Linn, NULL, method="BFGS", hessian=TRUE, control=list(trace=1,REPORT=1))
SE = sqrt(diag(solve(est$hessian)))
u = cbind(estimate=est$par,SE)
rownames(u)=c("Phi11","sigw1","sigw2","sigv"); u     

est

# Smooth
Phi = diag(0,4); Phi[1,1] = est$par[1] 
Phi[2,]=c(0,-1,-1,-1); Phi[3,]=c(0,1,0,0); Phi[4,]=c(0,0,1,0)
cQ1 = est$par[2]; cQ2 = est$par[3]      
cQ = diag(1,4); cQ[1,1]=cQ1; cQ[2,2]=cQ2   
cR = est$par[4]   
ks = Ksmooth0(num,jj,A,mu0,Sigma0,Phi,cQ,cR)   

# Plot 
Tsm = ts(ks$xs[1,,], start=1960, freq=4)
Ssm = ts(ks$xs[2,,], start=1960, freq=4)
p1 = 2*sqrt(ks$Ps[1,1,]); p2 = 2*sqrt(ks$Ps[2,2,])
par(mfrow=c(3,1))
plot(Tsm, main="Trend Component", ylab="Trend")
 lines(Tsm+p1, lty=2, col=4); lines(Tsm-p1,lty=2, col=4)
plot(Ssm, main="Seasonal Component", ylim=c(-5,4), ylab="Season")
 lines(Ssm+p2,lty=2, col=4); lines(Ssm-p2,lty=2, col=4)
plot(jj, type="p", main="Data (points) and Trend+Season (line)") 
 lines(Tsm+Ssm)

# Forecast 
n.ahead=12; y = ts(append(jj, rep(0,n.ahead)), start=1960, freq=4)
rmspe = rep(0,n.ahead); x00 = ks$xf[,,num]; P00 = ks$Pf[,,num]
Q=t(cQ)%*%cQ;  R=t(cR)%*%(cR)
for (m in 1:n.ahead){ 
 xp = Phi%*%x00; Pp = Phi%*%P00%*%t(Phi)+Q
 sig = A%*%Pp%*%t(A)+R; K = Pp%*%t(A)%*%(1/sig)
 x00 = xp; P00 = Pp-K%*%A%*%Pp
 y[num+m] = A%*%xp; rmspe[m] = sqrt(sig)  
}   

dev.new() 
par(mar=c(3,3,2,2),las=1)
plot(y, type="o", main="", ylab="", ylim=c(5,30), xlim=c(1975,1984))
 upp = ts(y[(num+1):(num+n.ahead)]+2*rmspe, start=1981, freq=4)
 low = ts(y[(num+1):(num+n.ahead)]-2*rmspe, start=1981, freq=4)
lines(upp, lty=2);  lines(low, lty=2);  abline(v=1980.75, lty=3) 

############################
# Example 6.10 (dynamic seasonal)
############################
require(astsa)
setwd("D:/kps/2015 spring/8281 time series/final project")
cp=ts(scan("chickpox.txt"), start=1931, frequency=12)
num = length(cp)
grow=diff(log(cp))
exp(mean(grow))
A = cbind(1,1,0,1,0,1,0)                                  

# Function to Calculate Likelihood 
Linn=function(para){
 Phi = diag(0,7); Phi[1,1] =para[1] 
 w1=2*pi/12
 w2=0
 w3=0
 Phi[2,]=c(0,cos(w1),sin(w1),0,0,0,0); 
 Phi[3,]=c(0,-sin(w1),cos(w1),0,0,0,0); 
 Phi[4,]=c(0,0,0,cos(w2),sin(w2),0,0);
 Phi[5,]=c(0,0,0,-sin(w2),cos(w2),0,0);
 Phi[6,]=c(0,0,0,0,0,cos(w3),sin(w3));
 Phi[7,]=c(0,0,0,0,0,-sin(w3),cos(w3));
 cQ1 = para[2]; cQ2 = para[3]; cQ3=para[4];  
 cQ4=para[5];  cQ5=para[6]; cQ6=para[7]; cQ7=para[8];     # sqrt q11 and sqrt q22
 cQ=diag(0,7); cQ[1,1]=cQ1; cQ[2,2]=cQ2; cQ[3,3]=cQ3; cQ[4,4]=cQ4;
 cQ[6,6]=cQ6; cQ[7,7]=cQ7; cQ[5,5]=cQ5;
 cR = para[9]       # sqrt r11
 #browser()
 kf = Kfilter0(num,cp,A,mu0,Sigma0,Phi,cQ,cR)
 return(kf$like)  
}

# Initial Parameters 
mu0 = c(-1,0.5,0.5,0.5,0.5,0.5,0.5);  Sigma0 = diag(.04,7)  
init.par = c(1.01,.1,.1,.5,0.5,0.5,0.5,0.5,0.5)   # Phi[1,1], the 2 Qs and R

# Estimation
est = optim(init.par, Linn, NULL, method="BFGS", hessian=TRUE, control=list(trace=1,REPORT=1))
SE = sqrt(diag(solve(est$hessian)))
u = cbind(estimate=est$par,SE)
rownames(u)=c("Phi11","sigw1","sigw2","sigv"); u     

est

# Smooth
Phi = diag(0,4); Phi[1,1] = est$par[1] 
Phi[2,]=c(0,-1,-1,-1); Phi[3,]=c(0,1,0,0); Phi[4,]=c(0,0,1,0)
cQ1 = est$par[2]; cQ2 = est$par[3]      
cQ = diag(1,4); cQ[1,1]=cQ1; cQ[2,2]=cQ2   
cR = est$par[4]   
ks = Ksmooth0(num,jj,A,mu0,Sigma0,Phi,cQ,cR)   

# Plot 
Tsm = ts(ks$xs[1,,], start=1960, freq=4)
Ssm = ts(ks$xs[2,,], start=1960, freq=4)
p1 = 2*sqrt(ks$Ps[1,1,]); p2 = 2*sqrt(ks$Ps[2,2,])
par(mfrow=c(3,1))
plot(Tsm, main="Trend Component", ylab="Trend")
 lines(Tsm+p1, lty=2, col=4); lines(Tsm-p1,lty=2, col=4)
plot(Ssm, main="Seasonal Component", ylim=c(-5,4), ylab="Season")
 lines(Ssm+p2,lty=2, col=4); lines(Ssm-p2,lty=2, col=4)
plot(jj, type="p", main="Data (points) and Trend+Season (line)") 
 lines(Tsm+Ssm)

# Forecast 
n.ahead=12; y = ts(append(jj, rep(0,n.ahead)), start=1960, freq=4)
rmspe = rep(0,n.ahead); x00 = ks$xf[,,num]; P00 = ks$Pf[,,num]
Q=t(cQ)%*%cQ;  R=t(cR)%*%(cR)
for (m in 1:n.ahead){ 
 xp = Phi%*%x00; Pp = Phi%*%P00%*%t(Phi)+Q
 sig = A%*%Pp%*%t(A)+R; K = Pp%*%t(A)%*%(1/sig)
 x00 = xp; P00 = Pp-K%*%A%*%Pp
 y[num+m] = A%*%xp; rmspe[m] = sqrt(sig)  
}   

dev.new() 
par(mar=c(3,3,2,2),las=1)
plot(y, type="o", main="", ylab="", ylim=c(5,30), xlim=c(1975,1984))
 upp = ts(y[(num+1):(num+n.ahead)]+2*rmspe, start=1981, freq=4)
 low = ts(y[(num+1):(num+n.ahead)]-2*rmspe, start=1981, freq=4)
lines(upp, lty=2);  lines(low, lty=2);  abline(v=1980.75, lty=3) 






Time = 1:num; par(mfrow=c(3,1))

plot(Time, mu[-1], main="Prediction", ylim=c(-5,10))      
lines(ks$xp)
lines(ks$xp+2*sqrt(ks$Pp), lty="dashed", col="blue")
lines(ks$xp-2*sqrt(ks$Pp), lty="dashed", col="blue")

plot(Time, mu[-1], main="Filter", ylim=c(-5,10))
lines(ks$xf)
lines(ks$xf+2*sqrt(ks$Pf), lty="dashed", col="blue")
lines(ks$xf-2*sqrt(ks$Pf), lty="dashed", col="blue")

plot(Time, mu[-1],  main="Smoother", ylim=c(-5,10))
lines(ks$xs)
lines(ks$xs+2*sqrt(ks$Ps), lty="dashed", col="blue")
lines(ks$xs-2*sqrt(ks$Ps), lty="dashed", col="blue") 








