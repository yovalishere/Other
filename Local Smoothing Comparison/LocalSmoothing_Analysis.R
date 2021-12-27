## 1

#################### (a) Data points generation ###############
## Generate n=101 equidistant points in [-2\pi, 2\pi]
m <- 1000
n <- 101
x <- 2*pi*seq(-1, 1, length=n)

#################### (b) Local smoothing estimate ###############

## Initialize the matrix of fitted values for three methods
fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)

##Generate data, fit the data and store the fitted values

for (j in 1:m){
  ## simulate y-values
  ##f(x)<-(1-x**2)*exp(-0.5*(x**2))
  ## Note that you need to replace $f(x)$ below by the mathematical definition in eq. (2)
  y <- (1-x**2)*exp(-0.5*(x**2)) + rnorm(length(x),mean = 0, sd = 0.2);
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x, span = 0.75), newdata = x);
  fvnw[,j] <- ksmooth(x, y, kernel="normal", bandwidth= 0.2, x.points=x)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x), x=x)$y
}

#################### (c0) Mean ########################################
meanlp = apply(fvlp,1,mean);
meannw = apply(fvnw,1,mean);
meanss = apply(fvss,1,mean);
dmin = min( meanlp, meannw, meanss);
dmax = max( meanlp, meannw, meanss);
allmax=max( meanlp, meannw, meanss,y);
allmin=min( meanlp, meannw, meanss,y);

#################### (c1) Empirical bias ########################################
biaslp=meanlp-y
biasnw=meannw-y
biasss=meanss-y
allmax_bias=max( biaslp, biasnw, biasss);
allmin_bias=min( biaslp, biasnw, biasss);

#################### (c2) Empirical variance ########################################
sq_subtract_lp=(fvlp-meanlp)**2
var_lp=apply(sq_subtract_lp,1,mean)

sq_subtract_nw=(fvnw-meannw)**2
var_nw=apply(sq_subtract_nw,1,mean)

sq_subtract_ss=(fvss-meanss)**2
var_ss=apply(sq_subtract_ss,1,mean)

allmax_var=max( var_lp, var_nw, var_ss);
allmin_var=min( var_lp, var_nw, var_ss);


#################### (c3) Empirical mean square error (MSE) ##############################
sq_subtractY_lp=(fvlp-y)**2
MSE_lp=apply(sq_subtractY_lp,1,mean)

sq_subtractY_nw=(fvnw-y)**2
MSE_nw=apply(sq_subtractY_nw,1,mean)

sq_subtractY_ss=(fvss-y)**2
MSE_ss=apply(sq_subtractY_ss,1,mean)

allmax_MSE=max( MSE_lp, MSE_nw, MSE_ss);
allmin_MSE=min( MSE_lp, MSE_nw, MSE_ss);


#################### (d) Plot  ########################################

## 1) Mean
matplot(x, meanlp, "l", ylim=c(allmin, allmax), ylab="Response")
matlines(x, meannw, col="red")
matlines(x, meanss, col="blue")
matlines(x, y, col="green")## Raw
legend("topright", legend=c("loess", "NW", "spline ", "Raw"),
       col=c("Black","red", "blue","green"), lty=1, cex=0.8)
title(main = "Mean")

## 1) Bias
matplot(x, biaslp, "l", ylim=c(allmin_bias, allmax_bias), ylab="Response")
matlines(x, biasnw, col="red")
matlines(x, biasss, col="blue")
legend("topright", legend=c("loess", "NW", "spline "),
       col=c("Black","red", "blue"), lty=1, cex=0.8)
title(main = "Bias")

## 2) Variance
matplot(x, var_lp, "l", ylim=c(allmin_var, allmax_var), ylab="Response")
matlines(x, var_nw, col="red")
matlines(x, var_ss, col="blue")
legend("topright", legend=c("loess", "NW", "spline "),
       col=c("Black","red", "blue"), lty=1, cex=0.8)
title(main = "Empirical variance")

## 2) MSE
matplot(x, MSE_lp, "l", ylim=c(allmin_MSE, allmax_MSE), ylab="Response")
matlines(x, MSE_nw, col="red")
matlines(x, MSE_ss, col="blue")
legend("topright", legend=c("loess", "NW", "spline "),
       col=c("Black","red", "blue"), lty=1, cex=0.8)
title(main = "Empirical MSE")

####################################################################################################################################
## 2

#################### (a) Data points generation ###############
## Generate n=101 equidistant points in [-2\pi, 2\pi]
m <- 1000
n <- 101
set.seed(79)
x_b <- 2*pi*sort(c(0.5, -1 + rbeta(50,2,2), rbeta(50,2,2)))
dim(x_b)<-c(101,1)
dim(x_b)<-c(1,101)

#################### (b) Local smoothing estimate ###############

## Initialize the matrix of fitted values for three methods
fvlp_b <- fvnw_b <- fvss_b <- matrix(0, nrow= n, ncol= m)

##Generate data, fit the data and store the fitted values

for (j in 1:m){
  ## simulate y-values
  ##f(x)<-(1-x**2)*exp(-0.5*(x**2))
  ## Note that you need to replace $f(x)$ below by the mathematical definition in eq. (2)
  y_b <- (1-x_b**2)*exp(-0.5*(x_b**2)) + rnorm(length(x_b),mean = 0, sd = 0.2);
  #dim(y_b)<-c(101,1);
  dim(y_b)<-c(1,101)
  ## Get the estimates and store them
  fvlp_b[,j] <- predict(loess(y_b ~ x_b, span = 0.3365), newdata = x_b);
  fvnw_b[,j] <- ksmooth(x_b, y_b, kernel="normal", bandwidth= 0.2, x.points=x_b)$y_b;
  ##fvnw_b[,j] <- ksmooth(x_b, y_b,  kernel="normal", bandwidth= 0.2, x.points=x_b)$y_b;
  fvss_b[,j] <- predict(smooth.spline(y_b ~ x_b, spar= 0.7163), x=x_b)$y_b
}
dim(x_b)<-c(101,1)
dim(y_b)<-c(101,1)
x_b
dim(x)
#################### (c0) Mean ########################################
meanlp = apply(fvlp,1,mean);
meannw = apply(fvnw,1,mean);
meanss = apply(fvss,1,mean);
dmin = min( meanlp, meannw, meanss);
dmax = max( meanlp, meannw, meanss);
allmax=max( meanlp, meannw, meanss,y);
allmin=min( meanlp, meannw, meanss,y);

#################### (c1) Empirical bias ########################################
biaslp=meanlp-y
biasnw=meannw-y
biasss=meanss-y
allmax_bias=max( biaslp, biasnw, biasss);
allmin_bias=min( biaslp, biasnw, biasss);

#################### (c2) Empirical variance ########################################
sq_subtract_lp=(fvlp-meanlp)**2
var_lp=apply(sq_subtract_lp,1,mean)

sq_subtract_nw=(fvnw-meannw)**2
var_nw=apply(sq_subtract_nw,1,mean)

sq_subtract_ss=(fvss-meanss)**2
var_ss=apply(sq_subtract_ss,1,mean)

allmax_var=max( var_lp, var_nw, var_ss);
allmin_var=min( var_lp, var_nw, var_ss);


#################### (c3) Empirical mean square error (MSE) ##############################
sq_subtractY_lp=(fvlp-y)**2
MSE_lp=apply(sq_subtractY_lp,1,mean)

sq_subtractY_nw=(fvnw-y)**2
MSE_nw=apply(sq_subtractY_nw,1,mean)

sq_subtractY_ss=(fvss-y)**2
MSE_ss=apply(sq_subtractY_ss,1,mean)

allmax_MSE=max( MSE_lp, MSE_nw, MSE_ss);
allmin_MSE=min( MSE_lp, MSE_nw, MSE_ss);


#################### (d) Plot  ########################################

## 1) Mean
matplot(x, meanlp, "l", ylim=c(allmin, allmax), ylab="Response")
matlines(x, meannw, col="red")
matlines(x, meanss, col="blue")
matlines(x, y, col="green")## Raw
legend("topright", legend=c("loess", "NW", "spline ", "Raw"),
       col=c("Black","red", "blue","green"), lty=1, cex=0.8)
title(main = "Mean")

## 1) Bias
matplot(x, biaslp, "l", ylim=c(allmin_bias, allmax_bias), ylab="Response")
matlines(x, biasnw, col="red")
matlines(x, biasss, col="blue")
legend("topright", legend=c("loess", "NW", "spline "),
       col=c("Black","red", "blue"), lty=1, cex=0.8)
title(main = "Bias")

## 2) Variance
matplot(x, var_lp, "l", ylim=c(allmin_var, allmax_var), ylab="Response")
matlines(x, var_nw, col="red")
matlines(x, var_ss, col="blue")
legend("topright", legend=c("loess", "NW", "spline "),
       col=c("Black","red", "blue"), lty=1, cex=0.8)
title(main = "Empirical variance")

## 2) MSE
matplot(x, MSE_lp, "l", ylim=c(allmin_MSE, allmax_MSE), ylab="Response")
matlines(x, MSE_nw, col="red")
matlines(x, MSE_ss, col="blue")
legend("topright", legend=c("loess", "NW", "spline "),
       col=c("Black","red", "blue"), lty=1, cex=0.8)
title(main = "Empirical MSE")