# Local Smoothing Comparison

For details, please check the report [HERE](https://github.com/yovalishere/Other/blob/main/Local%20Smoothing%20Comparison/LocalSmoothing_Report.pdf)
### Project description
It is aims to compare between 3 smoothers, namely **loess**, **Nadaraya-Watson (NW) kernel smoothing** and **spline smoothing**. 

### Data description
m = 1000 Monte Carlo runs were be used, and in each run, we simulate a data set of n = 101 observations with the Mexican hat function. 
The comparison will be made based on mean, empirical bias, empirical variance, and empirical mean square error (MSE). 
2 sets of data points based on deterministic fixed design will be generated: (1) Equidistant points (2) Non-equidistant points.

<img src="https://github.com/yovalishere/Other/blob/main/Local%20Smoothing%20Comparison/Demo_hatfunciton.jpg" width="550" height="300" />
The above figure demostrates the distribution of the means of the raw data and that of different smoothers with equidistant points. 

### Project findings.
NW kernel smoother is better in minimising bias while loess is better in minimising variance. Explained with the bias-variance trade-off, the low bias performance of
NW proves its capability in capturing the trend of the true y. Yet, it may result in overfitting the model. In contrast, the low variance in loess 
shows its potential in producing consistent prediction but maybe it will oversimplify the model. Without the conclusive finding in MSE, I don’t think it is fair to
compare them.<br><br>

<img src="https://github.com/yovalishere/Other/blob/main/Local%20Smoothing%20Comparison/Summary%20table_hat.jpg" width="520" height="250" />
