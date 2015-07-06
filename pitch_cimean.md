The CIMean Shiny App
========================================================
author: Homer White
date: July 6, 2015
transition:  rotate




Confidence Intervals
========================================================

Statistics students make confidence intervals all the time:


```r
library(tigerstats)
ttestGC(fastest~sex, data = m111survey, 
        conf.level = 0.95, verbose = FALSE)
```

```


Inferential Procedures for the Difference of Two Means mu1-mu2:
	(Welch's Approximation Used for Degrees of Freedom)
	 fastest grouped by sex 
95% Confidence Interval for mu1-mu2:

          lower.bound         upper.bound          
          -23.254640          -3.548586            
```


But ...
========================================================
type:  alert
incremental:  true

* They have trouble explaining what the "level of confidence" means.
* They also don't understand the conditions under which the actual level of confidence might differ significantly from the nominal "desired" level.

The CIMean Shiny App
========================================================

The CIMean Shiny app (click <a href = "http://homer.shinyapps.io/CIMean" target = "_blank">here</a> to view), aims to help them out by exploring t-interval for the mean of a population.  Students can:

* choose from four different populations (two of which are quite pathological)
* choose a sample size
* choose a nominal level of confidence

They can also have the app take a designated number of samples at once.

* Taking one sample at a time helps reinforce the distinction between sample and population.
* Taking many samples gives a good estimate of the actual level of confidence.

Other Features
=====================
incremental:  true

* A separate tab takes fifty samples at a time.  This helps students get a feel for situations in which the intervals are liable to miss on a particular "side" of the mean.
* A third tab compares the theoretical t-curve with the a data-based estimate of the actual distribution of the t-statistic used to construct t-intervals.  This helps students understand the effect of departures from model assumptions.
* A fourth tab reminds students of confidence interval principles and explains how to use the app.

