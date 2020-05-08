library(survival) # Attach the package survival
time<-c(17.88,28.92,33.0,41.52,42.12,45.60,48.48,51.84,51.96,54-12,55.56,67.80,
  68.64,68.64,68.88,84.12,93.12,98.64,105.12,105.84,127.92,128.04,173.4)
# Create a vector of survival times.
  
length(time)

status<-c(rep(1,23)) # Create a vector which indicates whether the
#survival time is censored or complete. 1 indicates complete and zero indicates censored
length(status)

ctrl<-data.frame(time,status);ctrl # Create a data set of the vectors time and status.
#attach(ctrl)
km.ctrl<-survfit(Surv(ctrl$time[1:23],ctrl$status[1:23])~1);km.ctrl # Create a survival object
plot(km.ctrl,conf.int=F,xlab= "time",ylab= "survival function",
     main="Non-parametric estimator of survival function", cex=.6) 
# Plot empirical survival curve.